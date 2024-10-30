# Modify tools::file_ext to allow *.ome.tiff, basically the last 2 things
.file_ext <- function(x) {
    pos <- regexpr("\\.([[:alnum:]\\.]+)$", x)
    ifelse(pos > -1L, substring(x, pos), "")
}
# Find the other files in a multi-file OME-TIFF given the file name of the first file
# Or shall I modify BFI so imgSource points to the directory for multi-file OME-TIFF?
# But for backward compatibility, I'll still write this function
#' @importFrom xml2 read_xml xml_child xml_attrs xml_children xml_name
#' @importFrom RBioFormats read.omexml
.get_ome_fns <- function(fn1) {
    # This is for OME-TIFF, might not work for other BioFormats
    fn1 <- normalizePath(fns, mustWork = TRUE)
    xml <- read.omexml(fn1) |> read_xml()
    file_info <- xml_child(xml, 3) |> xml_child(2)
    nms <- vapply(xml_children(file_info), xml_name, FUN.VALUE = character(1))
    inds <- which(nms == "TiffData")
    if (!length(inds)) return(NULL)
    fns <- vapply(inds, function(i) {
        xml_child(file_info, i) |> xml_child(1) |> xml_attr("FileName")
    }, FUN.VALUE = character(1))
    fns <- fns[!is.na(fns)]
    fns <- file.path(dirname(fn1), fns)
    fns
}
