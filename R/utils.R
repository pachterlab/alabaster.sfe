# Modify tools::file_ext to allow *.ome.tiff, basically the last 2 things
.file_ext <- function(x) {
    pos <- regexpr("\\.([[:alnum:]\\.]+)$", x)
    ifelse(pos > -1L, substring(x, pos), "")
}

#' @importFrom xml2 read_xml xml_attr xml_ns_strip xml_find_all
#' @importFrom RBioFormats read.omexml
.get_ome_fns <- function(fn1) {
    # This is for OME-TIFF, might not work for other BioFormats
    fn1 <- normalizePath(fn1, mustWork = TRUE)
    xml <- read.omexml(fn1) |> read_xml()
    uuid <- xml_ns_strip(xml) |> xml_find_all("//UUID")
    fns <- xml_attr(uuid, "FileName")
    fns <- fns[!is.na(fns)]
    if (!length(fns)) return(fn1)
    fns <- file.path(dirname(fn1), fns)
    fns
}

.get_object_dims <- function(path) {
    meta <- readObjectFile(path)
    type <- meta$type
    meta[[type]]$dimensions
}
