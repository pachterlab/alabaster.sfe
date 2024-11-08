#' Read \code{SpatRaster} from \code{alabaster} on disk representation
#'
#' Not the same as \code{terra::rast}; here this function also reads the
#' metadata file.
#'
#' @inheritParams alabaster.base::readObject
#' @param ... Arguments passed to \code{\link[terra]{rast}}, character method.
#' @family readObject-SFE-image
#' @return A \code{\link{SpatRasterImage}} object for SFE.
#' @importFrom terra rast
#' @importFrom alabaster.spatial loadSpatialImage
#' @export
#' @examples
#' library(SFEData)
#' fp <- tempfile()
#' fn <- file.path(fp, "vizgen")
#' d <- VizgenOutput(dataset = "cellpose", file_path = fn)
#' suppressWarnings(sfe <- readVizgen(d))
#' img <- getImg(sfe)
#' class(img)
#' fsave <- file.path(fp, "img")
#' saveObject(img, fsave)
#' img2 <- readObject(fsave)
#' 
readSpatRaster <- function(path, metadata = NULL, ...) {
    fn <- list.files(path, pattern = "^image\\.")
    # Validation function should check that there's only one file matching the pattern
    SpatRasterImage(rast(file.path(path, fn), ...))
}

#' Read \code{BioFormatsImage} from \code{alabaster} on disk representation
#'
#' Reads the metadata and \code{imgSource} will point to the file within the
#' on disk representation of SFE. The image itself should not be moved or the
#' \code{BioFormatsImabe} object will no longer work.
#'
#' @inheritParams alabaster.base::readObject
#' @param ... Ignored, but used for other methods.
#' @family readObject-SFE-image
#' @return A \code{\link{BioFormatsImage}} object for SFE.
#' @export
#' @examples
#' library(SFEData)
#' fp <- tempfile()
#' x1 <- XeniumOutput(dataset = "v1", file_path = file.path(fp, "xenium1"))
#' x2 <- XeniumOutput("v2", file_path = file.path(fp, "xenium2"))
#' 
#' # Single file OME-TIFF
#' fsave <- file.path(fp, "bfi1")
#' sfe <- readXenium(x1)
#' bfi <- getImg(sfe)
#' bfi <- affineImg(bfi, M = matrix(c(cos(pi/6), sin(pi/6), -sin(pi/6), cos(pi/6)), nrow = 2), 
#'                  v = c(0,0))
#' saveObject(bfi, fsave)
#' bfi2 <- readObject(fsave)
#' 
#' unlink(fsave, recursive = TRUE)
#' 
#' # Multi file OME-TIFF
#' fsave <- file.path(fp, "bfi2")
#' sfe <- readXenium(x2)
#' bfi <- getImg(sfe)
#' saveObject(bfi, fsave)
#' bfi2 <- readObject(fsave)
#' unlink(fsave, recursive = TRUE)
readBioFormatsImage <- function(path, metadata = NULL, ...) {
    metadata <- read_json(file.path(path, "OBJECT"), simplifyVector = TRUE)$bioformats_image
    if (dir.exists(file.path(path, "image"))) {
        fn <- list.files(file.path(path, "image"), full.names = TRUE)[1]
    } else {
        fn <- list.files(path, pattern = "^image\\.", full.names = TRUE)
    }
    if ("v" %in% names(metadata$transformation)) {
        metadata$transformation$v <- as.vector(metadata$transformation$v)
    }
    # Extent here is before any transformation
    BioFormatsImage(fn, ext = unlist(metadata$extent),
                    isFull = metadata$is_full, origin = metadata$origin,
                    transformation = metadata$transformation)
}

#' Read \code{ExtImage} from disk
#'
#' @inheritParams alabaster.base::readObject
#' @param ... Arguments passed to \code{\link{readImage}}.
#' @family readObject-SFE-image
#' @return A \code{\link{ExtImage}} object for SFE.
#' @importFrom EBImage readImage
#' @export
#' @examples
#' library(SFEData)
#' fp <- tempfile()
#' fsave <- file.path(fp, "exi")
#' x1 <- XeniumOutput(dataset = "v1", file_path = file.path(fp, "xenium1"))
#' sfe <- readXenium(x1)
#' bfi <- getImg(sfe)
#' exi <- toExtImage(bfi)
#' saveObject(exi, fsave)
#' exi2 <- readObject(fsave)
#' unlink(fsave, recursive = TRUE)
#'
readExtImage <- function(path, metadata = NULL, ...) {
    metadata <- read_json(file.path(path, "OBJECT"), simplifyVector = TRUE)$ext_image
    img <- readImage(file.path(path, "image.tiff"), as.is = TRUE, ...)
    ExtImage(img, ext = unlist(metadata$extent))
}
