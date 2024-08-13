#' Read \code{SpatRaster} from \code{alabaster} on disk representation
#'
#' Not the same as \code{terra::rast}; here this function also reads the
#' metadata file.
#'
#' @inheritParams alabaster.base::readObject
#' @param ... Arguments passed to \code{\link{terra::rast}}, character method.
#' @family readObject-SFE-image
#' @return A \code{\link{SpatRasterImage}} object for SFE.
#' @importFrom terra rast
#' @export
#' @examples
#' # example code
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
#' @param ... Ignored
#' @family readObject-SFE-image
#' @return A \code{\link{BioFormatsImage}} object for SFE.
#' @export
#' @examples
#' # example code
#'
readBioFormatsImage <- function(path, metadata = NULL, ...) {
    metadata <- read_json(file.path(path, "OBJECT"), simplifyVector = TRUE)
    fn <- list.files(path, pattern = "^image\\.")
    BioFormatsImage(file.path(path, fn), ext = metadata$extent,
                    isFull = metadata$isFull, origin = metadata$origin,
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
#' # example code
#'
readExtImage <- function(path, metadata = NULL, ...) {
    metadata <- read_json(file.path(path, "OBJECT"), simplifyVector = TRUE)
    img <- readImage(file.path(path, "image.tiff"), ...)
    ExtImage(img, ext = metadata$extent)
}
