#' Save \code{SpatRaster} to disk for \code{alabaster}
#'
#' Intended for \code{SpatRasterImage} which really is \code{SpatRaster} that
#' also inherits from SPE's \code{SpatialImage}. Besides \code{writeRaster},
#' this function also writes a metadata file in the \code{alabaster} framework.
#' If the image is not loaded into memory and the original file is already
#' spatially registered, e.g. it has a spatial extent, then the original file
#' is copied to a pre-defined place in the on-disk representation of SFE and
#' \code{writeRaster} is not called.
#'
#' @inheritParams alabaster.base::saveObject
#' @param ... Extra parameters passed to \code{\link{writeRaster}}.
#' @importFrom terra rast writeRaster
#' @importFrom tools file_ext
#' @family saveObject-SFE-image
#' @export
#' @examples
#' # example code
#'
setMethod("saveObject", "SpatRaster", function(x, path, ...) {
    dir.create(path)
    saveObjectFile(path, "spatraster", extra = list(version = "1.0"))
    f <- imgSource(x)
    if (is.na(f)) {
        is_geotiff <- FALSE
    } else {
        w <- tryCatch(rast(f), warning = function(w) w) # the extent warning
        is_geotiff <- !is(w, "warning")
    }
    if (is_geotiff) {
        ex <- file_ext(f)
        file.copy(f, file.path(path, "image", ex))
        # Still debating whether to keep original format or always use tiff
        # Since what if a special driver is required?
    } else {
        writeRaster(x, file.path(path, "image.tiff"), ...)
    }
})

#' Save \code{BioFormatsImage} for \code{alabaster}
#'
#' This function copies the original file to a pre-defined location within the
#' directory that stores the on disk representation of the SFE object for data
#' sharing. Since \code{BioFormatsImage} is essentially just some metadata in
#' memory and it never loads the image into memory (once the image is loaded
#' into memory it becomes \code{ExtImage}), once the original image is moved,
#' the \code{BioFormatsImage} object will no longer work, which is why the
#' pre-defined location is important. This function also saves the metadata,
#' which includes spatial extent and affine transformations.
#'
#' @inheritParams alabaster.base::saveObject
#' @param ... Ignored
#' @export
#' @family saveObject-SFE-image
#' @examples
#'
setMethod("saveObject", "BioFormatsImage", function(x, path, ...) {
    dir.create(path)
    saveObjectFile(path, "bioformats_image",
                   extra = list(version = "1.0",
                                is_full = isFull(x),
                                extent = ext(x),
                                origin = origin(x),
                                transformation = transformation(x)))
    f <- imgSource(x)
    ex <- file_ext(f)
    file.copy(f, file.path(path, "image", ex))
})

#' Save \code{ExtImage} to disk for \code{alabaster}
#'
#' @inheritParams alabaster.base::saveObject
#' @param ... Extra arguments passed to \code{\link{writeImage}}.
#' @importFrom EBImage writeImage
#' @family saveObject-SFE-image
#' @export
#' @examples
#' # example code
#'
setMethod("saveObject", "ExtImage", function(x, path, ...) {
    dir.create(path)
    saveObjectFile(path, "ext_image",
                   extra = list(version = "1.0",
                                extent = ext(x)))
    writeImage(x, file.path(path, "image.tiff"))
})
