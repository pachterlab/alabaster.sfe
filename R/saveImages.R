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
#' @return x is saved into \code{path} and \code{NULL} is invisibly returned.
#' @importFrom terra rast writeRaster inMemory
#' @family saveObject-SFE-image
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
setMethod("saveObject", "SpatRaster", function(x, path, ...) {
    dir.create(path)
    extra <- list(geotiff = list(version = "1.0",
                                 extent = as.list(ext(x))))
    extra$type <- "geotiff"
    # Not using saveObjectFile because I don't want to round off digits
    write_json(extra, path = file.path(path, "OBJECT"), 
               auto_unbox = TRUE, digits = NA)
    f <- imgSource(x)
    if (is.na(f)) {
        is_geotiff <- FALSE
    } else {
        w <- tryCatch(rast(f), warning = function(w) w) # the extent warning
        is_geotiff <- !is(w, "warning")
    }
    # What about large files not in memory but don't have extent?
    # That what the !inMemory(x) is for. Resaving with writeRaster is slow
    if (is_geotiff || !inMemory(x)) {
        ex <- .file_ext(f)
        file.copy(f, file.path(path, paste0("image", ex)))
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
#' @return x is saved into \code{path} and \code{NULL} is invisibly returned.
#' @export
#' @family saveObject-SFE-image
#' @examples
#' library(SFEData)
#' fp <- tempfile()
#' fsave <- file.path(fp, "bfi2")
#' x1 <- XeniumOutput(dataset = "v1", file_path = file.path(fp, "xenium1"))
#' sfe <- readXenium(x1)
#' bfi <- getImg(sfe)
#' saveObject(bfi, fsave)
#' bfi2 <- readObject(fsave)
#' unlink(fsave, recursive = TRUE)
setMethod("saveObject", "BioFormatsImage", function(x, path, ...) {
    dir.create(path)
    extra <- list(bioformats_image = list(version = "1.0",
                                          is_full = isFull(x),
                                          extent = as.list(.ext_(x)),
                                          origin = origin(x),
                                          transformation = transformation(x)))
    extra$type <- "bioformats_image"
    # Not using saveObjectFile because I don't want to round off digits in the
    # affine transformation matrix
    write_json(extra, path = file.path(path, "OBJECT"), 
               auto_unbox = TRUE, digits = NA)
    f <- imgSource(x)
    # Deal with multi-file OME-TIFF, where imgSource points to the first file
    # Can't change file name in order not to mess with the XML metadata;
    # I'll put those in a directory.
    fns <- .get_ome_fns(f)
    if (length(fns) > 1L) {
        new_dir <- file.path(path, "image")
        dir.create(new_dir)
        file.copy(fns, new_dir)
    } else {
        ex <- .file_ext(f)
        file.copy(f, file.path(path, paste0("image", ex)))
    }
})

#' Save \code{ExtImage} to disk for \code{alabaster}
#'
#' @inheritParams alabaster.base::saveObject
#' @param ... Extra arguments passed to \code{\link{writeImage}}.
#' @return x is saved into \code{path} and \code{NULL} is invisibly returned.
#' @importFrom EBImage writeImage normalize
#' @family saveObject-SFE-image
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
setMethod("saveObject", "ExtImage", function(x, path, ...) {
    dir.create(path)
    extra <- list(ext_image = list(version = "1.0",
                                   extent = as.list(ext(x))))
    extra$type <- "ext_image"
    # Not using saveObjectFile because I don't want to round off digits
    write_json(extra, path = file.path(path, "OBJECT"), 
               auto_unbox = TRUE, digits = NA)
    # I really hate the normalize values to [0,1] thing in the EBImage and tiff packages
    # Just do what terra and Python do
    is16 <- max(x) > 255
    writeImage(normalize(x, inputRange = c(0, if (is16) 65535L else 255L)), 
               file.path(path, "image.tiff"), 
               bits.per.sample = if (is16) 16L else 8L)
})
