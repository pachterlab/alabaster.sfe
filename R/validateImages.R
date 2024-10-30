# Validate the images
validateGeoTIFF <- function(path, metadata = NULL) {
    stopifnot(file.exists(file.path(path, "image.tiff")))
    w <- tryCatch(rast(f), warning = function(w) w)
    if (is(w, "warning")) stop("image.tiff must be GeoTIFF")
}

validateBFI <- function(path, metadata = NULL) {
    fn <- list.files(path, "(image.*)|(image$)")
    if (!length(fn)) stop("Image file/directory not found")
}

validateExtImage <- function(path, metadata = NULL) {
    stopifnot(file.exists(file.path(path, "image.tiff")))
}
