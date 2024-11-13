# Validate the images
validateGeoTIFF <- function(path, metadata = NULL) {
    fn <- list.files(path, pattern = "^image\\.")
    f <- file.path(path, fn)
    stopifnot(length(f) == 1L)
    stopifnot(file.exists(f))
}

validateBFI <- function(path, metadata = NULL) {
    fn <- list.files(path, "(image.*)|(image$)")
    if (!length(fn)) stop("Image file/directory not found")
}

validateExtImage <- function(path, metadata = NULL) {
    stopifnot(file.exists(file.path(path, "image.tiff")))
}
