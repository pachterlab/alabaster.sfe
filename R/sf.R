#' Save \code{sf} to disk for \code{alabaster}
#' 
#' \code{sf} data frames are saved as GeoParquet.
#' 
#' @inheritParams alabaster.base::saveObject
#' @return x is saved into \code{path} and \code{NULL} is invisibly returned.
#' @importFrom sfarrow st_write_parquet
#' @export
#' @examples
#' library(sf)
#' fp <- tempfile()
#' df <- as.data.frame(matrix(rnorm(10), ncol = 2))
#' df <- st_as_sf(df, coords = names(df), crs = NA)
#' saveObject(df, path = fp)
#' 
setMethod("saveObject", "sf", function(x, path) {
    dir.create(path)
    saveObjectFile(path, "simple_feature", 
                   extra = list(simple_feature = list(version = "1.0", dimensions = dim(x))))
    # Deal with row names
    rns <- rownames(x)
    rownames(x) <- NULL
    x$rownames <- rns
    suppressWarnings(st_write_parquet(x, file.path(path, "map.parquet")))
    # deal with featureData, specifically for SFE, doesn't affect saving sf outside SFE
    fd <- attr(x, "featureData")
    if (!is.null(fd)) altSaveObject(fd, file.path(path, "feature_data"))
    params <- attr(x, "params")
    if (!is.null(params)) altSaveObject(params, file.path(path, "params"))
    invisible(NULL)
})

#' Read \code{sf} from \code{alabaster} on disk representation
#' 
#' Read the GeoParquet file into R. GeoParquet should also work in any
#' programming language that supports \code{arrow}. Newer version of GDAL with
#' Parquet driver is not strictly necessary if the geometry's WKB can be 
#' converted to whichever language specific object such as \code{sfc} in R.
#' 
#' @inheritParams alabaster.base::readObject
#' @return An \code{sf} data frame
#' @importFrom sfarrow st_read_parquet
#' @export
#' @examples
#' library(sf)
#' fp <- tempfile()
#' df <- as.data.frame(matrix(rnorm(10), ncol = 2))
#' df <- st_as_sf(df, coords = names(df), crs = NA)
#' saveObject(df, path = fp)
#' df2 <- readObject(fp)
#' 
readSF <- function(path, metadata = NULL) {
    df <- st_read_parquet(file.path(path, "map.parquet"))
    if ("rownames" %in% names(df)) {
        rownames(df) <- df$rownames
        df$rownames <- NULL
    }
    fd_path <- file.path(path, "feature_data")
    if (dir.exists(fd_path)) {
        fd <- altReadObject(fd_path)
        attr(df, "featureData") <- fd
    }
    param_path <- file.path(path, "params")
    if (dir.exists(param_path)) {
        params <- altReadObject(param_path)
        attr(df, "params") <- params
    }
    return(df)
}

validateSF <- function(path, metadata = NULL) {
    stopifnot(file.exists(file.path(path, "map.parquet")))
    fd_path <- file.path(path, "feature_data")
    if (dir.exists(fd_path)) {
        validateObject(fd_path)
    }
    param_path <- file.path(path, "params")
    if (dir.exists(param_path))
        validateObject(param_path)
}
