#' Save \code{sf} to disk for \code{alabaster}
#' 
#' \code{sf} data frames are saved as GeoParquet.
#' 
#' @inheritParams alabaster.base::saveObject
#' @importFrom sfarrow st_write_parquet
#' @export
#' @examples
#' # example code
#' 
setMethod("saveObject", "sf", function(x, path) {
    dir.create(path)
    saveObjectFile(path, "simple_feature", extra = list(version = "1.0"))
    suppressWarnings(st_write_parquet(x, file.path(path, "map.parquet")))
    # deal with featureData, specifically for SFE, doesn't affect saving sf outside SFE
    fd <- attr(x, "featureData")
    if (!is.null(fd)) altSaveObject(fd, file.path(path, "feature_data"))
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
#' @importFrom sfarrow st_read_parquet
#' @export
#' @examples
#' # example code
#' 
readSF <- function(path, metadata = NULL) {
    df <- st_read_parquet(file.path(path, "map.parquet"))
    fd_path <- file.path(path, "feature_data")
    if (dir.exists(fd_path)) {
        fd <- altReadObject(fd_path)
        attr(df, "featureData") <- fd
    }
    return(df)
}

validateSF <- function(path, metadata = NULL) {
    stopifnot(file.exists(file.path("map.parquet")))
    fd_path <- file.path(path, "feature_data")
    if (dir.exists(fd_path)) {
        validateObject(fd_path)
    }
}
