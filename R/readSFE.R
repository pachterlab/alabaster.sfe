#' Read SFE object from on disk representation
#'
#' @inheritParams alabaster.base::readObject
#'
#' @export
#' @examples
#' # example code
#'
readSpatialFeatureExperiment <- function(path, metadata = NULL, ...) {
    if (is.null(metadata))
        metadata <- readObjectFile(path)
    metadata$type <- "spatial_experiment"
    spe <- altReadObject(path, metadata, ...)

    # The geometries
    # the files are named after the indices, but remember the 1, 10, 11..., 2 thing
    # spatialGraphs

    # localResults
}
