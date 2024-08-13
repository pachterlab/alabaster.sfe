#' @importFrom jsonlite write_json read_json
#' @importFrom sfarrow st_write_parquet
.saveGeometries <- function(gs, path, type = c("col", "row", "annot")) {
    # path would be sfe_path/colgeometries
    # How alabaster.sce saves reducedDim:
    # Each matrix is in its own directory with array.h5 and OBJECT metadata file
    # The directories are 0, 1, and so on, an index
    # There's a names.json file with a character vector of the names of the reducedDims
    if (length(gs)) {
        type <- match.arg(type)
        dir_use <- file.path(path, paste0(type, "geometries"))
        dir.create(dir_use)
        write_json(names(type), path = file.path(dir_use, "names.json"))
        # I think when I get sdf to CRAN I should write a saveObject method for it
        for (i in seq_along(gs)) {
            d <- file.path(dir_use, paste0(i-1L, ".parquet"))
            # This won't save row names, but the order should be same as in the matrix
            suppressWarnings(st_write_parquet(gs[[i]], d))
        }
    }
}

.saveLocalResults <- function(lrs, path) {
    # One directory for each type, and inside that one for each feature
    # Need to deal with illegal names at some point
    if (length(lrs)) {
        dir_use <- file.path(path, "local_results")
        write_json(names(lrs), path = file.path(dir_use, "names.json"))
        for (i in seq_along(lrs)) {
            # Types of local results, e.g. localmoran, LOSH
            d <- file.path(dir_use, i-1L)
            dir.create(d)
            write_json(names(lrs[[i]]), path = file.path(d, "names.json"))
            for (j in seq_along(lrs[[i]])) {
                # Features, e.g. genes and pairs of genes
                dd <- file.path(d, j-1L)
                altSaveObject(lrs[[i]][[j]], path = dd)
            }
        }
    }
}

.saveGraphs <- function(sfe, path) {
    # Also need to separate by sample_id
    # Because of sample_id, I'll save all graphs in this function, which is
    # different from .saveGeometries
    if (length(spatialGraphs(sfe))) {
        samples <- sampleIDs(sfe)
        ms <- c("row", "col", "annot")
        for (m in 1:3) { # margin
            type <- ms[m]
            d <- file.path(path, paste0(type, "graphs"))
            dir.create(d)
            samples_use <- list()
            sample_ind <- 0L
            for (i in seq_along(samples)) {
                gs <- spatialGraphs(sfe, MARGIN = m, sample_id = samples[[i]])
                if (length(gs)) {
                    dd <- file.path(d, sample_ind)
                    dir.create(dd)
                    samples_use <- c(samples_use, samples[[i]])
                    write_json(names(gs), path = file.path(dd, "names.json"))
                    sample_ind <- sample_ind + 1L
                }
                for (j in seq_along(gs)) {
                    ddd <- file.path(dd, j)
                    mat <- listw2sparse(gs[[j]])
                    # Calls the sparse matrix method from alabaster.base
                    altSaveObject(mat, path = ddd)
                }
            }
            samples_use <- unlist(samples_use)
            write_json(samples_use, path = file.path(d, "names.json"))
        }
    }
}

#' Save a SpatialFeatureExperiment object
#'
#' Save SFE objects to disk in an interoperable, language agnostic format that
#' may also facilitate out of memory operations via HDF5 (non-spatial, inherited
#' from \code{alabaster.sce}) and Apache Parquet (geometries).
#'
#' There's no new arguments for \code{...} for the SFE,
#' \code{SpatialExperiment}, and \code{SingleCellExperiment} methods, but there
#' is an argument that can be specified for \code{...} in the
#' \code{SummarizedExperiment} method of \code{saveObject}.
#'
#' At present, spatial results in \code{featureData} for geometries and
#' dimension reductions (see \code{\link{colFeatureData}}) and parameters of
#' spatial analyses (see \code{\link{getParams}}) are not save because those
#' parts of the SFE object are more experimental and are subject to change.
#' \code{colFeatureData} where global spatial results are stored for columns of
#' \code{colData(x)} is saved by the \code{alabaster} \code{saveObject} method
#' for \code{DataFrame}.
#'
#' @import SpatialFeatureExperiment alabaster.base methods
#' @importMethodsFrom alabaster.sce saveObject
#' @inheritParams alabaster.base::saveObject
#' @export
setMethod("saveObject", "SpatialFeatureExperiment",
          function(x, path, ...) {
              # Must use the more-images branch of alabaster.spatial
              # https://github.com/ArtifactDB/alabaster.spatial/pull/2/files
              base <- as(x, "SpatialExperiment")
              altSaveObject(base, path, ...)

              # Deal with geometries, will deal with sdf later
              .saveGeometries(colGeometries(x), path, "col")
              .saveGeometries(rowGeometries(x), path, "row")
              .saveGeometries(annotGeometries(x), path, "annot")

              # Deal with LocalResults
              .saveLocalResults(localResults(x), path)

              # Deal with spatialGraphs
              # I think I'll save the sparse adjacency matrices
              # NetworkX and igraph also support sparse matrices. dolomite will deal with the Python side
              .saveGraphs(x, path)

              # Deal with things like colFeatureData later
              # These are DataFrames and can have list columns

              # Images should've been taken care of by SPE method and my new
              # saveObject methods for new imageclasses from SFE

              meta <- readObjectFile(path)
              meta$spatial_feature_experiment <- list(version="1.0")
              saveObjectFile(path, "spatial_feature_experiment", meta)

              invisible(NULL)
          })
