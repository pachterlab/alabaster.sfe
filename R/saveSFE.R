#' @importFrom jsonlite write_json read_json
.saveGeometries <- function(gs, path, type = c("col", "row", "annot")) {
    # path would be sfe_path/colgeometries
    # How alabaster.sce saves reducedDim:
    # Each matrix is in its own directory with array.h5 and OBJECT metadata file
    # The directories are 0, 1, and so on, an index
    # There's a names.json file with a character vector of the names of the reducedDims
    if (!length(gs)) return(NULL)
    type <- match.arg(type)
    tg <- paste0(type, "geometries")
    dir_use <- file.path(path, tg)
    message(">>> Saving ", tg)
    dir.create(dir_use)
    write_json(names(gs), path = file.path(dir_use, "names.json"))
    # I think when I get sdf to CRAN I should write a saveObject method for it
    for (i in seq_along(gs)) {
        d <- file.path(dir_use, i-1L)
        altSaveObject(gs[[i]], d)
    }
}

.saveLocalResults <- function(lrs, path) {
    # One directory for each type, and inside that one for each feature
    # Need to deal with illegal names at some point
    if (!length(lrs)) return(NULL)
    message(">>> Saving localResults")
    dir_use <- file.path(path, "local_results")
    altSaveObject(lrs, dir_use)
}

.saveGraphs <- function(sfe, path) {
    # Also need to separate by sample_id
    # Because of sample_id, I'll save all graphs in this function, which is
    # different from .saveGeometries
    if (is.null(unlist(spatialGraphs(sfe)))) return(NULL)
    message(">>> Saving spatial graphs")
    samples <- sampleIDs(sfe)
    ms <- c("row", "col", "annot")
    # Top level should be samples, and then margin, then individual graphs
    d <- file.path(path, "spatial_graphs")
    dir.create(d)
    write_json(samples, path = file.path(d, "names.json"))
    all_graphs <- spatialGraphs(sfe)
    for (i in seq_along(samples)) {
        dd <- file.path(d, i-1L)
        dir.create(dd)
        for (m in ms) {
            gs <- all_graphs[[samples[i]]][[m]]
            if (length(gs)) {
                ddd <- file.path(dd, m)
                dir.create(ddd)
                write_json(names(gs), path = file.path(ddd, "names.json"))
                for (j in seq_along(gs)) {
                    dddd <- file.path(ddd, j-1L)
                    mat <- listw2sparse(gs[[j]])
                    # Calls the sparse matrix method from alabaster.base
                    altSaveObject(mat, path = dddd)
                    # Save the methods metadata
                    altSaveObject(attr(gs[[j]], "method"), file.path(dddd, "method"))
                }
            }
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
#' @importFrom SingleCellExperiment reducedDimNames int_colData int_colData<-
#' @importMethodsFrom alabaster.sce saveObject
#' @importFrom S4Vectors metadata metadata<-
#' @importFrom SummarizedExperiment colData colData<-
#' @inheritParams alabaster.base::saveObject
#' @export
setMethod("saveObject", "SpatialFeatureExperiment",
          function(x, path, ...) {
              # Must use the more-images branch of alabaster.spatial
              # https://github.com/ArtifactDB/alabaster.spatial/pull/2/files
              message(">>> Saving SpatialExperiment")
              base <- as(x, "SpatialExperiment")
              altSaveObject(base, path, ...)
              
              # Deal with geometries, will deal with sdf later
              .saveGeometries(colGeometries(x), path, "col")
              .saveGeometries(rowGeometries(x), path, "row")
              .saveGeometries(annotGeometries(x), path, "annot")

              # Deal with LocalResults
              .saveLocalResults(int_colData(x)$localResults, path)

              # Deal with spatialGraphs
              # I think I'll save the sparse adjacency matrices
              # NetworkX and igraph also support sparse matrices. dolomite will deal with the Python side
              .saveGraphs(x, path)
              
              # featureData; colFeatureData taken care of as mcol, geometryFeatureData taken care of in sf's saveObject method
              # Only need to deal with reducedDimFeatureData
              for (r in seq_along(reducedDimNames(x))) {
                  # Save all attrs that are not dims and dimnames
                  # including variance explained, rotation, featureData, and params
                  fp <- file.path(path, "reduced_dimensions", r-1L)
                  l <- attributes(reducedDim(x, r))
                  # Here assuming that reducedDims are dense base R matrices
                  l <- l[!names(l) %in% c("dim", "dimnames")]
                  altSaveObject(l, file.path(fp, "attrs"))
              }
              
              # Images should've been taken care of by SPE method and my new
              # saveObject methods for new imageclasses from SFE

              meta <- readObjectFile(path)
              meta$spatial_feature_experiment <- list(version=as.character(SFEVersion(x)),
                                                      unit = SpatialFeatureExperiment::unit(x))
              saveObjectFile(path, "spatial_feature_experiment", meta)

              invisible(NULL)
          })
