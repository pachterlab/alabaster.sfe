.readGeometries <- function(path, type = c("col", "row", "annot")) {
    fp <- file.path(path, paste0(type, "geometries"))
    if (!dir.exists(fp)) return(NULL)
    sfnames <- read_json(file.path(fp, "names.json"), simplifyVector = TRUE)
    gs <- lapply(seq_along(sfnames), function(i) {
        fp2 <- file.path(fp, i-1L)
        altReadObject(fp2)
    })
    names(gs) <- sfnames
    gs
}

.readGraphs <- function(path) {
    d <- file.path(path, "spatial_graphs")
    if (!dir.exists(d)) return(NULL)
    ms <- c("row", "col", "annot")
    # Top level should be samples, and then margin, then individual graphs
    samples <- read_json(file.path(d, "names.json"), simplifyVector = TRUE)
    out <- vector("list", length(samples))
    for (i in seq_along(samples)) {
        dd <- file.path(d, i-1L)
        mlist <- vector("list", 3)
        names(mlist) <- ms
        for (m in ms) {
            ddd <- file.path(dd, m)
            if (dir.exists(ddd)) {
                graph_names <- read_json(file.path(ddd, "names.json"), simplifyVector = TRUE)
                gs <- lapply(seq_along(graph_names), function(j) {
                    altReadObject(file.path(ddd, j-1L))
                })
                names(gs) <- graph_names
                mlist[[m]] <- gs
            }
        }
        out[[i]] <- mlist
    }
    names(out) <- samples
    return(out)
}

.readLocalResults <- function(path) {
    d <- file.path(path, "local_results")
    if (!dir.exists(d)) return(NULL)
    lr_names <- read_json(file.path(d, "names.json"), simplifyVector = TRUE)
    out <- lapply(seq_along(lr_names), function(i) {
        # Type of analysis, e.g. localmoran
        dd <- file.path(d, i-1L)
        features <- read_json(file.path(dd, "names.json"), simplifyVector = TRUE)
        feature_res <- lapply(seq_along(features), function(j) {
            # Results for each gene/feature
            altReadObject(file.path(dd, j-1L))
        })
        names(feature_res) <- features
    })
    names(out) <- lr_names
    return(out)
}

.readReducedDimFeatureData <- function(sfe, path) {
    d <- file.path(path, "reduced_dimensions")
    if (!dir.exists(d)) return(sfe)
    rd_names <- read_json(file.path(d, "names.json"), simplifyVector = TRUE)
    rds <- reducedDims(sfe)
    rds <- lapply(seq_along(rd_names), function(i) {
        dd <- file.path(d, i-1L, "feature_data")
        if (dir.exists(dd)) {
            fd <- altReadObject(dd)
            attr(rds[[i]], "featureData") <- fd
        }
    })
    reducedDims(sfe) <- rds
    sfe
}

#' Read SFE object from alabaster on disk representation
#'
#' @inheritParams alabaster.base::readObject
#'
#' @export
#' @importFrom spdep mat2listw
#' @examples
#' # example code
#'
readSpatialFeatureExperiment <- function(path, metadata = NULL, ...) {
    if (is.null(metadata))
        metadata <- readObjectFile(path)
    metadata$type <- "spatial_experiment"
    spe <- altReadObject(path, metadata, ...)
    
    sfe <- toSpatialFeatureExperiment(spe, 
                                      rowGeometries = .readGeometries(path, "row"),
                                      colGeometries = .readGeometries(path, "col"),
                                      annotGeometries = .readGeometries(path, "annot"),
                                      spatialGraphs = .readGraphs(path))
    # localResults
    localResults(sfe) <- .readLocalResults(path)
    # reducedDimFeatureData
    sfe <- .readReducedDimFeatureData(sfe, path)
    sfe
}
