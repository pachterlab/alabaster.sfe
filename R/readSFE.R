.readGeometries <- function(path, type = c("col", "row", "annot")) {
    tg <- paste0(type, "geometries")
    fp <- file.path(path, tg)
    if (!dir.exists(fp)) return(NULL)
    message(">>> Reading ", tg)
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
    message(">>> Reading spatial graphs")
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
                    dddd <- file.path(ddd, j-1L)
                    method <- altReadObject(file.path(dddd, "method"))
                    # There's a slow R loop in this, need to optimize or do away with listw
                    m <- mat2listw(altReadObject(dddd) |> as("CsparseMatrix"),
                                   style = method$args$style, 
                                   zero.policy = method$args$zero.policy)
                    attr(m, "method") <- method
                    m
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

.readLocalResults <- function(sfe, path) {
    d <- file.path(path, "local_results")
    if (!dir.exists(d)) return(sfe)
    message(">>> Reading localResults")
    lrs <- altReadObject(d)
    int_colData(sfe)$localResults <- lrs
    sfe
}

.readReducedDimFeatureData <- function(sfe, path) {
    d <- file.path(path, "reduced_dimensions")
    if (!dir.exists(d)) return(sfe)
    rd_names <- read_json(file.path(d, "names.json"), simplifyVector = TRUE)
    rds <- reducedDims(sfe)
    rds <- lapply(seq_along(rd_names), function(i) {
        rd <- as.matrix(rds[[i]])
        dd <- file.path(d, i-1L, "attrs")
        if (dir.exists(dd)) {
            a <- altReadObject(dd)
            attributes(rd) <- c(attributes(rd), a)
        }
        rd
    })
    names(rds) <- rd_names
    reducedDims(sfe) <- rds
    sfe
}

#' Read SFE object from alabaster on disk representation
#'
#' @inheritParams alabaster.base::readObject
#' @return A \code{SpatialFeatureExperiment} object
#' @export
#' @importFrom spdep mat2listw
#' @importFrom SingleCellExperiment int_metadata<- int_metadata reducedDims reducedDims<-
#' @examples
#' library(SFEData)
#' fp <- tempfile()
#' fn <- file.path(fp, "vizgen")
#' d <- VizgenOutput(dataset = "cellpose", file_path = fn)
#' suppressWarnings(sfe <- readVizgen(d))
#' fsave <- file.path(fp, "sfe_save")
#' saveObject(sfe, fsave)
#' sfe2 <- readObject(fsave)
#'
readSpatialFeatureExperiment <- function(path, metadata = NULL, ...) {
    if (is.null(metadata))
        metadata <- readObjectFile(path)
    message(">>> Reading SpatialExperiment")
    metadata$type <- "spatial_experiment"
    spe <- altReadObject(path, metadata, ...)
    
    sfe <- toSpatialFeatureExperiment(spe, 
                                      rowGeometries = .readGeometries(path, "row"),
                                      colGeometries = .readGeometries(path, "col"),
                                      annotGeometries = .readGeometries(path, "annot"),
                                      spatialGraphs = .readGraphs(path))
    # localResults
    sfe <- .readLocalResults(sfe, path)
    # reducedDimFeatureData
    sfe <- .readReducedDimFeatureData(sfe, path)
    int_metadata(sfe)$unit <- metadata$spatial_feature_experiment$unit
    int_metadata(sfe)$SFE_version <- package_version(metadata$spatial_feature_experiment$version)
    sfe
}
