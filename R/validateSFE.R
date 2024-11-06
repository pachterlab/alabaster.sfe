.validate_list_dir <- function(path) {
    nms <- read_json(file.path(path, "names.json"), simplifyVector = TRUE)
    dirs_expect <- file.path(path, seq_along(nms)-1L)
    if (!all(dir.exists(dirs_expect))) {
        stop("Number of names and contents don't match")
    }
}

.validateGeometries <- function(path) {
    if (!dir.exists(path)) return(NULL)
    .validate_list_dir(path)
    nms <- read_json(file.path(path, "names.json"), simplifyVector = TRUE)
    for (i in seq_along(nms)) {
        validateObject(file.path(path, i-1L))
    }
}

.validateGraphs <- function(path) {
    if (!dir.exists(path)) return(NULL)
    .validate_list_dir(path)
    margins <- c("row", "col", "annot")
    # Samples
    samples <- read_json(file.path(path, "names.json"), simplifyVector = TRUE)
    for (i in seq_along(samples)) {
        d <- file.path(path, i-1L)
        dds <- file.path(d, margins)
        if (!any(dir.exists(dds))) {
            stop("Graphs of none of the margin are present")
        }
        dds <- dds[dir.exists(dds)]
        for (dd in dds) {
            .validate_list_dir(dd)
            nms2 <- read_json(file.path(dd, "names.json"), simplifyVector = TRUE)
            for (j in seq_along(nms2)) {
                validateObject(file.path(dd, j-1L))
            }
        }
    }
    
    
}


# Validate SFE representation
validateSFE <- function(path, metadata = NULL) {
    # Use SPE validation
    if (is.null(metadata))
        metadata <- readObjectFile(path)
    metadata$type <- "spatial_experiment"
    validateObject(path, metadata)
    
    # Validate geometries if present
    .validateGeometries(file.path(path, "colgeometries"))
    .validateGeometries(file.path(path, "rowgeometries"))
    .validateGeometries(file.path(path, "annotgeometries"))
    
    # Validate spatial graphs if present
    .validateGraphs(file.path(path, "spatial_graphs"))
}
