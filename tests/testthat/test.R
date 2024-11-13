# Basically integration tests
library(terra)
library(SpatialFeatureExperiment)
library(Voyager)
library(SFEData)
library(SingleCellExperiment)
library(scater)

fp <- tempfile()
fn <- file.path(fp, "vizgen")
d <- VizgenOutput(dataset = "cellpose", file_path = fn)
suppressWarnings(sfe1 <- readVizgen(d, add_molecules = TRUE))

test_that("Save and read Vizgen data with SpatRaster", {
    fsave <- file.path(fp, "sfe_vizgen")
    saveObject(sfe1, fsave)
    expect_true(dir.exists(fsave))
    # Here make sure we don't get local_results and spatial_graphs
    expect_setequal(list.files(fsave), c("assays", "colgeometries", "column_data", "coordinates",
                                         "images", "OBJECT", "row_data", "rowgeometries"))
    
    sfe2 <- readObject(fsave)
    # can't directly expect_equal due to the images and ReloadedMatrix
    expect_s4_class(sfe2, "SpatialFeatureExperiment")
    expect_equal(dim(sfe1), dim(sfe2))
    expect_equal(assayNames(sfe1), assayNames(sfe2))
    expect_equal(colData(sfe1), colData(sfe2))
    expect_equal(rowData(sfe1), rowData(sfe2))
    expect_equal(colGeometries(sfe1), colGeometries(sfe2))
    expect_equal(rowGeometries(sfe1), rowGeometries(sfe2))
    expect_equal(SpatialFeatureExperiment::unit(sfe1), SpatialFeatureExperiment::unit(sfe2))
    expect_equal(SFEVersion(sfe1), SFEVersion(sfe2))
    # The images
    imgs1 <- lapply(imgData(sfe1)$data, terra::values)
    imgs2 <- lapply(imgData(sfe2)$data, terra::values)
    expect_equal(imgs1, imgs2, ignore_attr = "dimnames")
    # Extent
    exts1 <- lapply(imgData(sfe1)$data, ext)
    exts2 <- lapply(imgData(sfe1)$data, ext)
    expect_equal(exts1, exts2)
    unlink(fsave, recursive = TRUE)
})

# Add the other stuff
colGraph(sfe1, "knn5") <- findSpatialNeighbors(sfe1, method = "knearneigh", k = 5)
SpatialFeatureExperiment::centroids(sfe1)$foo <- rnorm(ncol(sfe1))
sfe1 <- logNormCounts(sfe1)
sfe1 <- runMoransI(sfe1, colGraphName = "knn5")
sfe1 <- colDataMoransI(sfe1, features = c("transcript_count", "anisotropy", 
                                          "perimeter_area_ratio", "solidity"))
sfe1 <- colGeometryMoransI(sfe1, colGeometryName = "centroids", features = "foo")
sfe1 <- runPCA(sfe1, ncomponents = 10)
sfe1 <- reducedDimMoransI(sfe1, components = 1:10)

sfe1 <- runUnivariate(sfe1, type = "localmoran", features = rownames(sfe1)[1])

test_that("Save and read sfe with other fields populated", {
    fsave <- file.path(fp, "sfe_vizgen")
    saveObject(sfe1, fsave)
    expect_setequal(list.files(fsave), 
                    c("assays", "colgeometries", "column_data", "coordinates", "images", 
                      "OBJECT", "reduced_dimensions", "row_data", "rowgeometries", "local_results",
                      "spatial_graphs"))
    expect_true(all(dir.exists(file.path(fsave, "colgeometries", "0", c("feature_data", "params")))))
    expect_true(all(dir.exists(file.path(fsave, "column_data", c("column_annotations", "other_annotations")))))
    expect_true(dir.exists(file.path(fsave, "reduced_dimensions", "0", "attrs")))
    expect_true(dir.exists(file.path(fsave, "row_data", "other_annotations")))
    
    sfe2 <- readObject(fsave)
    expect_s4_class(sfe2, "SpatialFeatureExperiment")
    expect_equal(colFeatureData(sfe2), colFeatureData(sfe1))
    expect_equal(metadata(colData(sfe2)), metadata(colData(sfe1)))
    expect_equal(rowFeatureData(sfe2), rowFeatureData(sfe1))
    expect_equal(spatialGraphs(sfe2), spatialGraphs(sfe1), ignore_attr = TRUE)
    expect_equal(getParams(sfe1, name = "localmoran", local = TRUE),
                 getParams(sfe2, name = "localmoran", local = TRUE))
    # I don't care if it's S3 or S4 data frame here, only care about the contents
    expect_equal(DataFrame(localResult(sfe1, "localmoran", "CD4"), check.names = FALSE),
                 localResult(sfe2, "localmoran", "CD4"))
    expect_equal(reducedDims(sfe1), reducedDims(sfe2), ignore_attr = TRUE)
    # Check the attributes
    expect_equal(names(attributes(reducedDim(sfe1))), names(attributes(reducedDim(sfe2))))
    unlink(fsave, recursive = TRUE)
})

x1 <- XeniumOutput(dataset = "v1", file_path = file.path(fp, "xenium1"))
x2 <- XeniumOutput("v2", file_path = file.path(fp, "xenium2"))

test_that("Save and read single file BioFormatsImage", {
    fsave <- file.path(fp, "bfi1")
    sfe <- readXenium(x1)
    bfi <- getImg(sfe)
    bfi <- affineImg(bfi, M = matrix(c(cos(pi/6), sin(pi/6), -sin(pi/6), cos(pi/6)), nrow = 2), 
                     v = c(0,0))
    saveObject(bfi, fsave)
    bfi2 <- readObject(fsave)
    # The path slot isn't supposed to match
    expect_equal(ext(bfi), ext(bfi2))
    expect_equal(transformation(bfi), transformation(bfi2))
    origin <- SpatialFeatureExperiment::origin
    expect_equal(origin(bfi), origin(bfi2), ignore_attr = "names")
    if (Sys.info()['sysname'] == "Windows") {
        spl1 <- strsplit(imgSource(bfi2), "[(\\)/]")[[1]]
        spl2 <- strsplit(normalizePath(file.path(fsave, "image.ome.tif")), "[(\\)/]")[[1]]
        expect_equal(tail(spl1, 3), tail(spl2,3))
    } else
        expect_equal(imgSource(bfi2), normalizePath(file.path(fsave, "image.ome.tif")))
    unlink(fsave, recursive = TRUE)
})

test_that("Save and read multi-file BFI", {
    fsave <- file.path(fp, "bfi2")
    sfe <- readXenium(x2)
    bfi <- getImg(sfe)
    saveObject(bfi, fsave)
    bfi2 <- readObject(fsave)
    expect_equal(ext(bfi), ext(bfi2))
    expect_equal(transformation(bfi), transformation(bfi2))
    origin <- SpatialFeatureExperiment::origin
    expect_equal(origin(bfi), origin(bfi2), ignore_attr = "names")
    expect_equal(basename(imgSource(bfi)), basename(imgSource(bfi2)))
    unlink(fsave, recursive = TRUE)
})

test_that("Save and read ExtImage", {
    fsave <- file.path(fp, "exi")
    sfe <- readXenium(x1)
    bfi <- getImg(sfe)
    exi <- toExtImage(bfi)
    saveObject(exi, fsave)
    exi2 <- readObject(fsave)
    expect_equal(ext(exi), ext(exi2))
    dimnames(exi) <- NULL
    expect_equal(as.array(exi), as.array(exi2))
    unlink(fsave, recursive = TRUE)
})
