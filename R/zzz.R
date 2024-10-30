.onLoad <- function(libname, pkgname) {
    registerReadObjectFunction("spatial_feature_experiment", readSpatialFeatureExperiment)
    registerReadObjectFunction("geotiff", readSpatRaster)
    registerReadObjectFunction("bioformats_image", readBioFormatsImage)
    registerReadObjectFunction("ext_image", readExtImage)
    registerReadObjectFunction("simple_feature", readSF)
    
    registerValidateObjectFunction("spatial_feature_experiment", validateSFE)
    registerValidateObjectFunction("simple_feature", validateSF)
}

.onUnload <- function(libname, pkgname) {
    registerReadObjectFunction("spatial_feature_experiment", NULL)
    registerReadObjectFunction("geotiff", NULL)
    registerReadObjectFunction("bioformats_image", NULL)
    registerReadObjectFunction("ext_image", NULL)
    registerReadObjectFunction("simple_feature", NULL)
    
    registerValidateObjectFunction("spatial_feature_experiment", NULL)
    registerValidateObjectFunction("simple_feature", NULL)
}
