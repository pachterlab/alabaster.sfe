.onLoad <- function(libname, pkgname) {
    registerReadObjectFunction("spatial_feature_experiment", readSpatialFeatureExperiment)
    registerReadObjectFunction("geotiff", readSpatRaster)
    registerReadObjectFunction("bioformats_image", readBioFormatsImage)
    registerReadObjectFunction("ext_image", readExtImage)
    registerReadObjectFunction("simple_feature", readSF)
    
    registerValidateObjectFunction("spatial_feature_experiment", validateSFE)
    registerValidateObjectFunction("simple_feature", validateSF)
    registerValidateObjectFunction("geotiff", validateGeoTIFF)
    registerValidateObjectFunction("bioformats_image", validateBFI)
    registerValidateObjectFunction("ext_image", validateExtImage)
    
    registerValidateObjectSatisfiesInterface("geotiff", "IMAGE")
    registerValidateObjectSatisfiesInterface("bioformats_image", "IMAGE")
    registerValidateObjectSatisfiesInterface("ext_image", "IMAGE")
}

.onUnload <- function(libname, pkgname) {
    registerReadObjectFunction("spatial_feature_experiment", NULL)
    registerReadObjectFunction("geotiff", NULL)
    registerReadObjectFunction("bioformats_image", NULL)
    registerReadObjectFunction("ext_image", NULL)
    registerReadObjectFunction("simple_feature", NULL)
    
    registerValidateObjectFunction("spatial_feature_experiment", NULL)
    registerValidateObjectFunction("simple_feature", NULL)
    registerValidateObjectFunction("geotiff", NULL)
    registerValidateObjectFunction("bioformats_image", NULL)
    registerValidateObjectFunction("ext_image", NULL)
    
    registerValidateObjectSatisfiesInterface("geotiff", "IMAGE", action = "remove")
    registerValidateObjectSatisfiesInterface("bioformats_image", "IMAGE", action = "remove")
    registerValidateObjectSatisfiesInterface("ext_image", "IMAGE", action = "remove")
}
