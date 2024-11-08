
# alabaster.sfe

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/pachterlab/alabaster.sfe/graph/badge.svg)](https://app.codecov.io/gh/pachterlab/alabaster.sfe)
[![R-CMD-check](https://github.com/pachterlab/alabaster.sfe/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pachterlab/alabaster.sfe/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of alabaster.sfe is to implement a language agnostic format to represent `SpatialFeatureExperiment` on disk, as in the [existing `alabaster` packages](https://github.com/orgs/ArtifactDB/repositories) from ArtifactDB. This serves to formalize and standardize the files written to disk when SFE reformats smFISH transcript spots and to improve interoperability between the R and Python implementations of Voyager.

## Installation

You can install the development version of alabaster.sfe from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pachterlab/alabaster.sfe")
```

This package has been submitted to Bioconductor and is under review
