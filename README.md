# crstools

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/EvolEcolGroup/crstools/graph/badge.svg?token=Z0JX70MBhw)](https://codecov.io/gh/EvolEcolGroup/crstools)
[![R-CMD-check](https://github.com/EvolEcolGroup/crstools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EvolEcolGroup/crstools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of crstools is to facilitate workign with map projections (technically
coordinate reference systems, CRS) in R. We provide functions to choose the
appropriate map projection for a given application,
visualise the resulting distortion, and georeference data from unknown
projections. The package seemlessly interfaces with the popular `sf` and
`terra` packages for spatial data handling.

## Installation

You can install the development version of crstools from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("EvolEcolGroup/crstools")
```
