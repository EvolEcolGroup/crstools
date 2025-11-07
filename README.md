# crstools

<!-- badges: start -->
[![codecov](https://codecov.io/gh/EvolEcolGroup/crstools/graph/badge.svg?token=Z0JX70MBhw)](https://codecov.io/gh/EvolEcolGroup/crstools)
[![R-CMD-check](https://github.com/EvolEcolGroup/crstools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/EvolEcolGroup/crstools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of crstools is to facilitate working with map projections (technically
coordinate reference systems, CRS) in R. We provide functions to choose the
appropriate map projection for a given application,
visualise the resulting distortion, and georeference data from unknown
projections. The package seamlessly interfaces with the popular `sf` and
`terra` packages for spatial data handling.

## Installation

You can install the development version of crstools from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("EvolEcolGroup/crstools")
```

## Getting help

`crstools` is a relatively new package. If you think you have found a bug,
or have a feature request, please open an issue on our
[GitHub repository]((https://github.com/EvolEcolGroup/crstools/issues). Before doing so, please 
make sure that you have installed the latest **development** version of
`crstools` (as the bug might have already been fixed), as well as updating 
all other packages on your system. If the problem persists, and there is no issue
already opened that deals with your bug, file a new issue **providing** a [reproducible
example](https://reprex.tidyverse.org/)
for the developers to investigate the problem. A small **reproducible example** is
crucial in allowing us to understand the problem and fix it, so please do your best to
come up with the shortest bit of code needed to show the bug.
