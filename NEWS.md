# crstools dev
* implement plotting function for GCPs.
* Allow to choose transformation method when georeferencing rasters.
* Add `get_points_coords()` function to georeference individual points with 
  a chosen transformation method.
* Add `get_gcp_residuals()` function` to get the residuals of polynomials to
  assess the quality of the transformation.
* `suggest_crs()` now accepts integer extents, reprojects `sf` and `SpatRaster`
  objects in a projected CRS to longitude/latitude, and honours `quiet = TRUE`
  for regional maps.
* Fix the proj4 string of Winkel Tripel (which defined a different projection
  from the WKT when used by `sf` or `terra`) and the WKT of Hammer (which could
  not be parsed by PROJ).
* Fix `choose_gcp()` and `plot_gcp()` swapping the width and height of
  non-square images, which led to incorrect georeferencing.
* `georeference_img()` now works with images supplied as arrays, passes GCPs to
  GDAL without rounding them to 7 significant digits, and checks that there
  are enough GCPs for the chosen polynomial.
* `get_pts_coords()` now stops if the GCPs contain missing values or cannot
  support the chosen polynomial.
* Fix several bugs in `extract_coords()`, `find_gcp_coords()` and
  `choose_gcp()` when no points, or more points than needed, are clicked.
* `geom_tissot()` now draws the indicatrices in every facet, and
  `get_distortion_metrics()` uses the same grid of centres as `geom_tissot()`.

# crstools 0.1.0
* Initial CRAN submission.
