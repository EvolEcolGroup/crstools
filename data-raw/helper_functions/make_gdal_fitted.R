# This scripts create the "inst/extdata/gdal_fitted.RDS" file which is used 
# in test_get_gcp_residuals to test the function. 

# path to the conda binary, get it with `conda run -n gdal which gdaltransform`
gdaltransform <- "/opt/homebrew/Caskroom/miniconda/base/envs/gdal/bin/gdaltransform"

# check that it runs, and keep the version
gdal_version <- system2(gdaltransform, "--version", stdout = TRUE)
gdal_version

#helper function to get predicted lon/lat from GDAL
gdal_fitted <- function(gcp, order = NULL) {
  # get arguments
  gdal_args <- c(rbind(
    "-gcp",
    sprintf("%.17g", gcp$x),
    sprintf("%.17g", gcp$y),
    sprintf("%.17g", gcp$longitude),
    sprintf("%.17g", gcp$latitude)
  ))
  # polynomial order
  if (!is.null(order)) {
    gdal_args <- c(gdal_args, "-order", order)
  }
  # tranform GCP pixel position with GDAL fit
  gdal_out <- system2(
    gdaltransform,
    c("-output_xy", gdal_args),
    input = sprintf("%.17g %.17g", gcp$x, gcp$y),
    stdout = TRUE
  )
  # quit if GDAL failed
  if (!is.null(attr(gdal_out, "status"))) {
    stop("gdaltransform failed with status ", attr(gdal_out, "status"))
  }
  # quite is not one line per GCP
  if (length(gdal_out) != nrow(gcp)) {
    stop("gdaltransform output has ", length(gdal_out), " lines, expected ",
         nrow(gcp))
  }
  # read output into matrix with col 1 for lon and col 2 for lat
  matrix(scan(text = gdal_out, quiet = TRUE), ncol = 2, byrow = TRUE)
}

# synthetic GCPs, same as in test_gcp_residual.R
set.seed(123)
n_gcp <- 15
gcp_syn <- data.frame(
  id = seq_len(n_gcp),
  x = runif(n_gcp, 0, 3000),
  y = runif(n_gcp, 0, 2500)
)
# get longitude for each GCP by starting at z + x with x*y and sd=0.3
# this makes the map non-linear and noi polynomial fits perfectly
gcp_syn$longitude <- -25 + 0.02 * gcp_syn$x + 1e-6 * gcp_syn$x * gcp_syn$y +
  rnorm(n_gcp, sd = 0.3)
# same approach for latitude
gcp_syn$latitude <- 70 - 0.013 * gcp_syn$y + 2e-6 * gcp_syn$x^2 +
  rnorm(n_gcp, sd = 0.3)

# GDAL fitted positions for each order and for auto with 5, 8 and 15 GCPs
gdal_ref <- list(
  gcp = gcp_syn,
  gdal_version = gdal_version,
  poly_1 = gdal_fitted(gcp_syn, order = 1),
  poly_2 = gdal_fitted(gcp_syn, order = 2),
  poly_3 = gdal_fitted(gcp_syn, order = 3),
  auto_5 = gdal_fitted(gcp_syn[1:5, ]),
  auto_8 = gdal_fitted(gcp_syn[1:8, ]),
  auto_15 = gdal_fitted(gcp_syn)
)

# save
saveRDS(gdal_ref, "inst/extdata/gdal_fitted.rds")