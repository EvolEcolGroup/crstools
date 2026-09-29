#################
## sanity checks
#################

# load GCP europe
gcp_europe <- readRDS(system.file("extdata/europe_gcp_georef.RDS",
                                  package = "crstools"))

# check imput class
test_that("gcp input class", {
  # turn into a matrix
  gcp_europe_matrix <- as.matrix(gcp_europe)
  # check that the function throws an error when gcp is not a dataframe
  expect_error(gcp_residuals(gcp_europe_matrix, transform_method = "poly_1"), 
               "gcp must be a data frame with columns")
  
})

# check column sanity
test_that("gcp input columns", {
  # remove a column
  gcp_europe_missing_col <- gcp_europe[, -1]
  # check that the function throws an error when gcp is missing a column
  expect_error(gcp_residuals(gcp_europe_missing_col,
                             transform_method = "poly_1"), 
               "gcp must be a data frame with columns")
  # rename a column
  gcp_europe_renamed_col <- gcp_europe
  colnames(gcp_europe_renamed_col)[which(colnames(gcp_europe_renamed_col)
                                         == "longitude")] <- "long"
  # check that the function throws an error when gcp has a wrong column name
  expect_error(gcp_residuals(gcp_europe_renamed_col,
                             transform_method = "poly_1"), 
               "gcp must be a data frame with columns")
})


# check for NAs 
test_that("gcp input NAs", {
  # introduce an NA in coordinates
  gcp_europe_with_na <- gcp_europe
  gcp_europe_with_na$latitude[2] <- NA
  # check that the function throws an error when gcp has NAs
  expect_error(gcp_residuals(gcp_europe_with_na, transform_method = "poly_1"), 
               "GCP dataframe contains NA values")
  # introduce an NA in pixels
  gcp_europe_with_na_pixels <- gcp_europe
  gcp_europe_with_na_pixels$x[3] <- NA
  # check that the function throws an error when gcp has NAs
  expect_error(gcp_residuals(gcp_europe_with_na_pixels,
                             transform_method = "poly_1"), 
               "GCP dataframe contains NA values")
  # NAs in non used columns should not throw an error
  gcp_extra <- gcp_europe
  gcp_extra$notes <- NA
  expect_no_error(gcp_residuals(gcp_extra, transform_method = "poly_1"))
  # check column is dropped in the output
  res_extra <- gcp_residuals(gcp_extra, transform_method = "poly_1")
  expect_false("notes" %in% colnames(res_extra))
  
})


# check for available transformation method
test_that("unavailable transformation method", {
  # use not available method 
  expect_error(gcp_residuals(gcp_europe, transform_method = "poly_4"), 
               "'arg' should be one of")
  
})

# check number of GCPs
test_that("number of GCPs", {
  # subset to two GCPs
  gcp_europe_two <- gcp_europe[1:2, ]
  # subset to five GCPs
  gcp_europe_five <- gcp_europe[1:5, ]
  # check error for poly_1 with less than 3 GCPs
  expect_error(gcp_residuals(gcp_europe_two, transform_method = "poly_1"), 
               "A polynomial of order 1")
  # check error for poly_2 with less than 6 GCPs
  expect_error(gcp_residuals(gcp_europe_five, transform_method = "poly_2"), 
               "A polynomial of order 2")
  # check error for poly_3 with less than 10 GCPs
  expect_error(gcp_residuals(gcp_europe, transform_method = "poly_3"), 
               "A polynomial of order 3")
  
})


# check spread of GCPs
test_that("gcp_residuals catches GCPs that are not spread enough", {
  # GCPs all on the diagonal of the image (x = y), so x and y are collinear
  gcp_line <- data.frame(
    id = 1:5,
    x = c(0, 100, 200, 300, 400),
    y = c(0, 100, 200, 300, 400),
    longitude = c(2, 3, 4, 5, 6),
    latitude = c(45, 44, 43, 42, 41)
  )
  expect_error(
    gcp_residuals(gcp_line, transform_method = "poly_1"),
    "The GCPs are not sufficiently spread to fit a polynomial of order 1"
  )
  # GCPs on a single row of the image (same y) for a second order polynomial
  gcp_row <- data.frame(
    id = 1:8,
    x = seq(0, 700, by = 100),
    y = rep(250, 8),
    longitude = seq(2, 9),
    latitude = rep(45, 8)
  )
  expect_error(
    gcp_residuals(gcp_row, transform_method = "poly_2"),
    "The GCPs are not sufficiently spread to fit a polynomial of order 2"
  )
})


# check output structure
test_that("check output structure", {
  res <- gcp_residuals(gcp_europe, transform_method = "poly_1")
  # check that the output is a data frame
  expect_true(is.data.frame(res))
  # check that the output has the same number of rows as the input
  expect_equal(nrow(res), nrow(gcp_europe))
  # check columns names
  # columns in the documented order
  expect_equal(
    colnames(res),
    c(
      "id", "x", "y", "longitude", "latitude",
      "fitted_lon", "fitted_lat",
      "res_lon", "res_lat", "res_km",
      "leverage",
      "loo_lon", "loo_lat", "loo_km"
    )
  )
  # output is numeric and no NAs
  new_cols <- c(
    "fitted_lon", "fitted_lat", "res_lon", "res_lat", "res_km",
    "leverage", "loo_lon", "loo_lat", "loo_km"
  )
  expect_true(all(vapply(res[, new_cols], is.numeric, logical(1))))
  expect_false(anyNA(res[, new_cols]))
  # distances in km can not be negative
  expect_true(all(res$res_km >= 0))
  expect_true(all(res$loo_km >= 0))
  # attributes are present
  expect_equal(attr(res, "order"), 1L)
  expect_length(attr(res, "rmse_km"), 1)
  expect_length(attr(res, "loo_rmse_km"), 1)
  expect_true(attr(res, "rmse_km") >= 0)
  expect_true(attr(res, "loo_rmse_km") >= 0)
  
})

# check orders of polynomial are stored correctly
test_that("output stores the order of the polynomial", {
  # first order polynomial
  res_1 <- gcp_residuals(gcp_europe, transform_method = "poly_1")
  expect_equal(attr(res_1, "order"), 1L)
  # make a grid of GCPs to fit a second and thirds order polynomial
  gcp_grid <- expand.grid(x = c(0, 100, 200, 300), y = c(0, 100, 200, 300))
  gcp_grid <- data.frame(
    id = seq_len(nrow(gcp_grid)),
    x = gcp_grid$x,
    y = gcp_grid$y,
    longitude = 2 + 0.01 * gcp_grid$x + 1e-5 * gcp_grid$x * gcp_grid$y,
    latitude = 45 - 0.01 * gcp_grid$y + 1e-5 * gcp_grid$x^2
  )
  # second order polynomial
  res_2 <- gcp_residuals(gcp_grid, transform_method = "poly_2")
  expect_equal(attr(res_2, "order"), 2L)
  # third order polynomial
  res_3 <- gcp_residuals(gcp_grid, transform_method = "poly_3")
  expect_equal(attr(res_3, "order"), 3L)
  # the structure of the output does not change with the order
  expect_equal(colnames(res_1), colnames(res_2))
  expect_equal(colnames(res_1), colnames(res_3))
  expect_equal(nrow(res_3), nrow(gcp_grid))
})

# check output not affected by input order
test_that("output not affected by input order", {
  res <- gcp_residuals(gcp_europe, transform_method = "poly_1")
  gcp_shuffled <- gcp_europe[, c("latitude", "longitude", "y", "x", "id")]
  res_shuffled <- gcp_residuals(gcp_shuffled, transform_method = "poly_1")
  expect_equal(res, res_shuffled)
})


######################
## functional checks
######################

# check residual are exact for a perfect fit
test_that("residuals are exact for a perfect fit", {
  # syntethic GCPs
  # set seed 
  set.seed(123)
  n_gcp <- 15 
  gcp_syn <- data.frame(
    id = seq_len(n_gcp),
    x = runif(n_gcp, 0, 3000),
    y = runif(n_gcp, 0, 2500)
  )
  # LINEAR MAP 
  # longitude follow linear function 
  gcp_syn$longitude <- -25 + 0.02 * gcp_syn$x - 0.001 * gcp_syn$y
  # latitude follow linear function
  gcp_syn$latitude <- 70 + 0.002 * gcp_syn$x - 0.013 * gcp_syn$y
  # first order polynomial should fit perfectly
  res <- gcp_residuals(gcp_syn, transform_method = "poly_1")
  # predicted longitudes should match the syn ones 
  expect_equal(res$fitted_lon, gcp_syn$longitude)
  # predicted latitudes should match the syn ones
  expect_equal(res$fitted_lat, gcp_syn$latitude)
  # residuals should be very close to zero (not zero because of rounding error)
  expect_true(max(abs(c(res$res_lon, res$res_lat))) < 1e-8)
  # LOO residual should be very close to zero (not zero because of rounding
  # error)
  expect_true(max(abs(c(res$loo_lon, res$loo_lat))) < 1e-8)
  # overall RMSE should be very close to zero (not zero because of rounding
  # error)
  expect_true(attr(res, "rmse_km") < 1e-8)
  # the LOO RMSE should be very close to zero (not zero because of rounding
  # error)
  expect_true(attr(res, "loo_rmse_km") < 1e-8)
  # QUADRATIC MAP
  # longitude follow quadratic function (x * y)
  gcp_syn$longitude <- gcp_syn$longitude + 1e-6 * gcp_syn$x * gcp_syn$y
  # latitude follow quadratic function (x^2)
  gcp_syn$latitude <- gcp_syn$latitude + 2e-6 * gcp_syn$x^2
  # second order polynomial should fit perfectly
  res <- gcp_residuals(gcp_syn, transform_method = "poly_2")
  # predicted longitudes should match the syn ones
  expect_equal(res$fitted_lon, gcp_syn$longitude)
  # predicted latitudes should match the syn ones
  expect_equal(res$fitted_lat, gcp_syn$latitude)
  # residuals should be very close to zero (not zero because of rounding error)
  expect_true(max(abs(c(res$res_lon, res$res_lat))) < 1e-8)
  # LOO residual should be very close to zero (not zero because of rounding
  # error)
  expect_true(max(abs(c(res$loo_lon, res$loo_lat))) < 1e-8)
  # overall RMSE should be very close to zero (not zero because of rounding
  # error)
  expect_true(attr(res, "rmse_km") < 1e-8)
  # the LOO RMSE should be very close to zero (not zero because of rounding
  # error)
  expect_true(attr(res, "loo_rmse_km") < 1e-8)
  # first oder polynomial should not fit perfectly thus error must be 
  # larger than 1 km
  res_under <- gcp_residuals(gcp_syn, transform_method = "poly_1")
  expect_true(attr(res_under, "rmse_km") > 1)
  # CUBIC MAP
  # longitude follow cubic function (x^2 * y)
  gcp_syn$longitude <- gcp_syn$longitude + 1e-10 * gcp_syn$x^2 * gcp_syn$y
  # latitude follow cubic function (y^3)
  gcp_syn$latitude <- gcp_syn$latitude + 3e-10 * gcp_syn$y^3
  # third order polynomial should fit perfectly
  res <- gcp_residuals(gcp_syn, transform_method = "poly_3")
  # predicted longitudes should match the syn ones
  expect_equal(res$fitted_lon, gcp_syn$longitude)
  # predicted latitudes should match the syn ones
  expect_equal(res$fitted_lat, gcp_syn$latitude)
  # residuals should be very close to zero (not zero because of rounding error)
  expect_true(max(abs(c(res$res_lon, res$res_lat))) < 1e-8)
  # LOO residual should be very close to zero (not zero because of rounding
  # error)
  expect_true(max(abs(c(res$loo_lon, res$loo_lat))) < 1e-8)
  # overall RMSE should be very close to zero (not zero because of rounding
  # error)
  expect_true(attr(res, "rmse_km") < 1e-8)
  # the LOO RMSE should be very close to zero (not zero because of rounding
  # error)
  expect_true(attr(res, "loo_rmse_km") < 1e-8)
  # second order polynomial should not fit perfectly thus error must be
  # larger than 1 km
  res_under <- gcp_residuals(gcp_syn, transform_method = "poly_2")
  expect_true(attr(res_under, "rmse_km") > 1)
})

# check properties of least squares fit are maintained
test_that("proporties of least squares fit mantrained", {
  # set seed 
  set.seed(123)
  # syntethic GCPs scattered with some noise 
  n_gcp <- 30
  gcp_syn <- data.frame(
    id = seq_len(n_gcp),
    x = runif(n_gcp, 0, 3000),
    y = runif(n_gcp, 0, 2500)
  )
  gcp_syn$longitude <- -25 + 0.02 * gcp_syn$x + 1e-6 * gcp_syn$x * gcp_syn$y +
    rnorm(n_gcp, sd = 0.3)
  gcp_syn$latitude <- 70 - 0.013 * gcp_syn$y + 2e-6 * gcp_syn$x^2 +
    rnorm(n_gcp, sd = 0.3)
  # define number of coefficient for each order
  n_coeff <- c(3, 6, 10)
  # empty numeric vectors for loop
  rss <- numeric(3)
  rss_lon <- numeric(3)
  rss_lat <- numeric(3)
  # loop over polynomial order
  for (i in seq_along(n_coeff)) {
    # compute residuals
    res <- gcp_residuals(gcp_syn, transform_method = paste0("poly_", i))
    # check that observed = fitted + residual for lat and lon
    expect_equal(res$fitted_lon + res$res_lon, gcp_syn$longitude)
    expect_equal(res$fitted_lat + res$res_lat, gcp_syn$latitude)
    # check leverage is within range (0-1) and sums up o to number of
    # coefficients
    expect_true(all(res$leverage >= 0 & res$leverage <= 1))
    expect_equal(sum(res$leverage), n_coeff[i])
    # LOO residuals are never smaller than the ordinary residuals
    # as LOO = res/(1-leverage)
    expect_true(all(abs(res$loo_lon) >= abs(res$res_lon)))
    expect_true(all(abs(res$loo_lat) >= abs(res$res_lat)))
    # check residuals sum of squares for each axis
    rss_lon[i] <- sum(res$res_lon^2)
    rss_lat[i] <- sum(res$res_lat^2)
  }
  # check that residuals sum of squares are smaller for higher order polynomials
  # test for lat and lon
  expect_true(rss_lon[2] <= rss_lon[1])
  expect_true(rss_lon[3] <= rss_lon[2])
  expect_true(rss_lat[2] <= rss_lat[1])
  expect_true(rss_lat[3] <= rss_lat[2])
})


# Check that function returns same fitted coordinates as GDAL
test_that("gcp_residuals returns same fitted coordinates as GDAL", {
  # do not run on CRAN
  skip_on_cran()
  # do not run on GitHub Actions
  skip_on_ci()
  # skip if GDAL not available
  skip_if(
    Sys.which("gdaltransform") == "",
    "gdaltransform not available"
  )
  # helper fucntion to get predicted lon/lat from GDAL
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
      "gdaltransform",
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
  # set seed
  set.seed(123)
  # syntethic GCPs
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
  # loop over polynomial order
  for (i in 1:3) {
    # run gcp_residuals with polynomial order i
    res <- gcp_residuals(gcp_syn, transform_method = paste0("poly_", i))
    # get fitted coordinates from GDAL
    gdal_pos <- gdal_fitted(gcp_syn, order = i)
    # check that fitted coordinates from gcp_residuals and GDAL are equal
    # very small tollerance applied 
    # longitude
    expect_equal(res$fitted_lon, gdal_pos[, 1], tolerance = 1e-8)
    # latitude
    expect_equal(res$fitted_lat, gdal_pos[, 2], tolerance = 1e-8)
  }
  # check same thing with auto using different numbers of GCPs to trigger
  # different polynomial orders
  # use 15 GCPs but will only use first and second poly
  for (a in c(5,8, 15)){
    # run gcp_residuals with polynomial order a
    res <- gcp_residuals(gcp_syn[1:a, ], transform_method = "auto")
    # get fitted coordinates from GDAL
    gdal_pos <- gdal_fitted(gcp_syn[1:a, ])
    # check that fitted coordinates from gcp_residuals and GDAL are equal
    # very small tollerance applied 
    # longitude
    expect_equal(res$fitted_lon, gdal_pos[, 1], tolerance = 1e-8)
    # latitude
    expect_equal(res$fitted_lat, gdal_pos[, 2], tolerance = 1e-8)
  }
})
               
               