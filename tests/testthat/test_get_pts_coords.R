skip_if_not_installed("scales")
skip_if_not_installed("sf")

library(testthat)
library(sf)

# -----------------------------------------------------------------------------
# Shared synthetic dataset
# -----------------------------------------------------------------------------

# A genuine 2D grid of known lon/lat locations.
# These are used as GCPs.
lon_grid <- seq(-3.2, -3.0, length.out = 5)
lat_grid <- seq(55.8, 56.0, length.out = 4)

gcp_ll <- expand.grid(
  lon = lon_grid,
  lat = lat_grid
)

gcp_ll$id <- seq_len(nrow(gcp_ll))

sf_gcp_ll <- st_as_sf(
  gcp_ll,
  coords = c("lon", "lat"),
  crs = 4326
)

# -----------------------------------------------------------------------------
# Project GCPs to a projected CRS
# -----------------------------------------------------------------------------

sf_gcp_proj <- st_transform(sf_gcp_ll, 3857)

gcp_proj_coords <- st_coordinates(sf_gcp_proj)

# -----------------------------------------------------------------------------
# Create synthetic pixel coordinates for GCPs
# -----------------------------------------------------------------------------

gcp_px <- scales::rescale(gcp_proj_coords[, 1], to = c(100, 1000))
gcp_py <- scales::rescale(gcp_proj_coords[, 2], to = c(100, 1000))

pixel_gcp <- data.frame(
  id = gcp_ll$id,
  x = gcp_px,
  y = gcp_py,
  lon = st_coordinates(sf_gcp_ll)[, 1],
  lat = st_coordinates(sf_gcp_ll)[, 2]
)

# -----------------------------------------------------------------------------
# Create a separate set of target points that lie BETWEEN grid nodes
# -----------------------------------------------------------------------------

# Midpoints in each direction, so all target points lie inside the GCP grid
lon_mid <- (lon_grid[-length(lon_grid)] + lon_grid[-1]) / 2
lat_mid <- (lat_grid[-length(lat_grid)] + lat_grid[-1]) / 2

target_ll <- expand.grid(
  lon = lon_mid,
  lat = lat_mid
)

target_ll$id <- seq_len(nrow(target_ll))

sf_target_ll <- st_as_sf(
  target_ll,
  coords = c("lon", "lat"),
  crs = 4326
)

sf_target_proj <- st_transform(sf_target_ll, 3857)

target_proj_coords <- st_coordinates(sf_target_proj)

# Rescale the target points using the SAME scaling derived from the GCP extent
x_rng <- range(gcp_proj_coords[, 1])
y_rng <- range(gcp_proj_coords[, 2])

target_px <- scales::rescale(target_proj_coords[, 1], to = c(100, 1000), from = x_rng)
target_py <- scales::rescale(target_proj_coords[, 2], to = c(100, 1000), from = y_rng)

target_pixel_df <- data.frame(
  id = target_ll$id,
  x = target_px,
  y = target_py,
  lon = st_coordinates(sf_target_ll)[, 1],
  lat = st_coordinates(sf_target_ll)[, 2]
)

# -----------------------------------------------------------------------------
# Create a mildly nonlinear warped version for TPS testing
# -----------------------------------------------------------------------------

pixel_gcp_tps <- data.frame(
  id = pixel_gcp$id,
  x = pixel_gcp$x + 8 * sin(pixel_gcp$y / 250) + 4 * cos(pixel_gcp$x / 300),
  y = pixel_gcp$y + 6 * cos(pixel_gcp$x / 220) - 3 * sin(pixel_gcp$y / 260),
  lon = pixel_gcp$lon,
  lat = pixel_gcp$lat
)

target_pixel_df_tps <- data.frame(
  id = target_pixel_df$id,
  x = target_pixel_df$x + 8 * sin(target_pixel_df$y / 250) + 4 * cos(target_pixel_df$x / 300),
  y = target_pixel_df$y + 6 * cos(target_pixel_df$x / 220) - 3 * sin(target_pixel_df$y / 260),
  lon = target_pixel_df$lon,
  lat = target_pixel_df$lat
)


# -----------------------------------------------------------------------------
# Small helper datasets for validation/error tests
# -----------------------------------------------------------------------------

basic_gcp <- data.frame(
  id = 1:3,
  x = c(0, 100, 0),
  y = c(0, 0, 100),
  lon = c(-3.0, -2.9, -3.0),
  lat = c(55.0, 55.0, 55.1)
)

basic_target <- data.frame(
  id = 1,
  x = 50,
  y = 50
)

# -----------------------------------------------------------------------------
# poly_1 recovery test
# -----------------------------------------------------------------------------

test_that("get_pts_coords correctly recovers lon/lat coordinates using poly_1", {
  gcp <- pixel_gcp
  target_pts <- target_pixel_df[, c("id", "x", "y")]
  truth <- target_pixel_df[, c("lon", "lat")]

  recovered <- get_pts_coords(
    gcp = gcp,
    target_pts = target_pts,
    transform_method = "poly_1"
  )

  expect_true(all(c("lon", "lat") %in% names(recovered)))
  expect_equal(nrow(recovered), nrow(target_pts))

  expect_equal(
    recovered$lon,
    truth$lon,
    tolerance = 1e-6
  )

  expect_equal(
    recovered$lat,
    truth$lat,
    tolerance = 1e-6
  )
})

# -----------------------------------------------------------------------------
# TPS recovery test
# -----------------------------------------------------------------------------

test_that("get_pts_coords correctly recovers lon/lat coordinates using tps", {
  skip_if_not_installed("fields")

  gcp <- pixel_gcp_tps
  target_pts <- target_pixel_df_tps[, c("id", "x", "y")]
  truth <- target_pixel_df_tps[, c("lon", "lat")]

  recovered <- get_pts_coords(
    gcp = gcp,
    target_pts = target_pts,
    transform_method = "tps",
    lambda = 1e-6
  )

  expect_true(all(c("lon", "lat") %in% names(recovered)))
  expect_equal(nrow(recovered), nrow(target_pts))

  expect_equal(
    recovered$lon,
    truth$lon,
    tolerance = 1e-3
  )

  expect_equal(
    recovered$lat,
    truth$lat,
    tolerance = 1e-3
  )
})

# -----------------------------------------------------------------------------
# cx/cy support
# -----------------------------------------------------------------------------

test_that("get_pts_coords accepts cx/cy columns", {
  gcp <- pixel_gcp
  target_pts <- target_pixel_df[, c("id", "x", "y")]
  names(target_pts) <- c("id", "cx", "cy")

  result <- get_pts_coords(
    gcp = gcp,
    target_pts = target_pts,
    transform_method = "poly_1"
  )

  expect_true(all(c("lon", "lat") %in% names(result)))
  expect_equal(nrow(result), nrow(target_pts))
})

# -----------------------------------------------------------------------------
# Invalid transform method
# -----------------------------------------------------------------------------

test_that("get_pts_coords throws error for invalid transform method", {
  expect_error(
    get_pts_coords(
      gcp = basic_gcp,
      target_pts = basic_target,
      transform_method = "banana"
    ),
    "Invalid 'transform_method'"
  )
})

# -----------------------------------------------------------------------------
# Missing GCP columns
# -----------------------------------------------------------------------------

test_that("get_pts_coords throws error for missing GCP columns", {
  bad_gcp <- basic_gcp[, c("id", "x", "y", "lon")]

  expect_error(
    get_pts_coords(
      gcp = bad_gcp,
      target_pts = basic_target
    ),
    "Missing required columns in 'gcp'"
  )
})

# -----------------------------------------------------------------------------
# Invalid target columns
# -----------------------------------------------------------------------------

test_that("get_pts_coords throws error for invalid target point columns", {
  bad_target <- data.frame(
    id = 1,
    px = 50,
    py = 50
  )

  expect_error(
    get_pts_coords(
      gcp = basic_gcp,
      target_pts = bad_target
    ),
    "target_pts must contain either columns"
  )
})

# -----------------------------------------------------------------------------
# poly_1 minimum GCP count
# -----------------------------------------------------------------------------

test_that("get_pts_coords validates minimum GCP count for poly_1", {
  small_gcp <- basic_gcp[1:2, ]

  expect_error(
    get_pts_coords(
      gcp = small_gcp,
      target_pts = basic_target,
      transform_method = "poly_1"
    ),
    "At least 3 GCPs are required"
  )
})

# -----------------------------------------------------------------------------
# poly_2 minimum GCP count
# -----------------------------------------------------------------------------

test_that("get_pts_coords validates minimum GCP count for poly_2", {
  gcp <- data.frame(
    id = 1:5,
    x = runif(5),
    y = runif(5),
    lon = runif(5),
    lat = runif(5)
  )

  expect_error(
    get_pts_coords(
      gcp = gcp,
      target_pts = basic_target,
      transform_method = "poly_2"
    ),
    "At least 6 GCPs are required"
  )
})

# -----------------------------------------------------------------------------
# poly_3 minimum GCP count
# -----------------------------------------------------------------------------

test_that("get_pts_coords validates minimum GCP count for poly_3", {
  gcp <- data.frame(
    id = 1:9,
    x = runif(9),
    y = runif(9),
    lon = runif(9),
    lat = runif(9)
  )

  expect_error(
    get_pts_coords(
      gcp = gcp,
      target_pts = basic_target,
      transform_method = "poly_3"
    ),
    "At least 10 GCPs are required"
  )
})


# -----------------------------------------------------------------------------
# Lambda validation
# -----------------------------------------------------------------------------

test_that("get_pts_coords validates lambda", {
  expect_error(
    get_pts_coords(
      gcp = basic_gcp,
      target_pts = basic_target,
      transform_method = "tps",
      lambda = "a"
    ),
    "'lambda' must be a single numeric value"
  )

  expect_error(
    get_pts_coords(
      gcp = basic_gcp,
      target_pts = basic_target,
      transform_method = "tps",
      lambda = -1
    ),
    "'lambda' must be >= 0"
  )
})
