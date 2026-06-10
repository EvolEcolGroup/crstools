skip_if_not_installed("scales")

test_that("get_pts_coords correctly recovers lon/lat coordinates", {

  library(sf)
  
  # ---------------------------------------------------------------------------
  # Create example lon/lat points
  # ---------------------------------------------------------------------------
  
  pts_ll <- data.frame(
    id = 1:12,
    lon = seq(-3.2, -3.0, length.out = 12),
    lat = seq(55.8, 56.0, length.out = 12)
  )
  
  sf_ll <- st_as_sf(
    pts_ll,
    coords = c("lon", "lat"),
    crs = 4326
  )
  
  # ---------------------------------------------------------------------------
  # Project points to a projected CRS
  # ---------------------------------------------------------------------------
  
  sf_proj <- st_transform(sf_ll, 3857)
  
  coords_proj <- st_coordinates(sf_proj)
  
  # ---------------------------------------------------------------------------
  # Simulate pixel coordinates
  # ---------------------------------------------------------------------------
  
  px <- scales::rescale(coords_proj[,1], to = c(100, 1000))
  py <- scales::rescale(coords_proj[,2], to = c(100, 1000))
  
  pixel_df <- data.frame(
    id = pts_ll$id,
    x = px,
    y = py,
    lon = pts_ll$lon,
    lat = pts_ll$lat
  )
  
  # ---------------------------------------------------------------------------
  # Split into GCPs and holdout points
  # ---------------------------------------------------------------------------
  
  gcp <- pixel_df[1:8, ]
  
  target_pts <- pixel_df[9:12, c("id", "x", "y")]
  
  truth <- pixel_df[9:12, c("lon", "lat")]
  
  # ---------------------------------------------------------------------------
  # Recover coordinates
  # ---------------------------------------------------------------------------
  
  recovered <- get_pts_coords(
    gcp = gcp,
    target_pts = target_pts,
    transform_method = "poly_1"
  )
  
  # ---------------------------------------------------------------------------
  # Validate output structure
  # ---------------------------------------------------------------------------
  
  expect_true(all(c("lon", "lat") %in% names(recovered)))
  
  expect_equal(nrow(recovered), nrow(target_pts))
  
  # ---------------------------------------------------------------------------
  # Validate recovered coordinates
  # ---------------------------------------------------------------------------
  
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


test_that("get_pts_coords throws error for invalid transform method", {
  
  gcp <- data.frame(
    id = 1:3,
    x = c(0, 100, 0),
    y = c(0, 0, 100),
    lon = c(-3, -2.9, -3),
    lat = c(55, 55, 55.1)
  )
  
  target_pts <- data.frame(
    id = 1,
    x = 50,
    y = 50
  )
  
  expect_error(
    get_pts_coords(
      gcp = gcp,
      target_pts = target_pts,
      transform_method = "banana"
    ),
    "Invalid 'transform_method'"
  )
  
})


test_that("get_pts_coords throws error for missing GCP columns", {
  
  gcp <- data.frame(
    id = 1:3,
    x = c(0, 100, 0),
    y = c(0, 0, 100),
    lon = c(-3, -2.9, -3)
  )
  
  target_pts <- data.frame(
    id = 1,
    x = 50,
    y = 50
  )
  
  expect_error(
    get_pts_coords(
      gcp = gcp,
      target_pts = target_pts
    ),
    "Missing required columns in 'gcp'"
  )
  
})


test_that("get_pts_coords throws error for missing target point columns", {
  
  gcp <- data.frame(
    id = 1:3,
    x = c(0, 100, 0),
    y = c(0, 0, 100),
    lon = c(-3, -2.9, -3),
    lat = c(55, 55, 55.1)
  )
  
  target_pts <- data.frame(
    id = 1,
    x = 50
  )
  
  expect_error(
    get_pts_coords(
      gcp = gcp,
      target_pts = target_pts
    ),
    "Missing required columns in 'target_pts'"
  )
  
})


test_that("get_pts_coords validates minimum GCP count for poly_1", {
  
  gcp <- data.frame(
    id = 1:2,
    x = c(0, 100),
    y = c(0, 100),
    lon = c(-3, -2.9),
    lat = c(55, 55.1)
  )
  
  target_pts <- data.frame(
    id = 1,
    x = 50,
    y = 50
  )
  
  expect_error(
    get_pts_coords(
      gcp = gcp,
      target_pts = target_pts,
      transform_method = "poly_1"
    ),
    "At least 3 GCPs are required"
  )
  
})


test_that("get_pts_coords validates minimum GCP count for poly_2", {
  
  gcp <- data.frame(
    id = 1:5,
    x = runif(5),
    y = runif(5),
    lon = runif(5),
    lat = runif(5)
  )
  
  target_pts <- data.frame(
    id = 1,
    x = 0.5,
    y = 0.5
  )
  
  expect_error(
    get_pts_coords(
      gcp = gcp,
      target_pts = target_pts,
      transform_method = "poly_2"
    ),
    "At least 6 GCPs are required"
  )
  
})


test_that("get_pts_coords validates minimum GCP count for poly_3", {
  
  gcp <- data.frame(
    id = 1:9,
    x = runif(9),
    y = runif(9),
    lon = runif(9),
    lat = runif(9)
  )
  
  target_pts <- data.frame(
    id = 1,
    x = 0.5,
    y = 0.5
  )
  
  expect_error(
    get_pts_coords(
      gcp = gcp,
      target_pts = target_pts,
      transform_method = "poly_3"
    ),
    "At least 10 GCPs are required"
  )
  
})
