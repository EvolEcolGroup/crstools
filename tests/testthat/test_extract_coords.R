# a small georeferenced raster
georef_r <- terra::rast(
  nrows = 10, ncols = 20, xmin = 0, xmax = 20, ymin = 40, ymax = 50,
  vals = 1:200, crs = "EPSG:4326"
)

# mock the interactive device (each call to extract_coords() opens a new
# device, which has to be closed), and the points clicked on the image
mock_clicks <- function(coords, env = parent.frame()) {
  local_mocked_bindings(
    x11 = function(...) grDevices::pdf(NULL),
    .package = "grDevices",
    .env = env
  )
  local_mocked_bindings(
    locator = function(...) coords,
    .package = "graphics",
    .env = env
  )
}

test_that("extract_coords checks its inputs", {
  expect_error(
    extract_coords(as.matrix(georef_r)),
    "georef_image must be a SpatRaster object"
  )
  r_no_crs <- georef_r
  terra::crs(r_no_crs) <- ""
  expect_error(
    extract_coords(r_no_crs),
    "georef_image must have a coordinate reference system"
  )
  expect_error(
    extract_coords(georef_r, coords_df = data.frame(id = 1, lon = 1)),
    "coords_df dataframe must contain columns"
  )
})

test_that("extract_coords returns the clicked points", {
  mock_clicks(list(x = c(5, 6), y = c(45, 46)))
  coords_df <- extract_coords(georef_r)
  grDevices::dev.off()
  expect_equal(
    coords_df,
    data.frame(id = 1:2, longitude = c(5, 6), latitude = c(45, 46))
  )
})

test_that("extract_coords appends new points to an existing data frame", {
  mock_clicks(list(x = c(5, 6), y = c(45, 46)))
  old_df <- data.frame(id = c(3, 7), longitude = c(1, 2), latitude = c(41, 42))
  coords_df <- extract_coords(georef_r, coords_df = old_df)
  grDevices::dev.off()
  expect_equal(coords_df$id, c(3, 7, 8, 9))
  expect_equal(coords_df$longitude, c(1, 2, 5, 6))
})

test_that("extract_coords works with an empty data frame", {
  mock_clicks(list(x = 5, y = 45))
  empty_df <- data.frame(
    id = integer(0), longitude = numeric(0), latitude = numeric(0)
  )
  expect_no_warning(coords_df <- extract_coords(georef_r, empty_df))
  grDevices::dev.off()
  expect_equal(coords_df$id, 1)
})

test_that("extract_coords works if no point is clicked", {
  mock_clicks(NULL)
  old_df <- data.frame(id = 1, longitude = 1, latitude = 41)
  expect_equal(extract_coords(georef_r, coords_df = old_df), old_df)
  grDevices::dev.off()
  expect_equal(nrow(extract_coords(georef_r)), 0)
  grDevices::dev.off()
})
