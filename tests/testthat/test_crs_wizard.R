# styler: off

test_that("test input sanity", {
  # check if input is not an expected class
  site <- data.frame(
    latitude = c(11, 8),
    longitude = c(5, 9)
  )
  expect_error(
    suggest_crs(site, distortion = "equal_area"),
    "x must be a numeric vector "
  )

  # check for correct coordinates
  expect_error(
    suggest_crs(c(-180, 180, 90, -90), distortion = "equal_area", return_best = FALSE),
    "lat_min must be smaller than lat_max"
  )
  expect_error(
    suggest_crs(c(180, -180, -90, 90), distortion = "equal_area", return_best = FALSE),
    "lon_min must be smaller than lon_max"
  )
  expect_error(
    suggest_crs(c(-180, 180, -91, 90), distortion = "equal_area"),
    "Latitude values must be between -90 and 90"
  )
})

test_that("integer extents are accepted", {
  expect_identical(
    suggest_crs(c(-60L, 20L, 40L, 70L)),
    suggest_crs(c(-60, 20, 40, 70))
  )
})

test_that("quiet = TRUE suppresses messages for regional maps", {
  # extents covering east-west, north-south and square regional maps
  extents <- list(
    ew = c(-60, 20, 40, 70),
    ew_tropics = c(-40, 40, -5, 10),
    ns = c(-10, 10, 30, 70),
    square = c(-10, 10, 0, 20),
    square_mid = c(0, 30, 40, 60)
  )
  for (ext in extents) {
    for (dist in c("equal_area", "conformal", "equidistant")) {
      expect_silent(suggest_crs(ext, distortion = dist, quiet = TRUE))
    }
  }
  # messages are still given when quiet = FALSE
  expect_message(
    suggest_crs(c(-40, 40, -5, 10), distortion = "conformal"),
    "scale factor k"
  )
  expect_message(
    suggest_crs(c(-10, 10, 30, 70), distortion = "equal_area"),
    "compress the map in the north-south direction"
  )
  expect_message(
    suggest_crs(c(-10, 10, 0, 20), distortion = "conformal"),
    "scale factor"
  )
})
