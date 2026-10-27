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

test_that("matrix are not accepted", {
  expect_error(
    suggest_crs(matrix(c(-60, 20, 40, 70), nrow = 2)),
    "x must be a numeric vector"
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

test_that("projected sf and SpatRaster objects are reprojected", {
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(
      rbind(c(-80, -55), c(-35, -55), c(-35, 12), c(-80, 12), c(-80, -55))
    )),
    crs = 4326
  ))
  ref <- suggest_crs(area, quiet = TRUE)
  # a projected sf object
  area_merc <- sf::st_transform(area, "+proj=merc +units=km")
  expect_equal(suggest_crs(area_merc, quiet = TRUE), ref)
  # a SpatRaster in lon/lat and in a projected CRS
  area_r <- terra::rast(
    terra::ext(-80, -35, -55, 12),
    crs = "EPSG:4326", nrows = 10, ncols = 10
  )
  expect_equal(suggest_crs(area_r, quiet = TRUE), ref)
  # Mercator maps a lon/lat rectangle onto a rectangle, so the extent is the
  # same
  area_3857 <- sf::st_transform(area, 3857)
  area_r_3857 <- terra::rast(
    terra::ext(as.vector(sf::st_bbox(area_3857))[c(1, 3, 2, 4)]),
    crs = "EPSG:3857", nrows = 10, ncols = 10
  )
  expect_equal(suggest_crs(area_r_3857, quiet = TRUE), ref)
  # objects without a CRS are assumed to be in lon/lat
  area_no_crs <- sf::st_set_crs(area, NA)
  expect_equal(suggest_crs(area_no_crs, quiet = TRUE), ref)
})
