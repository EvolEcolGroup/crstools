ref_map <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(rbind(c(0, 40), c(10, 40), c(10, 50), c(0, 50), c(0, 40)))),
  crs = 4326
))

test_that("find_gcp_coords checks its inputs", {
  gcp <- data.frame(id = 1, x = 1, y = 1, longitude = NA, latitude = NA)
  expect_error(
    find_gcp_coords(gcp[, 1:3], ref_map),
    "gcp must be a data frame with columns"
  )
  expect_error(
    find_gcp_coords(gcp, sf::st_geometry(ref_map)),
    "sf_obj must be an sf object"
  )
  gcp$longitude <- 1
  gcp$latitude <- 41
  expect_error(
    find_gcp_coords(gcp, ref_map),
    "all GCPs already have coordinates"
  )
})

test_that("find_gcp_coords stops once all GCPs have coordinates", {
  gcp <- data.frame(
    id = 1:3,
    x = 1:3,
    y = 1:3,
    longitude = c(1, NA, NA),
    latitude = c(41, NA, NA)
  )
  # each click returns a new point; the user never presses ESC
  n_clicks <- 0
  local_mocked_bindings(
    x11 = function(...) grDevices::pdf(NULL),
    .package = "grDevices"
  )
  local_mocked_bindings(
    locator = function(...) {
      n_clicks <<- n_clicks + 1
      if (n_clicks > 10) stop("too many clicks requested")
      list(x = n_clicks, y = 40 + n_clicks)
    },
    .package = "graphics"
  )
  expect_message(new_gcp <- find_gcp_coords(gcp, ref_map), "Click on the map")
  grDevices::dev.off()
  expect_equal(n_clicks, 2)
  expect_equal(new_gcp$longitude, c(1, 1, 2))
  expect_equal(new_gcp$latitude, c(41, 41, 42))
})

test_that("find_gcp_coords can be stopped before all GCPs have coordinates", {
  gcp <- data.frame(
    id = 1:3,
    x = 1:3,
    y = 1:3,
    longitude = NA_real_,
    latitude = NA_real_
  )
  n_clicks <- 0
  local_mocked_bindings(
    x11 = function(...) grDevices::pdf(NULL),
    .package = "grDevices"
  )
  # one click, then ESC
  local_mocked_bindings(
    locator = function(...) {
      n_clicks <<- n_clicks + 1
      if (n_clicks > 1) NULL else list(x = 5, y = 45)
    },
    .package = "graphics"
  )
  expect_message(new_gcp <- find_gcp_coords(gcp, ref_map), "Click on the map")
  grDevices::dev.off()
  expect_equal(new_gcp$longitude, c(5, NA, NA))
  expect_equal(new_gcp$latitude, c(45, NA, NA))
})
