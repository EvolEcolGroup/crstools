test_that("plot_gcp creates a correct plot with gcp points", {
  img_path <- system.file("extdata/europe_map.jpeg", package = "crstools")
  # load some previously captured GCPs
  gcp_europe <- readRDS(system.file(
    "extdata/europe_gcp_georef.RDS",
    package = "crstools"
  ))
  # plot
  expect_no_error(plot_gcp(img_path, gcp = gcp_europe))
  # now give it an incorrect gcp dataframe
  gcp_wrong <- data.frame(
    id = 1:5,
    x = c(100, 200, 300, 400, 500),
    latitude = c(100, 200, 300, 400, 500),
    longitude = c(10, 20, 30, 40, 50)
  )
  expect_error(
    plot_gcp(img_path, gcp = gcp_wrong),
    "gcp dataframe must contain columns: id, x, y"
  )
  # test for incorrect image size attribute
  attr(gcp_europe, "image_dims") <- c(100, 100, 3)
  expect_error(
    plot_gcp(img_path, gcp = gcp_europe),
    "Image dimensions do not match the dimensions stored in gcp."
  )
  # invalid path
  expect_error(
    plot_gcp("non_existent_file.jpeg", gcp = gcp_europe),
    "File does not exist: non_existent_file.jpeg"
  )
  # invalid array for image file (e.g. a 2D array instead of a 3D RGB array)
  invalid_img_array <- array(1:100, dim = c(10, 10))
  expect_error(
    plot_gcp(invalid_img_array, gcp = gcp_europe),
    "Image must be a colour image with three channels"
  )
})

test_that("plot_gcp uses columns as x and rows as y for non-square images", {
  # image 20 pixels high (rows) and 40 pixels wide (columns)
  img <- array(0.5, dim = c(20, 40, 3))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  plot_gcp(img, gcp = data.frame(id = 1, x = 35, y = 5))
  usr <- graphics::par("usr")
  # default axes extend the range by 4% on each side
  expect_equal(usr[1:2], c(0, 40) + c(-1, 1) * 0.04 * 40)
  expect_equal(usr[3:4], c(0, 20) + c(-1, 1) * 0.04 * 20)
})

test_that("choose_gcp uses columns as x and rows as y for non-square images", {
  img <- array(0.5, dim = c(20, 40, 3))
  # mock the interactive device and the clicks on the image
  local_mocked_bindings(
    x11 = function(...) grDevices::pdf(NULL),
    .package = "grDevices"
  )
  local_mocked_bindings(
    locator = function(...) list(x = c(35, 5), y = c(5, 15)),
    .package = "graphics"
  )
  gcp <- choose_gcp(img)
  usr <- graphics::par("usr")
  grDevices::dev.off()
  expect_equal(usr[1:2], c(0, 40) + c(-1, 1) * 0.04 * 40)
  expect_equal(usr[3:4], c(0, 20) + c(-1, 1) * 0.04 * 20)
  expect_equal(gcp$x, c(35, 5))
  expect_equal(gcp$y, c(5, 15))
  expect_equal(attr(gcp, "image_dims"), c(20, 40, 3))
})

test_that("choose_gcp works if no point is clicked", {
  img <- array(0.5, dim = c(20, 40, 3))
  local_mocked_bindings(
    x11 = function(...) grDevices::pdf(NULL),
    .package = "grDevices"
  )
  local_mocked_bindings(locator = function(...) NULL, .package = "graphics")
  gcp <- choose_gcp(img)
  grDevices::dev.off()
  expect_equal(nrow(gcp), 0)
  expect_equal(attr(gcp, "image_dims"), c(20, 40, 3))
  # previous GCPs are returned unchanged
  old_gcp <- data.frame(id = 1, x = 2, y = 3, longitude = NA, latitude = NA)
  gcp <- choose_gcp(img, gcp = old_gcp)
  grDevices::dev.off()
  expect_equal(gcp[, names(old_gcp)], old_gcp)
})

test_that("choose_gcp works with an empty gcp data frame", {
  img <- array(0.5, dim = c(20, 40, 3))
  local_mocked_bindings(
    x11 = function(...) grDevices::pdf(NULL),
    .package = "grDevices"
  )
  local_mocked_bindings(
    locator = function(...) list(x = 1, y = 2),
    .package = "graphics"
  )
  empty_gcp <- data.frame(
    id = integer(0), x = numeric(0), y = numeric(0),
    longitude = numeric(0), latitude = numeric(0)
  )
  expect_no_warning(gcp <- choose_gcp(img, gcp = empty_gcp))
  grDevices::dev.off()
  expect_equal(gcp$id, 1)
})
