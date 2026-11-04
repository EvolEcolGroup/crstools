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
