test_that("georeference_img works", {
  # get the path to an example image included in the package
  img_path <- system.file("extdata/europe_map.jpeg", package = "crstools")
  # load a set of GCPs generated with choose_gcp() but not yet passed through
  # find_gcp() to get coordinates
  gcp_df <- readRDS(system.file(
    "extdata/europe_gcp1.RDS",
    package = "crstools"
  ))
  # Expect an error as the gcp does not have lat and long
  expect_error(
    georeference_img(
      image_obj = img_path, gcp = gcp_df,
      output_path = tempfile(
        patter = "georef_img_",
        tmpdir = tempdir(),
        fileext = ".tif"
      )
    ),
    "gcp dataframe contains NA values."
  )
  # now check that we catch incorrect column headers
  gcp_df_invalid <- gcp_df
  colnames(gcp_df_invalid)[which(colnames(gcp_df_invalid) == "longitude")] <-
    "long"
  expect_error(
    georeference_img(
      image_obj = img_path, gcp = gcp_df_invalid,
      output_path = tempfile(
        patter = "georef_img_",
        tmpdir = tempdir(),
        fileext = ".tif"
      )
    ),
    "gcp must be a data frame with columns"
  )
  
  # now test different transformations
  gcp <- readRDS(system.file("extdata/europe_gcp_georef.RDS",
                             package = "crstools"))
  # warp image
  img_out <- georeference_img(img_path, gcp, output_path = tempfile(),
                   transform_method = "auto")
  expect_true(file.exists(img_out))
  img_out <- georeference_img(img_path, gcp, output_path = tempfile(),
                              transform_method = "tps")
  expect_true(file.exists(img_out))
  img_out <- georeference_img(img_path, gcp, output_path = tempfile(),
                              transform_method = "poly_1")
  expect_true(file.exists(img_out))
})

# check that the fucntion does not use third order polynomials with auto
# this happens because GDAL fucntion never uses 3rd order polynomials when in
# in auto, instead it uses 2nd order polynomials when there are more than 6 GCPs
# check related GitHub issue #36 for details. 
test_that("auto never uses 3rd order polynomial", {
  # make a grid of points to use as GCPs 
  g <- expand.grid(x = c(0, 30, 60, 90), y = c(0, 30, 60, 90))
  # map to cubic function to get exact fit
  # this allows to compare differences in fit with 2nd order poly
  gcp <- data.frame(
    id = 1:16, g,
    longitude = g$x^3 / 1e4,
    latitude = g$y^3 / 1e4
  )
  # generate a dummy image to georeference
  img <- tempfile(fileext = ".jpg")
  jpeg::writeJPEG(matrix(1, 100, 100), img)
  # georeference with auto, 2nd and 3rd order
  out_auto <- georeference_img(img, gcp, tempfile(),
                               transform_method = "auto")
  out_poly2 <- georeference_img(img, gcp, tempfile(),
                                transform_method = "poly_2")
  out_poly3 <- georeference_img(img, gcp, tempfile(),
                                transform_method = "poly_3")
  # check auto matches 2nd order polynomial
  expect_equal(
    as.vector(terra::ext(terra::rast(out_auto))),
    as.vector(terra::ext(terra::rast(out_poly2)))
  )
  # check auto does not match 3rd order polynomial
  expect_false(isTRUE(all.equal(
    as.vector(terra::ext(terra::rast(out_auto))),
    as.vector(terra::ext(terra::rast(out_poly3)))
  )))
})

test_that("georeference_img works with an image array", {
  gcp <- data.frame(
    id = 1:4,
    x = c(10, 90, 90, 10),
    y = c(10, 10, 40, 40),
    longitude = c(0, 8, 8, 0),
    latitude = c(40, 40, 43, 43)
  )
  # a non-square image, 50 pixels high and 100 pixels wide
  img_array <- array(0.5, dim = c(50, 100, 3))
  img_file <- tempfile(fileext = ".jpg")
  jpeg::writeJPEG(img_array, img_file)
  out_array <- georeference_img(img_array, gcp,
    output_path = tempfile(), transform_method = "poly_1"
  )
  out_file <- georeference_img(img_file, gcp,
    output_path = tempfile(), transform_method = "poly_1"
  )
  expect_true(file.exists(out_array))
  expect_equal(
    as.vector(terra::ext(terra::rast(out_array))),
    as.vector(terra::ext(terra::rast(out_file)))
  )
  # each pixel is 0.1 degrees, so the image covers 10 x 5 degrees
  expect_equal(
    as.vector(terra::ext(terra::rast(out_file))),
    c(xmin = -1, xmax = 9, ymin = 39, ymax = 44),
    tolerance = 1e-6
  )
})

test_that("georeference_img passes GCPs to GDAL at full precision", {
  img_file <- tempfile(fileext = ".jpg")
  jpeg::writeJPEG(array(0.5, dim = c(50, 100, 3)), img_file)
  # a small pixel size and coordinates with many significant digits, which
  # would be affected by rounding to 7 significant digits
  pixel_size <- 0.000123456789
  gcp <- data.frame(
    id = 1:4,
    x = c(10.123456, 90.654321, 90.654321, 10.123456),
    y = c(10.987654, 10.987654, 40.13579, 40.13579)
  )
  gcp$longitude <- 123.456789123 + gcp$x * pixel_size
  gcp$latitude <- -45.987654321 + gcp$y * pixel_size
  out <- georeference_img(img_file, gcp,
    output_path = tempfile(), transform_method = "poly_1"
  )
  img_ext <- as.vector(terra::ext(terra::rast(out)))
  expected_ext <- c(
    123.456789123, 123.456789123 + 100 * pixel_size,
    -45.987654321, -45.987654321 + 50 * pixel_size
  )
  # error smaller than 1/1000 of a pixel
  expect_true(all(abs(img_ext - expected_ext) < pixel_size / 1000))
})

test_that("georeference_img checks that there are enough GCPs", {
  img_file <- tempfile(fileext = ".jpg")
  jpeg::writeJPEG(array(0.5, dim = c(50, 100, 3)), img_file)
  gcp <- data.frame(
    id = 1:5,
    x = c(10, 90, 90, 10, 50),
    y = c(10, 10, 40, 40, 25),
    longitude = c(0, 8, 8, 0, 4.5),
    latitude = c(40, 40, 43, 43, 41.2)
  )
  expect_error(
    georeference_img(img_file, gcp,
      output_path = tempfile(), transform_method = "poly_2"
    ),
    "At least 6 GCPs are required for transform_method = \"poly_2\""
  )
  expect_error(
    georeference_img(img_file, gcp,
      output_path = tempfile(), transform_method = "poly_3"
    ),
    "At least 10 GCPs are required"
  )
  for (method in c("auto", "poly_1", "tps")) {
    expect_error(
      georeference_img(img_file, gcp[1:2, ],
        output_path = tempfile(), transform_method = method
      ),
      "At least 3 GCPs are required"
    )
  }
})
