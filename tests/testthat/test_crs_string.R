testthat::test_that("generate crs string correctly", {
  # test error for non-exising projectsion
  expect_error(crs_string("blah", NA, NA, NA, NA, 0, NA, "WGS84", "m"),
               "Projection not recognized.")
  test_string <- crs_string("eqearth", NA, NA, NA, NA, 0, NA, "WGS84", "m")
  # BUG we are missing the output from lon0=0 (central meridian in WKT)
  # create a correct stringa

})
