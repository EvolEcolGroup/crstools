testthat::test_that("generate crs string correctly", {
  # test error for non-exising projectsion
  expect_error(crs_string("blah", NA, NA, NA, NA, 0, NA, "WGS84", "m"),
               "Projection not recognized.")
  # equal earth default with whole world
  eqearth_proj <- "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs"
  test_string <- crs_string("eqearth", NA, NA, NA, NA, 0, NA, "WGS84", "m")
  expect_equal(test_string$proj4, "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs")
  expect_equal(test_string$wkt, 'PROJCS["ProjWiz_Custom_Equal_Earth", GEOGCS["GCS_WGS_1984", DATUM["D_WGS_1984", SPHEROID["WGS_1984",6378137.0,298.257223563]], PRIMEM["Greenwich",0.0], UNIT["Degree",0.0174532925199433]], PROJECTION["Equal_Earth"], PARAMETER["False_Easting",0.0], PARAMETER["False_Northing",0.0], PARAMETER["Central_Meridian",0], UNIT["Meter",1.0]]"')
  # BUG we are missing the output from lon0=0 (central meridian in WKT)
  # create a correct stringa

})
