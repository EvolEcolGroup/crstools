testthat::test_that("test_crs_wizard", {
  ############################################
  # The whole world
  ############################################
  whole_eqa <- crs_wizard(c(-180,180,-90, 90), distortion = "equal_area")
  expect_equal(whole_eqa$proj4, "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs")
  ref_wkt<- 'PROJCS["ProjWiz_Custom_Equal_Earth",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Equal_Earth"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",0],
 UNIT["Meter",1.0]]'
  expect_true(sf::st_crs(whole_eqa$wkt) == sf::st_crs(ref_wkt))
  
  whole_eqa_list <- crs_wizard(c(-180,180,-90, 90), distortion = "equal_area",return_best = FALSE)
  expect_true(length(whole_eqa_list) == 6)
  # TODO check at least one of these projections against proj wizard
  
  # check for correct coordinates 
  expect_error(crs_wizard(c(-180, 180, 90, -90), distortion = "equal_area", return_best = FALSE), 
               "lat_min must be smaller than lat_max")
  expect_error(crs_wizard(c(180, -180, -90, 90), distortion = "equal_area", return_best = FALSE),
               "lon_min must be smaller than lon_max")
  
  ############################################
  # A hemisphere
  ############################################

  
  
  
})
