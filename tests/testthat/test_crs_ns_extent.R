test_that("test ns extent", {
  # CONFORMAL
  ## Transverse Mercator
  expect_message(suggested_crs_ns_mercator <- suggest_crs(c(12, 24, 2, 22), distortion = "conformal"), 
                 "To reduce overall area distortion on the map")
  ref_proj4_ns_mercator <- "+proj=tmerc +lon_0=18 +datum=WGS84 +units=m +no_defs"
  ref_wkt_ns_mercator <- 'PROJCS["ProjWiz_Custom_Transverse_Mercator",
                                GEOGCS["GCS_WGS_1984",
                                       DATUM["D_WGS_1984",
                                             SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                       PRIMEM["Greenwich",0.0],
                                       UNIT["Degree",0.0174532925199433]],
                                PROJECTION["Transverse_Mercator"],
                                PARAMETER["False_Easting",0.0],
                                PARAMETER["False_Northing",0.0],
                                PARAMETER["Central_Meridian",18],
                                PARAMETER["Scale_Factor",1.0],
                                PARAMETER["Latitude_Of_Origin",0.0],
                                UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_ns_mercator$proj4, ref_proj4_ns_mercator)
  expect_true(sf::st_crs(suggested_crs_ns_mercator$wkt) == sf::st_crs(ref_wkt_ns_mercator))
  
  # EQUAL AREA
  ## Transverse cylindrical equal-area
  expect_message(suggested_crs_ns_equal_area <- suggest_crs(c(12, 24, 2, 22), distortion = "equal_area"),
                 "To reduce overall distortion on the map")
  ref_proj4_ns_equal_area <- "+proj=tcea +lon_0=18 +datum=WGS84 +units=m +no_defs"
  ref_wkt_ns_equal_area <- 'PROJCS["ProjWiz_Custom_Transverse_Cylindrical_Equal_Area",
                                  GEOGCS["GCS_WGS_1984",
                                         DATUM["D_WGS_1984",
                                               SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                         PRIMEM["Greenwich",0.0],
                                         UNIT["Degree",0.0174532925199433]],
                                  PROJECTION["Transverse_Cylindrical_Equal_Area"],
                                  PARAMETER["False_Easting",0.0],
                                  PARAMETER["False_Northing",0.0],
                                  PARAMETER["Central_Meridian",18],
                                  PARAMETER["Scale_Factor",1.0],
                                  PARAMETER["Latitude_Of_Origin",0.0],
                                  UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_ns_equal_area$proj4, ref_proj4_ns_equal_area)
  expect_true(sf::st_crs(suggested_crs_ns_equal_area$wkt) == sf::st_crs(ref_wkt_ns_equal_area))
  
})