test_that("test ew extent", {
  # CONFORMAL
  # conic projection 
  expect_message(suggested_crs_ew_conic <- suggest_crs(c(25, 76, 48, 56), distortion = "conformal"),
                 "Conformal projection for regional maps")
  ref_proj4_ew_conic <- "+proj=lcc +lon_0=50.5 +lat_1=49.3333333 +lat_2=54.6666667 +lat_0=52 +datum=WGS84 +units=m +no_defs"
  ref_wkt_ew_conic <- 'PROJCS["ProjWiz_Custom_Lambert_Conformal_Conic",
                             GEOGCS["GCS_WGS_1984",
                                    DATUM["D_WGS_1984",
                                          SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                    PRIMEM["Greenwich",0.0],
                                    UNIT["Degree",0.0174532925199433]],
                             PROJECTION["Lambert_Conformal_Conic"],
                             PARAMETER["False_Easting",0.0],
                             PARAMETER["False_Northing",0.0],
                             PARAMETER["Central_Meridian",50.5],
                             PARAMETER["Standard_Parallel_1",49.3333333],
                             PARAMETER["Standard_Parallel_2",54.6666667],
                             PARAMETER["Latitude_Of_Origin",52],
                             UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_ew_conic$proj4, ref_proj4_ew_conic)
  expect_true(sf::st_crs(suggested_crs_ew_conic$wkt) == sf::st_crs(ref_wkt_ew_conic))
  
  # Mercator projection
  suggested_crs_ew_mercator <- suggest_crs(c(-13, 37, 8, 16), distortion = "conformal")
  ref_proj4_ew_mercator <- "+proj=merc +lon_0=12 +lat_ts=12 +datum=WGS84 +units=m +no_defs"
  ref_wkt_ew_mercator <- 'PROJCS["ProjWiz_Custom_Mercator",
                                GEOGCS["GCS_WGS_1984",
                                       DATUM["D_WGS_1984",
                                             SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                       PRIMEM["Greenwich",0.0],
                                       UNIT["Degree",0.0174532925199433]],
                                PROJECTION["Mercator"],
                                PARAMETER["False_Easting",0.0],
                                PARAMETER["False_Northing",0.0],
                                PARAMETER["Central_Meridian",12],
                                PARAMETER["Standard_Parallel_1",12],
                                UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_ew_mercator$proj4, ref_proj4_ew_mercator)
  expect_true(sf::st_crs(suggested_crs_ew_mercator$wkt) == sf::st_crs(ref_wkt_ew_mercator))
  
  # Lambert conformal 
  expect_message(suggested_crs_ew_lcc <- suggest_crs(c(-69, -48, -26, -19), distortion = "conformal"),
                 "For maps at this scale")
  ref_proj4_ew_lcc <- "+proj=lcc +lon_0=-58.5 +lat_1=-24.8333333 +lat_2=-20.1666667 +lat_0=-22.5 +datum=WGS84 +units=m +no_defs"
  ref_wkt_ew_lcc <- 'PROJCS["ProjWiz_Custom_Lambert_Conformal_Conic",
                           GEOGCS["GCS_WGS_1984",
                                  DATUM["D_WGS_1984",
                                        SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                  PRIMEM["Greenwich",0.0],
                                  UNIT["Degree",0.0174532925199433]],
                           PROJECTION["Lambert_Conformal_Conic"],
                           PARAMETER["False_Easting",0.0],
                           PARAMETER["False_Northing",0.0],
                           PARAMETER["Central_Meridian",-58.5],
                           PARAMETER["Standard_Parallel_1",-24.8333333],
                           PARAMETER["Standard_Parallel_2",-20.1666667],
                           PARAMETER["Latitude_Of_Origin",-22.5],
                           UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_ew_lcc$proj4, ref_proj4_ew_lcc)
  expect_true(sf::st_crs(suggested_crs_ew_lcc$wkt) == sf::st_crs(ref_wkt_ew_lcc))
  
  # Polar stereographic
  suggested_crs_ew_ps <- suggest_crs(c(6, 35, 75, 81), distortion = "conformal")
  ref_proj4_ew_ps <- "+proj=stere +lon_0=20.5 +lat_0=90 +datum=WGS84 +units=m +no_defs"
  ref_wkt_ew_ps <- 'PROJCS["ProjWiz_Custom_Stereographic",
                          GEOGCS["GCS_WGS_1984",
                                 DATUM["D_WGS_1984",
                                       SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                 PRIMEM["Greenwich",0.0],
                                 UNIT["Degree",0.0174532925199433]],
                          PROJECTION["Stereographic"],
                          PARAMETER["False_Easting",0.0],
                          PARAMETER["False_Northing",0.0],
                          PARAMETER["Central_Meridian",20.5],
                          PARAMETER["Scale_Factor",1.0],
                          PARAMETER["Latitude_Of_Origin",90],
                          UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_ew_ps$proj4, ref_proj4_ew_ps)
  expect_true(sf::st_crs(suggested_crs_ew_ps$wkt) == sf::st_crs(ref_wkt_ew_ps))
  
  # EQUAL AREA
  # Albers equal area
  expect_message(suggested_crs_ew_eql <- suggest_crs(c(25, 76, 48, 56), distortion = "equal_area"),
                 "Equal-area projection for regional maps with an east-west extent")
  ref_proj4_ew_eql <- "+proj=aea +lon_0=50.5 +lat_1=49.3333333 +lat_2=54.6666667 +lat_0=52 +datum=WGS84 +units=m +no_defs"
  ref_wkt_ew_eql <- 'PROJCS["ProjWiz_Custom_Albers",
                             GEOGCS["GCS_WGS_1984",
                                    DATUM["D_WGS_1984",
                                          SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                    PRIMEM["Greenwich",0.0],
                                    UNIT["Degree",0.0174532925199433]],
                             PROJECTION["Albers"],
                             PARAMETER["False_Easting",0.0],
                             PARAMETER["False_Northing",0.0],
                             PARAMETER["Central_Meridian",50.5],
                             PARAMETER["Standard_Parallel_1",49.3333333],
                             PARAMETER["Standard_Parallel_2",54.6666667],
                             PARAMETER["Latitude_Of_Origin",52],
                             UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_ew_eql$proj4, ref_proj4_ew_eql)
  expect_true(sf::st_crs(suggested_crs_ew_eql$wkt) == sf::st_crs(ref_wkt_ew_eql))
  
  # Polar Lambert equal area
  expect_message(suggested_crs_ew_pole <- suggest_crs(c(6, 35, 75, 81), distortion = "equal_area"),
                 "For maps at this scale")
  ref_proj4_ew_pole <- "+proj=laea +lon_0=20.5 +lat_0=90 +datum=WGS84 +units=m +no_defs"
  ref_wkt_ew_pole <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal",
                            GEOGCS["GCS_WGS_1984",
                                   DATUM["D_WGS_1984",
                                         SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                   PRIMEM["Greenwich",0.0],
                                   UNIT["Degree",0.0174532925199433]],
                            PROJECTION["Lambert_Azimuthal_Equal_Area"],
                            PARAMETER["False_Easting",0.0],
                            PARAMETER["False_Northing",0.0],
                            PARAMETER["Central_Meridian",20.5],
                            PARAMETER["Latitude_Of_Origin",90],
                            UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_ew_pole$proj4, ref_proj4_ew_pole)
  expect_true(sf::st_crs(suggested_crs_ew_pole$wkt) == sf::st_crs(ref_wkt_ew_pole))
  
})
