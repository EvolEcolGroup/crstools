test_that("test square format", {
  # EQUAL AREA 
  suggested_crs_square_eqa <- suggest_crs(c(-22, 57, -37, 36), distortion = "equal_area")
  ref_proj4_square_eqa <- "+proj=laea +lon_0=17.5 +lat_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_square_eqa <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal",
                               GEOGCS["GCS_WGS_1984",
                                      DATUM["D_WGS_1984",
                                            SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                      PRIMEM["Greenwich",0.0],
                                      UNIT["Degree",0.0174532925199433]],
                               PROJECTION["Lambert_Azimuthal_Equal_Area"],
                               PARAMETER["False_Easting",0.0],
                               PARAMETER["False_Northing",0.0],
                               PARAMETER["Central_Meridian",17.5],
                               PARAMETER["Latitude_Of_Origin",0],
                               UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_square_eqa$proj4, ref_proj4_square_eqa)
  expect_true(sf::st_crs(suggested_crs_square_eqa$wkt) == sf::st_crs(ref_wkt_square_eqa))
  
  # Polar Lambert (north)
  expect_message(suggested_crs_square_eqa_polar <- suggest_crs(c(85, 109, 77, 82), distortion = "equal_area"),
                 "For maps at this scale")
  ref_proj4_square_eqa_polar <- "+proj=laea +lon_0=97 +lat_0=90 +datum=WGS84 +units=m +no_defs"
  ref_wkt_square_eqa_polar <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal",
                                     GEOGCS["GCS_WGS_1984",
                                            DATUM["D_WGS_1984",
                                                  SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                            PRIMEM["Greenwich",0.0],
                                            UNIT["Degree",0.0174532925199433]],
                                     PROJECTION["Lambert_Azimuthal_Equal_Area"],
                                     PARAMETER["False_Easting",0.0],
                                     PARAMETER["False_Northing",0.0],
                                     PARAMETER["Central_Meridian",97],
                                     PARAMETER["Latitude_Of_Origin",90],
                                     UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_square_eqa_polar$proj4, ref_proj4_square_eqa_polar)
  expect_true(sf::st_crs(suggested_crs_square_eqa_polar$wkt) == sf::st_crs(ref_wkt_square_eqa_polar))
  
  # Polar Lambert (south)
  suggested_crs_square_conf_eqa_polar <- suggest_crs(c(-56, -39, -81, -77), distortion = "equal_area")
  ref_proj4_square_conf_eqa_polar <- "+proj=laea +lon_0=-47.5 +lat_0=-90 +datum=WGS84 +units=m +no_defs"
  ref_wkt_square_conf_eqa_polar <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal",
                                         GEOGCS["GCS_WGS_1984",
                                                DATUM["D_WGS_1984",
                                                      SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                                PRIMEM["Greenwich",0.0],
                                                UNIT["Degree",0.0174532925199433]],
                                         PROJECTION["Lambert_Azimuthal_Equal_Area"],
                                         PARAMETER["False_Easting",0.0],
                                         PARAMETER["False_Northing",0.0],
                                         PARAMETER["Central_Meridian",-47.5],
                                         PARAMETER["Latitude_Of_Origin",-90],
                                         UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_square_conf_eqa_polar$proj4, ref_proj4_square_conf_eqa_polar)
  expect_true(sf::st_crs(suggested_crs_square_conf_eqa_polar$wkt) == sf::st_crs(ref_wkt_square_conf_eqa_polar))
  
  # Oblique Lambert
  suggested_crs_square_eqa_obl <- suggest_crs(c(-12, 2, 49, 59), distortion = "equal_area")
  ref_proj4_square_eqa_obl <- "+proj=laea +lon_0=-5 +lat_0=54 +datum=WGS84 +units=m +no_defs"
  ref_wkt_square_eqa_obl <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal",
                                   GEOGCS["GCS_WGS_1984",
                                          DATUM["D_WGS_1984",
                                                SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                          PRIMEM["Greenwich",0.0],
                                          UNIT["Degree",0.0174532925199433]],
                                   PROJECTION["Lambert_Azimuthal_Equal_Area"],
                                   PARAMETER["False_Easting",0.0],
                                   PARAMETER["False_Northing",0.0],
                                   PARAMETER["Central_Meridian",-5],
                                   PARAMETER["Latitude_Of_Origin",54],
                                   UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_square_eqa_obl$proj4, ref_proj4_square_eqa_obl)
  expect_true(sf::st_crs(suggested_crs_square_eqa_obl$wkt) == sf::st_crs(ref_wkt_square_eqa_obl))
  
  # CONFORMAL
  expect_message(suggested_crs_square_conf <- suggest_crs(c(-22, 57, -37, 36),
                                                          distortion = "conformal"),
                 "To reduce overall area")
  ref_proj4_square_conf <- "+proj=stere +lon_0=17.5 +lat_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_square_conf <- 'PROJCS["ProjWiz_Custom_Stereographic",
                                GEOGCS["GCS_WGS_1984",
                                       DATUM["D_WGS_1984",
                                             SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                       PRIMEM["Greenwich",0.0],
                                       UNIT["Degree",0.0174532925199433]],
                                PROJECTION["Stereographic"],
                                PARAMETER["False_Easting",0.0],
                                PARAMETER["False_Northing",0.0],
                                PARAMETER["Central_Meridian",17.5],
                                PARAMETER["Scale_Factor",1.0],
                                PARAMETER["Latitude_Of_Origin",0],
                                UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_square_conf$proj4, ref_proj4_square_conf)
  expect_true(sf::st_crs(suggested_crs_square_conf$wkt) == sf::st_crs(ref_wkt_square_conf))
  
  # Oblique stereographic
  suggested_crs_square_conf_obl_ste <- suggest_crs(c(-25, -13, 62, 67), distortion = "conformal")
  ref_proj4_square_conf_obl_ste <- "+proj=stere +lon_0=-19 +lat_0=64.5 +datum=WGS84 +units=m +no_defs"
  ref_wkt_square_conf_obl_ste <- 'PROJCS["ProjWiz_Custom_Stereographic",
                                        GEOGCS["GCS_WGS_1984",
                                               DATUM["D_WGS_1984",
                                                     SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                               PRIMEM["Greenwich",0.0],
                                               UNIT["Degree",0.0174532925199433]],
                                        PROJECTION["Stereographic"],
                                        PARAMETER["False_Easting",0.0],
                                        PARAMETER["False_Northing",0.0],
                                        PARAMETER["Central_Meridian",-19],
                                        PARAMETER["Scale_Factor",1.0],
                                        PARAMETER["Latitude_Of_Origin",64.5],
                                        UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_square_conf_obl_ste$proj4, ref_proj4_square_conf_obl_ste)
  expect_true(sf::st_crs(suggested_crs_square_conf_obl_ste$wkt) == sf::st_crs(ref_wkt_square_conf_obl_ste))
  
  # Polar stereographic (north)
  suggested_crs_square_conf_pol_ste <- suggest_crs(c(85, 109, 77, 82), distortion = "conformal")
  ref_proj4_square_conf_pol_ste <- "+proj=stere +lon_0=97 +lat_0=90 +datum=WGS84 +units=m +no_defs"
  ref_wkt_square_conf_pol_ste <- 'PROJCS["ProjWiz_Custom_Stereographic",
                                        GEOGCS["GCS_WGS_1984",
                                               DATUM["D_WGS_1984",
                                                     SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                               PRIMEM["Greenwich",0.0],
                                               UNIT["Degree",0.0174532925199433]],
                                        PROJECTION["Stereographic"],
                                        PARAMETER["False_Easting",0.0],
                                        PARAMETER["False_Northing",0.0],
                                        PARAMETER["Central_Meridian",97],
                                        PARAMETER["Scale_Factor",1.0],
                                        PARAMETER["Latitude_Of_Origin",90],
                                        UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_square_conf_pol_ste$proj4, ref_proj4_square_conf_pol_ste)
  expect_true(sf::st_crs(suggested_crs_square_conf_pol_ste$wkt) == sf::st_crs(ref_wkt_square_conf_pol_ste))
  
  # Polar stereographic (south)
  suggested_crs_square_conf_pol_ste <- suggest_crs(c(-56, -39, -81, -77), distortion = "conformal")
  ref_proj4_square_conf_pol_ste <- "+proj=stere +lon_0=-47.5 +lat_0=-90 +datum=WGS84 +units=m +no_defs"
  ref_wkt_square_conf_pol_ste <- 'PROJCS["ProjWiz_Custom_Stereographic",
                                        GEOGCS["GCS_WGS_1984",
                                               DATUM["D_WGS_1984",
                                                     SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                               PRIMEM["Greenwich",0.0],
                                               UNIT["Degree",0.0174532925199433]],
                                        PROJECTION["Stereographic"],
                                        PARAMETER["False_Easting",0.0],
                                        PARAMETER["False_Northing",0.0],
                                        PARAMETER["Central_Meridian",-47.5],
                                        PARAMETER["Scale_Factor",1.0],
                                        PARAMETER["Latitude_Of_Origin",-90],
                                        UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_square_conf_pol_ste$proj4, ref_proj4_square_conf_pol_ste)
  expect_true(sf::st_crs(suggested_crs_square_conf_pol_ste$wkt) == sf::st_crs(ref_wkt_square_conf_pol_ste))
  
  # Oblique stereographic
  suggested_crs_square_obl_ste <- suggest_crs(c(-12, 2, 49, 59), distortion = "conformal")
  ref_proj4_square_obl_ste <- "+proj=stere +lon_0=-5 +lat_0=54 +datum=WGS84 +units=m +no_defs"
  ref_wkt_square_obl_ste <- 'PROJCS["ProjWiz_Custom_Stereographic",
                                   GEOGCS["GCS_WGS_1984",
                                          DATUM["D_WGS_1984",
                                                SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                          PRIMEM["Greenwich",0.0],
                                          UNIT["Degree",0.0174532925199433]],
                                   PROJECTION["Stereographic"],
                                   PARAMETER["False_Easting",0.0],
                                   PARAMETER["False_Northing",0.0],
                                   PARAMETER["Central_Meridian",-5],
                                   PARAMETER["Scale_Factor",1.0],
                                   PARAMETER["Latitude_Of_Origin",54],
                                   UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_square_obl_ste$proj4, ref_proj4_square_obl_ste)
  expect_true(sf::st_crs(suggested_crs_square_obl_ste$wkt) == sf::st_crs(ref_wkt_square_obl_ste))
  
  
  # EQUIDISTANT
  suggested_crs_square_eqd <- suggest_crs(c(-22, 57, -37, 36), distortion = "equidistant")
  ref_proj4_square_eqd <- "+proj=eqc +lon_0=17.5 +lat_ts=18.5 +datum=WGS84 +units=m +no_defs"
  ref_wkt_square_eqd <- 'PROJCS["ProjWiz_Custom_Equidistant_Cylindrical",
                               GEOGCS["GCS_WGS_1984",
                                      DATUM["D_WGS_1984",
                                            SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                      PRIMEM["Greenwich",0.0],
                                      UNIT["Degree",0.0174532925199433]],
                               PROJECTION["Equidistant_Cylindrical"],
                               PARAMETER["False_Easting",0.0],
                               PARAMETER["False_Northing",0.0],
                               PARAMETER["Central_Meridian",17.5],
                               PARAMETER["Standard_Parallel_1",18.5],
                               UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_square_eqd$proj4, ref_proj4_square_eqd)
  expect_true(sf::st_crs(suggested_crs_square_eqd$wkt) == sf::st_crs(ref_wkt_square_eqd))
  
  # COMPROMISE
  expect_error(suggested_crs_square_comp <- suggest_crs(c(-22, 57, -37, 36), distortion = "compromise"),
               "compromise is not available for maps focussing on a single continent")
  
})  
