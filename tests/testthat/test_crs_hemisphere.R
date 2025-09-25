# styler: off

test_that("test whole hemisphere", {
  # EQUAL AREA
  # check northern hemisphere
  suggested_crs_north_hem_eqa <- suggest_crs(c(-180, 180, 0, 90), distortion = "equal_area")
  ref_proj4_north_hem_eqa <- "+proj=laea +lon_0=0 +lat_0=45 +datum=WGS84 +units=m +no_defs"
  ref_wkt_north_hem_eqa <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal",
                                  GEOGCS["GCS_WGS_1984",
                                         DATUM["D_WGS_1984",
                                               SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                         PRIMEM["Greenwich",0.0],
                                         UNIT["Degree",0.0174532925199433]],
                                  PROJECTION["Lambert_Azimuthal_Equal_Area"],
                                  PARAMETER["False_Easting",0.0],
                                  PARAMETER["False_Northing",0.0],
                                  PARAMETER["Central_Meridian",0],
                                  PARAMETER["Latitude_Of_Origin",45],
                                  UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_north_hem_eqa$proj4, ref_proj4_north_hem_eqa)
  expect_true(sf::st_crs(suggested_crs_north_hem_eqa$wkt) == sf::st_crs(ref_wkt_north_hem_eqa))

  # check southern hemisphere
  suggested_crs_south_hem_eqa <- suggest_crs(c(-180, 180, -90, 0), distortion = "equal_area")
  ref_proj4_south_hem_eqa <- "+proj=laea +lon_0=0 +lat_0=-45 +datum=WGS84 +units=m +no_defs"
  ref_wkt_south_hem_eqa <- 'PROJCS["ProjWiz_Custom_Lambert_Azimuthal",
                                  GEOGCS["GCS_WGS_1984",
                                         DATUM["D_WGS_1984",
                                               SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                         PRIMEM["Greenwich",0.0],
                                         UNIT["Degree",0.0174532925199433]],
                                  PROJECTION["Lambert_Azimuthal_Equal_Area"],
                                  PARAMETER["False_Easting",0.0],
                                  PARAMETER["False_Northing",0.0],
                                  PARAMETER["Central_Meridian",0],
                                  PARAMETER["Latitude_Of_Origin",-45],
                                  UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_south_hem_eqa$proj4, ref_proj4_south_hem_eqa)
  expect_true(sf::st_crs(suggested_crs_south_hem_eqa$wkt) == sf::st_crs(ref_wkt_south_hem_eqa))

  # EQUIDISTANT
  # check northern hemisphere
  suggested_crs_north_hem_eqd <- suggest_crs(c(-180, 180, 0, 90), distortion = "equidistant")
  ref_proj4_north_hem_eqd <- "+proj=aeqd +lon_0=0 +lat_0=45 +datum=WGS84 +units=m +no_defs"
  ref_wkt_north_hem_eqd <- 'PROJCS["ProjWiz_Custom_Azimuthal_Equidistant",
                                  GEOGCS["GCS_WGS_1984",
                                         DATUM["D_WGS_1984",
                                               SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                         PRIMEM["Greenwich",0.0],
                                         UNIT["Degree",0.0174532925199433]],
                                  PROJECTION["Azimuthal_Equidistant"],
                                  PARAMETER["False_Easting",0.0],
                                  PARAMETER["False_Northing",0.0],
                                  PARAMETER["Central_Meridian",0],
                                  PARAMETER["Latitude_Of_Origin",45],
                                  UNIT["Meter",1.0]]'

  expect_equal(suggested_crs_north_hem_eqd$proj4, ref_proj4_north_hem_eqd)
  expect_true(sf::st_crs(suggested_crs_north_hem_eqd$wkt) == sf::st_crs(ref_wkt_north_hem_eqd))

  # check southern hemisphere
  suggested_crs_south_hem_eqd <- suggest_crs(c(-180, 180, -90, 0), distortion = "equidistant")
  ref_proj4_south_hem_eqd <- "+proj=aeqd +lon_0=0 +lat_0=-45 +datum=WGS84 +units=m +no_defs"
  ref_wkt_south_hem_eqd <- 'PROJCS["ProjWiz_Custom_Azimuthal_Equidistant",
                                  GEOGCS["GCS_WGS_1984",
                                         DATUM["D_WGS_1984",
                                               SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                         PRIMEM["Greenwich",0.0],
                                         UNIT["Degree",0.0174532925199433]],
                                  PROJECTION["Azimuthal_Equidistant"],
                                  PARAMETER["False_Easting",0.0],
                                  PARAMETER["False_Northing",0.0],
                                  PARAMETER["Central_Meridian",0],
                                  PARAMETER["Latitude_Of_Origin",-45],
                                  UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_south_hem_eqd$proj4, ref_proj4_south_hem_eqd)
  expect_true(sf::st_crs(suggested_crs_south_hem_eqd$wkt) == sf::st_crs(ref_wkt_south_hem_eqd))

  # COMPROMISE
  expect_error(suggested_crs_south_hem_comp <- suggest_crs(c(-180, 180, -90, 0), distortion = "compromise"),
               "compromise is not available for maps covering a whole hemisphere")

  # CONFORMAL
  expect_error(suggested_crs_south_hem_conf <- suggest_crs(c(-180, 180, -90, 0), distortion = "conformal"),
               "conformal is not available for maps covering a whole hemisphere")

})

test_that("test if area in tropics", {
  # equal area projection
  suggested_crs_trop_eqa <- suggest_crs(c(-180, 180, -22, 22), distortion = "equal_area")
  ref_proj4_trop_eqa <- "+proj=cea +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_trop_eqa <- 'PROJCS["ProjWiz_Custom_Cylindrical_Equal_Area",
                             GEOGCS["GCS_WGS_1984",
                                    DATUM["D_WGS_1984",
                                          SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                    PRIMEM["Greenwich",0.0],
                                    UNIT["Degree",0.0174532925199433]],
                             PROJECTION["Cylindrical_Equal_Area"],
                             PARAMETER["False_Easting",0.0],
                             PARAMETER["False_Northing",0.0],
                             PARAMETER["Central_Meridian",0],
                             PARAMETER["Standard_Parallel_1",0],
                             UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_trop_eqa$proj4, ref_proj4_trop_eqa)
  expect_true(sf::st_crs(suggested_crs_trop_eqa$wkt) == sf::st_crs(ref_wkt_trop_eqa))

  # conformal projection
  suggested_crs_trop_conf <- suggest_crs(c(-180, 180, -22, 22), distortion = "conformal")
  ref_proj4_trop_conf <- "+proj=merc +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_trop_conf <- 'PROJCS["ProjWiz_Custom_Mercator",
                              GEOGCS["GCS_WGS_1984",
                                     DATUM["D_WGS_1984",
                                           SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                     PRIMEM["Greenwich",0.0],
                                     UNIT["Degree",0.0174532925199433]],
                              PROJECTION["Mercator"],
                              PARAMETER["False_Easting",0.0],
                              PARAMETER["False_Northing",0.0],
                              PARAMETER["Central_Meridian",0],
                              PARAMETER["Standard_Parallel_1",0],
                              UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_trop_conf$proj4, ref_proj4_trop_conf)
  expect_true(sf::st_crs(suggested_crs_trop_conf$wkt) == sf::st_crs(ref_wkt_trop_conf))

  # equidistant projection
  suggested_crs_trop_eqd <- suggest_crs(c(-180, 180, -22, 22), distortion = "equidistant")
  ref_proj4_trop_eqd <- "+proj=eqc +lon_0=0 +lat_ts=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_trop_eqd <- 'PROJCS["ProjWiz_Custom_Equidistant_Cylindrical",
                             GEOGCS["GCS_WGS_1984",
                                    DATUM["D_WGS_1984",
                                          SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                    PRIMEM["Greenwich",0.0],
                                    UNIT["Degree",0.0174532925199433]],
                             PROJECTION["Equidistant_Cylindrical"],
                             PARAMETER["False_Easting",0.0],
                             PARAMETER["False_Northing",0.0],
                             PARAMETER["Central_Meridian",0],
                             PARAMETER["Standard_Parallel_1",0],
                             UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_trop_eqd$proj4, ref_proj4_trop_eqd)
  expect_true(sf::st_crs(suggested_crs_trop_eqd$wkt) == sf::st_crs(ref_wkt_trop_eqd))

})
