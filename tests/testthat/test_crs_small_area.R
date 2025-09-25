# styler: off

test_that("test small area", {
  # EQUIDISTANT
  # Azimuthal equidistant north
  expect_message(
    suggested_crs_small_eqd_north <- suggest_crs(c(18, 27, 79, 80), distortion = "equidistant"),
    "For maps at this scale"
  )
  ref_proj4_small_eqd_north <- "+proj=aeqd +lon_0=22.5 +lat_0=90 +datum=WGS84 +units=m +no_defs"
  ref_wkt_small_eqd_north <- 'PROJCS["ProjWiz_Custom_Azimuthal_Equidistant",
                                    GEOGCS["GCS_WGS_1984",
                                           DATUM["D_WGS_1984",
                                                 SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                           PRIMEM["Greenwich",0.0],
                                           UNIT["Degree",0.0174532925199433]],
                                    PROJECTION["Azimuthal_Equidistant"],
                                    PARAMETER["False_Easting",0.0],
                                    PARAMETER["False_Northing",0.0],
                                    PARAMETER["Central_Meridian",22.5],
                                    PARAMETER["Latitude_Of_Origin",90],
                                    UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_small_eqd_north$proj4, ref_proj4_small_eqd_north)
  expect_true(sf::st_crs(suggested_crs_small_eqd_north$wkt) == sf::st_crs(ref_wkt_small_eqd_north))

  # Azimuthal equidistant south
  expect_message(
    suggested_crs_small_eqd_south <- suggest_crs(c(-81, -73, -80, -79), distortion = "equidistant"),
    "For maps at this scale"
  )
  ref_proj4_small_eqd_south <- "+proj=aeqd +lon_0=-77 +lat_0=-90 +datum=WGS84 +units=m +no_defs"
  ref_wkt_small_eqd_south <- 'PROJCS["ProjWiz_Custom_Azimuthal_Equidistant",
                                    GEOGCS["GCS_WGS_1984",
                                           DATUM["D_WGS_1984",
                                                 SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                           PRIMEM["Greenwich",0.0],
                                           UNIT["Degree",0.0174532925199433]],
                                    PROJECTION["Azimuthal_Equidistant"],
                                    PARAMETER["False_Easting",0.0],
                                    PARAMETER["False_Northing",0.0],
                                    PARAMETER["Central_Meridian",-77],
                                    PARAMETER["Latitude_Of_Origin",-90],
                                    UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_small_eqd_south$proj4, ref_proj4_small_eqd_south)
  expect_true(sf::st_crs(suggested_crs_small_eqd_south$wkt) == sf::st_crs(ref_wkt_small_eqd_south))

  # Cassini
  expect_message(
    suggested_crs_small_cassini <- suggest_crs(c(30, 36, 25, 35), distortion = "equidistant"),
    "For maps at this scale"
  )
  ref_proj4_small_cassini <- "+proj=cass +lon_0=33 +datum=WGS84 +units=m +no_defs"
  ref_wkt_small_cassini <- 'PROJCS["ProjWiz_Custom_Cassini",
                                  GEOGCS["GCS_WGS_1984",
                                         DATUM["D_WGS_1984",
                                               SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                         PRIMEM["Greenwich",0.0],
                                         UNIT["Degree",0.0174532925199433]],
                                  PROJECTION["Cassini"],
                                  PARAMETER["False_Easting",0.0],
                                  PARAMETER["False_Northing",0.0],
                                  PARAMETER["Central_Meridian",33],
                                  PARAMETER["Scale_Factor",1.0],
                                  PARAMETER["Latitude_Of_Origin",0.0],
                                  UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_small_cassini$proj4, ref_proj4_small_cassini)
  expect_true(sf::st_crs(suggested_crs_small_cassini$wkt) == sf::st_crs(ref_wkt_small_cassini))

  expect_message(
    suggested_crs_small_eqd <- suggest_crs(c(24, 34, -23, -14), distortion = "equidistant", return_best = FALSE),
    "For maps at this scale"
  )
  # check Equidistant conic
  ref_proj4_small_eqdc <- "+proj=eqdc +lon_0=29 +lat_1=-21.5 +lat_2=-15.5 +lat_0=-18.5 +datum=WGS84 +units=m +no_defs"
  ref_wkt_small_eqdc <- 'PROJCS["ProjWiz_Custom_Equidistant_Conic",
                               GEOGCS["GCS_WGS_1984",
                                      DATUM["D_WGS_1984",
                                            SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                      PRIMEM["Greenwich",0.0],
                                      UNIT["Degree",0.0174532925199433]],
                               PROJECTION["Equidistant_Conic"],
                               PARAMETER["False_Easting",0.0],
                               PARAMETER["False_Northing",0.0],
                               PARAMETER["Central_Meridian",29],
                               PARAMETER["Standard_Parallel_1",-21.5],
                               PARAMETER["Standard_Parallel_2",-15.5],
                               PARAMETER["Latitude_Of_Origin",-18.5],
                               UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_small_eqd$eqdc$proj4, ref_proj4_small_eqdc)
  expect_true(sf::st_crs(suggested_crs_small_eqd$eqdc$wkt) == sf::st_crs(ref_wkt_small_eqdc))

  # check Oblique azimuthal
  ref_proj4_small_aeqd <- "+proj=aeqd +lon_0=29 +lat_0=-18.5 +datum=WGS84 +units=m +no_defs"
  ref_wkt_small_aeqd <- 'PROJCS["ProjWiz_Custom_Azimuthal_Equidistant",
                               GEOGCS["GCS_WGS_1984",
                                      DATUM["D_WGS_1984",
                                            SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                      PRIMEM["Greenwich",0.0],
                                      UNIT["Degree",0.0174532925199433]],
                               PROJECTION["Azimuthal_Equidistant"],
                               PARAMETER["False_Easting",0.0],
                               PARAMETER["False_Northing",0.0],
                               PARAMETER["Central_Meridian",29],
                               PARAMETER["Latitude_Of_Origin",-18.5],
                               UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_small_eqd$aeqd$proj4, ref_proj4_small_aeqd)
  expect_true(sf::st_crs(suggested_crs_small_eqd$aeqd$wkt) == sf::st_crs(ref_wkt_small_aeqd))

  # CONFORMAL
  ## Polar stereographic
  expect_message(
    suggested_crs_small_conf_pol <- suggest_crs(c(-32, -23, 84, 85), distortion = "conformal"),
    "For maps at this scale"
  )
  ref_proj4_small_conf_pol <- "+proj=stere +lon_0=-27.5 +lat_0=90 +k_0=0.994 +datum=WGS84 +units=m +no_defs"
  ref_wkt_small_conf_pol <- 'PROJCS["ProjWiz_Custom_Stereographic",
                                   GEOGCS["GCS_WGS_1984",
                                          DATUM["D_WGS_1984",
                                                SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                          PRIMEM["Greenwich",0.0],
                                          UNIT["Degree",0.0174532925199433]],
                                   PROJECTION["Stereographic"],
                                   PARAMETER["False_Easting",0.0],
                                   PARAMETER["False_Northing",0.0],
                                   PARAMETER["Central_Meridian",-27.5],
                                   PARAMETER["Scale_Factor",0.994],
                                   PARAMETER["Latitude_Of_Origin",90],
                                   UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_small_conf_pol$proj4, ref_proj4_small_conf_pol)
  expect_true(sf::st_crs(suggested_crs_small_conf_pol$wkt) == sf::st_crs(ref_wkt_small_conf_pol))

  ## Transverse Mercator (scale factor 0.9996)
  expect_message(
    suggested_crs_small_conf_tm <- suggest_crs(c(27, 32, 10, 20), distortion = "conformal"),
    "For maps at this scale"
  )
  ref_proj4_small_conf_tm <- "+proj=tmerc +x_0=500000 +lon_0=29.5 +k_0=0.9996 +datum=WGS84 +units=m +no_defs"
  ref_wkt_small_conf_tm <- 'PROJCS["ProjWiz_Custom_Transverse_Mercator",
                                  GEOGCS["GCS_WGS_1984",
                                         DATUM["D_WGS_1984",
                                               SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                         PRIMEM["Greenwich",0.0],
                                         UNIT["Degree",0.0174532925199433]],
                                  PROJECTION["Transverse_Mercator"],
                                  PARAMETER["False_Easting",500000],
                                  PARAMETER["False_Northing",0.0],
                                  PARAMETER["Central_Meridian",29.5],
                                  PARAMETER["Scale_Factor",0.9996],
                                  PARAMETER["Latitude_Of_Origin",0.0],
                                  UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_small_conf_tm$proj4, ref_proj4_small_conf_tm)
  expect_true(sf::st_crs(suggested_crs_small_conf_tm$wkt) == sf::st_crs(ref_wkt_small_conf_tm))

  ## Transverse Mercator (scale factor 0.9999)
  expect_message(
    suggested_crs_small_conf_tm <- suggest_crs(c(32, 34, 10, 20), distortion = "conformal"),
    "For maps at this scale"
  )
  ref_proj4_small_conf_tm <- "+proj=tmerc +x_0=500000 +lon_0=33 +k_0=0.9999 +datum=WGS84 +units=m +no_defs"
  ref_wkt_small_conf_tm <- 'PROJCS["ProjWiz_Custom_Transverse_Mercator",
                                  GEOGCS["GCS_WGS_1984",
                                         DATUM["D_WGS_1984",
                                               SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                         PRIMEM["Greenwich",0.0],
                                         UNIT["Degree",0.0174532925199433]],
                                  PROJECTION["Transverse_Mercator"],
                                  PARAMETER["False_Easting",500000],
                                  PARAMETER["False_Northing",0.0],
                                  PARAMETER["Central_Meridian",33],
                                  PARAMETER["Scale_Factor",0.9999],
                                  PARAMETER["Latitude_Of_Origin",0.0],
                                  UNIT["Meter",1.0]]'
  expect_equal(suggested_crs_small_conf_tm$proj4, ref_proj4_small_conf_tm)
  expect_true(sf::st_crs(suggested_crs_small_conf_tm$wkt) == sf::st_crs(ref_wkt_small_conf_tm))
})
