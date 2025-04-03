testthat::test_that("test whole world", {
  # EQUAL AREA
  # check Equal Earth projection
  suggested_crs <- suggest_crs(c(-180, 180, -90, 90), distortion = "equal_area")
  ref_proj4 <- "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt <- 'PROJCS["ProjWiz_Custom_Equal_Earth",
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
  
  expect_equal(suggested_crs$proj4, ref_proj4)
  expect_true(sf::st_crs(suggested_crs$wkt) == sf::st_crs(ref_wkt))
  
  whole_eqa_list <- suggest_crs(c(-180, 180, -90, 90), distortion = "equal_area", return_best = FALSE)
  expect_true(length(whole_eqa_list) == 6)
  
  # check Mollweide projection
  ref_proj4_moll <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_moll <- 'PROJCS["ProjWiz_Custom_Mollweide",
                         GEOGCS["GCS_WGS_1984",
                                DATUM["D_WGS_1984",
                                      SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                PRIMEM["Greenwich",0.0],
                                UNIT["Degree",0.0174532925199433]],
                         PROJECTION["Mollweide"],
                         PARAMETER["False_Easting",0.0],
                         PARAMETER["False_Northing",0.0],
                         PARAMETER["Central_Meridian",0],
                         UNIT["Meter",1.0]]'
  
  expect_equal(whole_eqa_list$moll$proj4, ref_proj4_moll)
  expect_true(sf::st_crs(whole_eqa_list$moll$wkt) == sf::st_crs(ref_wkt_moll))
  
  # check Hammer projection
  ref_proj4_hammer <- "+proj=hammer +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_hammer <- 'PROJCS["ProjWiz_Custom_Hammer_Aitoff",
                           GEOGCS["GCS_WGS_1984",
                                  DATUM["D_WGS_1984",
                                        SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                  PRIMEM["Greenwich",0.0],
                                  UNIT["Degree",0.0174532925199433]],
                           PROJECTION["Hammer_Aitoff"],
                           PARAMETER["False_Easting",0.0],
                           PARAMETER["False_Northing",0.0],
                           PARAMETER["Central_Meridian",0],
                           UNIT["Meter",1.0]]'
  
  expect_equal(whole_eqa_list$hammer$proj4, ref_proj4_hammer)
  expect_true(sf::st_crs(whole_eqa_list$hammer$wkt) == sf::st_crs(ref_wkt_hammer))
  
  # check Eckert IV projection
  ref_proj4_eck4 <- "+proj=eck4 +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_eck4 <- 'PROJCS["ProjWiz_Custom_Eckert_IV",
                           GEOGCS["GCS_WGS_1984",
                                  DATUM["D_WGS_1984",
                                        SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                  PRIMEM["Greenwich",0.0],
                                  UNIT["Degree",0.0174532925199433]],
                           PROJECTION["Eckert_IV"],
                           PARAMETER["False_Easting",0.0],
                           PARAMETER["False_Northing",0.0],
                           PARAMETER["Central_Meridian",0],
                           UNIT["Meter",1.0]]'
  
  expect_equal(whole_eqa_list$eck4$proj4, ref_proj4_eck4)
  expect_true(sf::st_crs(whole_eqa_list$eck4$wkt) == sf::st_crs(ref_wkt_eck4))
  
  # check Wagner IV projection
  ref_proj4_wag4 <- "+proj=wag4 +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_wag4 <- 'PROJCS["ProjWiz_Custom_Wagner_IV",
                         GEOGCS["GCS_WGS_1984",
                                DATUM["D_WGS_1984",
                                      SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                PRIMEM["Greenwich",0.0],
                                UNIT["Degree",0.0174532925199433]],
                         PROJECTION["Wagner_IV"],
                         PARAMETER["False_Easting",0.0],
                         PARAMETER["False_Northing",0.0],
                         PARAMETER["Central_Meridian",0],
                         UNIT["Meter",1.0]]'
  
  expect_equal(whole_eqa_list$wag4$proj4, ref_proj4_wag4)
  expect_true(sf::st_crs(whole_eqa_list$wag4$wkt) == sf::st_crs(ref_wkt_wag4))
  
  # check Wagner VII projection
  ref_proj4_wag7 <- "+proj=wag7 +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_wag7 <- 'PROJCS["ProjWiz_Custom_Wagner_VII",
                         GEOGCS["GCS_WGS_1984",
                                DATUM["D_WGS_1984",
                                      SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                PRIMEM["Greenwich",0.0],
                                UNIT["Degree",0.0174532925199433]],
                         PROJECTION["Wagner_VII"],
                         PARAMETER["False_Easting",0.0],
                         PARAMETER["False_Northing",0.0],
                         PARAMETER["Central_Meridian",0],
                         UNIT["Meter",1.0]]'
  
  expect_equal(whole_eqa_list$wag7$proj4, ref_proj4_wag7)
  expect_true(sf::st_crs(whole_eqa_list$wag7$wkt) == sf::st_crs(ref_wkt_wag7))
  
  # EQUIDISTANT
  # check for error if details of projection are missing
  expect_error(whole_equidist <- suggest_crs(c(-180, 180, -90, 90), distortion = "equidistant"),
               "`world_equidist` must be provided for equidistant world map projections")
  expect_error(whole_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                             distortion = "equidistant",
                                             world_equidist = list(blah = "blah")),
               "`world_equidistant` must be a list with a `prj` element")
  expect_error(whole_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                             distortion = "equidistant",
                                             world_equidist = "blah"),
               "`world_equidistant` must be a list with a `prj` element")  
  expect_error(whole_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                             distortion = "equidistant",
                                             world_equidist = list(prj = "polar", lng_central = 0)),
               "`world_equidistant` must contain a `pole` element")
  expect_error(whole_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                             distortion = "equidistant",
                                             world_equidist = list(prj = "polar", pole = -80, lng_central = 0)),
               "`pole` must be either 90 or -90")
  
  expect_error(whole_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                             distortion = "equidistant",
                                             world_equidist = list(prj = "polar", pole = -90)),
               "`world_equidistant` must contain a `lng_central` element")
  expect_error(whole_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                             distortion = "equidistant",
                                             world_equidist = list(prj = "oblique",
                                                                   lng_center = 0)),
               "`world_equidistant` must contain a `lat_center` element")
  expect_error(whole_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                             distortion = "equidistant",
                                             world_equidist = list(prj = "oblique",
                                                                   lat_center = 0)),
               "`world_equidistant` must contain a `lng_center` element")
  expect_error(whole_equidist <- two_points_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                                                    distortion = "equidistant",
                                                                    world_equidist = list(prj = "two_points",
                                                                                          lng1 = -117,
                                                                                          lat2 = 46,
                                                                                          lng2 = 16)),
               "`world_equidistant` must contain a `lat1` element")
  expect_error(whole_equidist <- two_points_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                                                    distortion = "equidistant",
                                                                    world_equidist = list(prj = "two_points",
                                                                                          lat1 = 34,
                                                                                          lat2 = 46,
                                                                                          lng2 = 16)),
               "`world_equidistant` must contain a `lng1` element")
  expect_error(whole_equidist <- two_points_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                                                    distortion = "equidistant",
                                                                    world_equidist = list(prj = "two_points",
                                                                                          lat1 = 34,
                                                                                          lng1 = -117,
                                                                                          lng2 = 16)),
               "`world_equidistant` must contain a `lat2` element")
  expect_error(whole_equidist <- two_points_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                                                    distortion = "equidistant",
                                                                    world_equidist = list(prj = "two_points",
                                                                                          lat1 = 34,
                                                                                          lng1 = -117,
                                                                                          lat2 = 46)),
               "`world_equidistant` must contain a `lng2` element")
  expect_error(whole_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                             distortion = "equidistant",
                                             world_equidist = list(prj = "blah", pole = -90, lng_central = 0)),
               "the `prj` element of world_equidistant` should be one of")
  
  
  
  
  # polar equidistant
  polar_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                distortion = "equidistant",
                                world_equidist = list(prj = "polar", pole = -90, lng_central = 0))
  ref_proj4_polar_eqd <- "+proj=aeqd +lon_0=0 +lat_0=-90 +datum=WGS84 +units=m +no_defs"
  ref_wkt_polar_eqd <- 'PROJCS["ProjWiz_Custom_Azimuthal_Equidistant",
 GEOGCS["GCS_WGS_1984",
  DATUM["D_WGS_1984",
   SPHEROID["WGS_1984",6378137.0,298.257223563]],
  PRIMEM["Greenwich",0.0],
  UNIT["Degree",0.0174532925199433]],
 PROJECTION["Azimuthal_Equidistant"],
 PARAMETER["False_Easting",0.0],
 PARAMETER["False_Northing",0.0],
 PARAMETER["Central_Meridian",0],
 PARAMETER["Latitude_Of_Origin",-90],
 UNIT["Meter",1.0]]'
  expect_equal(polar_equidist$proj4, ref_proj4_polar_eqd)
  expect_true(sf::st_crs(polar_equidist$wkt) == sf::st_crs(ref_wkt_polar_eqd))
  
  # Oblique Azimuthal equidistant
  oblique_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                  distortion = "equidistant",
                                  world_equidist = list(prj = "oblique",
                                                        lat_center = 0,
                                                        lng_center = 0))
  ref_proj4_oblique_eqd <- "+proj=aeqd +lon_0=0 +lat_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_oblique_eqd <- 'PROJCS["ProjWiz_Custom_Azimuthal_Equidistant",
                                GEOGCS["GCS_WGS_1984",
                                       DATUM["D_WGS_1984",
                                             SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                       PRIMEM["Greenwich",0.0],
                                       UNIT["Degree",0.0174532925199433]],
                                PROJECTION["Azimuthal_Equidistant"],
                                PARAMETER["False_Easting",0.0],
                                PARAMETER["False_Northing",0.0],
                                PARAMETER["Central_Meridian",0],
                                PARAMETER["Latitude_Of_Origin",0],
                                UNIT["Meter",1.0]]'
  expect_equal(oblique_equidist$proj4, ref_proj4_oblique_eqd)
  expect_true(sf::st_crs(oblique_equidist$wkt) == sf::st_crs(ref_wkt_oblique_eqd))
  
  # Two-points equidistant
  two_points_equidist <- suggest_crs(c(-180, 180, -90, 90),
                                     distortion = "equidistant",
                                     world_equidist = list(prj = "two_points",
                                                           lat1 = 34,
                                                           lng1 = -117,
                                                           lat2 = 46,
                                                           lng2 = 16))
  
  ref_proj4_two_points_eqd <- "+proj=tpeqd +lat_1=34 +lon_1=-117 +lat_2=46 +lon_2=16 +datum=WGS84 +units=m +no_defs"
  ref_wkt_two_points_eqd <- 'PROJCS["ProjWiz_Custom_Two_Point_Equidistant",
                                   GEOGCS["GCS_WGS_1984",
                                          DATUM["D_WGS_1984",
                                                SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                          PRIMEM["Greenwich",0.0],
                                          UNIT["Degree",0.0174532925199433]],
                                   PROJECTION["Two_Point_Equidistant"],
                                   PARAMETER["False_Easting",0.0],
                                   PARAMETER["False_Northing",0.0],
                                   PARAMETER["Latitude_Of_1st_Point",34],
                                   PARAMETER["Latitude_Of_2nd_Point",46],
                                   PARAMETER["Longitude_Of_1st_Point",-117],
                                   PARAMETER["Longitude_Of_2nd_Point",16],
                                   UNIT["Meter",1.0]]'
  expect_equal(two_points_equidist$proj4, ref_proj4_two_points_eqd)
  expect_true(sf::st_crs(two_points_equidist$wkt) == sf::st_crs(ref_wkt_two_points_eqd))
  
  # COMPROMISE 
  expect_message(whole_comp_list <- suggest_crs(c(-180, 180, -90, 90), 
                                                distortion = "compromise", 
                                                return_best = FALSE),
                 "Rectangular projections are not generally")
  expect_true(length(whole_comp_list) == 6)
  
  # check Robinson projection
  ref_proj4_robin <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_robin <- 'PROJCS["ProjWiz_Custom_Robinson",
                             GEOGCS["GCS_WGS_1984",
                                    DATUM["D_WGS_1984",
                                          SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                    PRIMEM["Greenwich",0.0],
                                    UNIT["Degree",0.0174532925199433]],
                             PROJECTION["Robinson"],
                             PARAMETER["False_Easting",0.0],
                             PARAMETER["False_Northing",0.0],
                             PARAMETER["Central_Meridian",0],
                             UNIT["Meter",1.0]]'
  expect_equal(whole_comp_list$robin$proj4, ref_proj4_robin)
  expect_true(sf::st_crs(whole_comp_list$robin$wkt) == sf::st_crs(ref_wkt_robin))
  
  # check Natural Earth projection
  ref_proj4_natearth <- "+proj=natearth +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_natearth <- 'PROJCS["ProjWiz_Custom_Natural_Earth",
                             GEOGCS["GCS_WGS_1984",
                                    DATUM["D_WGS_1984",
                                          SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                    PRIMEM["Greenwich",0.0],
                                    UNIT["Degree",0.0174532925199433]],
                             PROJECTION["Natural_Earth"],
                             PARAMETER["False_Easting",0.0],
                             PARAMETER["False_Northing",0.0],
                             PARAMETER["Central_Meridian",0],
                             UNIT["Meter",1.0]]'
  expect_equal(whole_comp_list$natearth$proj4, ref_proj4_natearth)
  expect_true(sf::st_crs(whole_comp_list$natearth$wkt) == sf::st_crs(ref_wkt_natearth))
  
  # check Winkel Tripel projection
  ref_proj4_wintri <- "+proj=wintri +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_wintri <- 'PROJCS["ProjWiz_Custom_Winkel_Tripel",
                           GEOGCS["GCS_WGS_1984",
                                  DATUM["D_WGS_1984",
                                        SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                  PRIMEM["Greenwich",0.0],
                                  UNIT["Degree",0.0174532925199433]],
                           PROJECTION["Winkel_Tripel"],
                           PARAMETER["False_Easting",0.0],
                           PARAMETER["False_Northing",0.0],
                           PARAMETER["Central_Meridian",0],
                           PARAMETER["Standard_Parallel_1",50.467],
                           UNIT["Meter",1.0]]'
  expect_equal(whole_comp_list$wintri$proj4, ref_proj4_wintri)
  expect_true(sf::st_crs(whole_comp_list$wintri$wkt) == sf::st_crs(ref_wkt_wintri))
  
  # check Patterson projection
  ref_proj4_patterson <- "+proj=patterson +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_patterson <- 'PROJCS["ProjWiz_Custom_Patterson",
                              GEOGCS["GCS_WGS_1984",
                                     DATUM["D_WGS_1984",
                                           SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                     PRIMEM["Greenwich",0.0],
                                     UNIT["Degree",0.0174532925199433]],
                              PROJECTION["Patterson"],
                              PARAMETER["False_Easting",0.0],
                              PARAMETER["False_Northing",0.0],
                              PARAMETER["Central_Meridian",0],
                              UNIT["Meter",1.0]]'
  expect_equal(whole_comp_list$patterson$proj4, ref_proj4_patterson)
  expect_true(sf::st_crs(whole_comp_list$patterson$wkt) == sf::st_crs(ref_wkt_patterson))
  
  # check Plate Carree projection
  ref_proj4_latlong <- "+proj=eqc +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_latlong <- 'PROJCS["ProjWiz_Custom_Plate_Carree",
                            GEOGCS["GCS_WGS_1984",
                                   DATUM["D_WGS_1984",
                                         SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                   PRIMEM["Greenwich",0.0],
                                   UNIT["Degree",0.0174532925199433]],
                            PROJECTION["Plate_Carree"],
                            PARAMETER["False_Easting",0.0],
                            PARAMETER["False_Northing",0.0],
                            PARAMETER["Central_Meridian",0],
                            UNIT["Meter",1.0]]'
  expect_equal(whole_comp_list$latlong$proj4, ref_proj4_latlong)
  expect_true(sf::st_crs(whole_comp_list$latlong$wkt) == sf::st_crs(ref_wkt_latlong))
  
  # check Miller cylindrical projection
  ref_proj4_mill <- "+proj=mill +lon_0=0 +datum=WGS84 +units=m +no_defs"
  ref_wkt_mill <- 'PROJCS["ProjWiz_Custom_Miller_Cylindrical",
                           GEOGCS["GCS_WGS_1984",
                                  DATUM["D_WGS_1984",
                                        SPHEROID["WGS_1984",6378137.0,298.257223563]],
                                  PRIMEM["Greenwich",0.0],
                                  UNIT["Degree",0.0174532925199433]],
                           PROJECTION["Miller_Cylindrical"],
                           PARAMETER["False_Easting",0.0],
                           PARAMETER["False_Northing",0.0],
                           PARAMETER["Central_Meridian",0],
                           UNIT["Meter",1.0]]'
  expect_equal(whole_comp_list$mill$proj4, ref_proj4_mill)
  expect_true(sf::st_crs(whole_comp_list$mill$wkt) == sf::st_crs(ref_wkt_mill))
  
  # CONFORMAL
  expect_error(whole_conf <- suggest_crs(c(-180, 180, -90, 90), distortion = "conformal"),
               "conformal is not available for maps covering the whole world")
  
})