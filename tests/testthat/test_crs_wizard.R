testthat::test_that("test input sanity", {
  # check if input is not an expected class
  site <- data.frame(latitude = c(11,8),
                     longitude = c(5,9))
  expect_error(crs_wizard(site, distortion = "equal_area"), 
               "x must be a vector, a SpatExtent object or a SpatRaster object")
  
  # check for correct coordinates
  expect_error(
    crs_wizard(c(-180, 180, 90, -90), distortion = "equal_area", return_best = FALSE),
    "lat_min must be smaller than lat_max"
  )
  expect_error(
    crs_wizard(c(180, -180, -90, 90), distortion = "equal_area", return_best = FALSE),
    "lon_min must be smaller than lon_max"
  )
  expect_error(
    crs_wizard(c(-180, 180, -91, 90), distortion = "equal_area"),
    "Latitude values must be between -90 and 90"
  )
})

testthat::test_that("test whole world", {
  # EQUAL AREA
  # check Equal Earth projection
  suggested_crs <- crs_wizard(c(-180, 180, -90, 90), distortion = "equal_area")
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

  whole_eqa_list <- crs_wizard(c(-180, 180, -90, 90), distortion = "equal_area", return_best = FALSE)
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
  
  #@TODO: need to add test for EQUIDISTANT as for the moment the function does 
  # not work with the whole world "equidistant for the whole 
  # world not implemented yet"
  
  # COMPROMISE 
  whole_comp_list <- crs_wizard(c(-180, 180, -90, 90), distortion = "compromise", return_best = FALSE)
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
  expect_error(whole_conf <- crs_wizard(c(-180, 180, -90, 90), distortion = "conformal"),
                 "conformal is not available for maps covering the whole world")

})

testthat::test_that("test whole hemisphere", {
  # EQUAL AREA
  # check northern hemisphere
  suggested_crs_north_hem_eqa <- crs_wizard(c(-180, 180, 0, 90), distortion = "equal_area")
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
  suggested_crs_south_hem_eqa <- crs_wizard(c(-180, 180, -90, 0), distortion = "equal_area")
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
  suggested_crs_north_hem_eqd <- crs_wizard(c(-180, 180, 0, 90), distortion = "equidistant")
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
  suggested_crs_south_hem_eqd <- crs_wizard(c(-180, 180, -90, 0), distortion = "equidistant")
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
  expect_error(suggested_crs_south_hem_comp <- crs_wizard(c(-180, 180, -90, 0), distortion = "compromise"),
               "compromise is not available for maps covering a whole hemisphere")
  
  # CONFORMAL
  expect_error(suggested_crs_south_hem_conf <- crs_wizard(c(-180, 180, -90, 0), distortion = "conformal"),
               "conformal is not available for maps covering a whole hemisphere")
  
  })


