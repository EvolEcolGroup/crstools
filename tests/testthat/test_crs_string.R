# styler: off

test_that("generate crs string correctly", {
  # test error for non-exising projectsion
  # commented out because we use `check.arg` which has an automatic
  # error message prone to change
  #expect_error(
  #  crs_string("blah", NA, NA, NA, NA, 0, NA, "WGS84", "m"),
  #  "Projection not recognized."
  #)


  # equal earth default with whole world
  test_string <- crs_string("eqearth", NA, NA, NA, NA, 0, NA, "WGS84", "m")
  expect_equal(test_string$proj4, "+proj=eqearth +lon_0=0 +datum=WGS84 +units=m +no_defs")
  # BUG is the wkt from the website valid?!? there s
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
  expect_true(sf::st_crs(test_string$wkt) == sf::st_crs(ref_wkt))
  # check that proj4 is the same as wkt
  expect_true(sf::st_crs(test_string$wkt) == sf::st_crs(test_string$proj4))
})

# project a few points with both the proj4 and the wkt strings
project_with_both <- function(crs_obj) {
  pts <- sf::st_as_sf(
    data.frame(lon = c(-30, 0, 10, 25, 40), lat = c(-20, 0, 35, 50, 60)),
    coords = c("lon", "lat"),
    crs = 4326
  )
  list(
    proj4 = sf::st_coordinates(sf::st_transform(pts, crs_obj$proj4)),
    wkt = sf::st_coordinates(sf::st_transform(pts, crs_obj$wkt))
  )
}

test_that("proj4 and wkt strings define the same projection", {
  prjs <- eval(formals(crs_string)$prj)
  for (prj in prjs) {
    if (prj == "tpeqd") {
      # lat0, lat1, lat2, lon0 store lat_1, lon_1, lat_2, lon_2
      crs_obj <- crs_string(prj, NA, 34, -117, 46, 16, NA, "WGS84", "m")
    } else {
      crs_obj <- crs_string(prj, NA, 30, 20, 50, 10, NA, "WGS84", "m")
    }
    coords <- project_with_both(crs_obj)
    expect_false(anyNA(coords$proj4), label = prj)
    expect_equal(coords$wkt, coords$proj4, tolerance = 1e-6, label = prj)
  }
})

test_that("Winkel Tripel uses the standard parallel 50.467", {
  wintri <- crs_string("wintri", NA, NA, NA, NA, 10, NA, "WGS84", "m")
  expect_match(wintri$proj4, "+lat_1=50.467", fixed = TRUE)
})

test_that("Hammer wkt can be used by sf and terra", {
  for (datum in c("WGS84", "ETRS89", "NAD83")) {
    for (unit in c("m", "ft")) {
      hammer <- crs_string("hammer", NA, NA, NA, NA, 10, NA, datum, unit)
      expect_false(is.na(sf::st_crs(hammer$wkt)$proj4string))
      coords <- project_with_both(hammer)
      expect_false(anyNA(coords$wkt))
      expect_equal(coords$wkt, coords$proj4, tolerance = 1e-6)
    }
  }
  hammer <- crs_string("hammer", NA, NA, NA, NA, 10, NA, "WGS84", "m")
  pts <- terra::vect(cbind(c(-30, 40), c(-20, 60)), crs = "EPSG:4326")
  expect_equal(
    terra::crds(terra::project(pts, hammer$wkt)),
    terra::crds(terra::project(pts, hammer$proj4))
  )
})
