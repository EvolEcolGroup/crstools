#################
## Sanity checks
#################

# check if osx and if proj database is available
# if not skip tests
# if (Sys.info()[["sysname"]] == "Darwin") {
#   proj_db_check <- try(
#     PROJ::proj_factors(cbind(0, 0), "EPSG:3857"),
#     silent = TRUE
#   )
#   if (inherits(proj_db_check, "try-error")) {
#     skip()
#   }
# }

# define object for tests
location <- sf::st_as_sf(data.frame(lon = c(-10, 10), lat = c(35, 55)),
                         coords = c("lon", "lat"),
                         crs = 4326)

# grid of known centres used across tests
test_centres <- list(lng = c(0, 5), lat = c(0, 30, 60))


# check warning for unprojected locations
test_that("Warning unprojected points", {
  expect_error(get_distortion_metrics(location, centres = c(2, 2)),
                 "data uses a geographic")

})

# check input data is supported format
test_that("Check input data", {
  location <- data.frame(lon = c(-10, 10), lat = c(35, 55))
  expect_error(get_distortion_metrics(location, centres = c(2,2)), 
               "data must be an sf, SpatRaster, or SpatVector object")
})

# check CRS is defined 
test_that("Check crs is defined", {
  # remove CRS
  sf::st_crs(location) <- NA
  expect_error(get_distortion_metrics(location, centres =c(2,2)), 
               "data must have a defined CRS")
})


# check that centres is a numeric vector of length 2
test_that("Check centres is a numeric vector of length 2", {
  expect_error(get_distortion_metrics(location, centres = c(2,2,2)), 
               "centres must be either a list with elements 'lng' and 'lat'")
  expect_error(get_distortion_metrics(location, centres = list(c(2,2))), 
               "centres must be either a list with elements 'lng' and 'lat'")
})

# check that method is either "sphere" or "ellipsoid"
test_that("Check method is either 'sphere' or 'ellipsoid'", {
  expect_error(get_distortion_metrics(location, centres = c(2,2),
                                      method = "blah"), 
               "must be one of")
})

# check output structure
test_that("Output has the expected structure", {
  # get crs
  suggested_crs <- suggest_crs(location, distortion = "equal_area")
  location <- sf::st_transform(location, suggested_crs$proj4)
  metrics <- get_distortion_metrics(location, centres = test_centres,
                                    radius = 1000)
  expect_true(inherits(metrics, "data.frame"))
  expect_named(metrics, c("lon", "lat", "areal_scale", "angular_distortion",
                          "intersection_angle", "semi_major", "semi_minor"))
  # one row per combination of lng and lat
  expect_equal(nrow(metrics), 6)
  expect_equal(sort(unique(metrics$lon)), c(0, 5))
  expect_equal(sort(unique(metrics$lat)), c(0, 30, 60))
  # semi-major is never smaller than semi-minor
  expect_true(all(metrics$semi_major >= metrics$semi_minor))
  # the area of the indicatrix is the product of the semi-axes
  expect_equal(metrics$semi_major * metrics$semi_minor, metrics$areal_scale,
               tolerance = 1e-8)
})

# check no difference in output when using SpatVector or sf object
test_that("sf, SpatVector and SpatRaster give the same results", {
  # get crs
  suggested_crs <- suggest_crs(location, distortion = "equal_area")
  location <- sf::st_transform(location, suggested_crs$proj4)
  location_vect <- terra::vect(location)
  location_rast <- terra::rast(terra::ext(location_vect),
                               crs = terra::crs(location_vect),
                               nrows = 10, ncols = 10)
  metrics_sf <- get_distortion_metrics(location)
  expect_equal(get_distortion_metrics(location_vect), metrics_sf,
               tolerance = 1e-6)
  expect_equal(get_distortion_metrics(location_rast), metrics_sf,
               tolerance = 1e-6)
})



# check that the grid of centres is the same used by geom_tissot()
test_that("grid of centres matches geom_tissot()", {
  area <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(
      rbind(c(-80, -60), c(-30, -60), c(-30, 10), c(-80, 10), c(-80, -60))
    )),
    crs = 4326
  ))
  area_laea <- sf::st_transform(area, "+proj=laea +lat_0=-25 +lon_0=-55")
  metrics <- get_distortion_metrics(area_laea)
  p <- ggplot2::ggplot(area_laea) +
    ggplot2::geom_sf() +
    geom_tissot()
  circles <- ggplot2::ggplot_build(p)$data[[2]]$geometry
  tissot_centres <- sf::st_coordinates(suppressWarnings(
    sf::st_centroid(sf::st_transform(circles, 4326))
  ))
  expect_setequal(unique(round(tissot_centres[, "X"])), unique(metrics$lon))
  expect_setequal(unique(round(tissot_centres[, "Y"])), unique(metrics$lat))
})


####################
## Theoretical checks
####################


# check that for an equal area projection we `areal_scale` values of 1
test_that("Check areal_scale values for equal area projection", {
  # rnaturalearth (and its data package) are only suggested
  skip_if_not_installed("rnaturalearth")
  skip_if_not_installed("rnaturalearthdata")
  s_america_sf <- rnaturalearth::ne_countries(
    continent = "South America",
    returnclass = "sf"
  )
  s_am_equal_area <- suggest_crs(s_america_sf, distortion = "equal_area")
  s_america_proj <- sf::st_transform(s_america_sf, s_am_equal_area$proj4)
  metrics <- get_distortion_metrics(s_america_proj)
  # check that areal_scale values are all equal to 1
  expect_true(all(round(metrics["areal_scale"], 1) == 1))
})


# check that for a conformal projection we `angular_distortion` values of 0
test_that("Check angular_distortion values for conformal projection", {
  # rnaturalearth (and its data package) are only suggested
  skip_if_not_installed("rnaturalearth")
  skip_if_not_installed("rnaturalearthdata")
  s_america_sf <- rnaturalearth::ne_countries(
    continent = "South America",
    returnclass = "sf"
  )
  s_am_conformal <- suggest_crs(s_america_sf, distortion = "conformal")
  s_america_proj <- sf::st_transform(s_america_sf, s_am_conformal$proj4)
  # check for sphere (approximation,small residual angular distortion)
  metrics_sph <- get_distortion_metrics(s_america_proj, method = "sphere", 
                                        radius = 100)
  expect_true(all(round(metrics_sph$angular_distortion, 1) < 1))
  # check for ellipsoid (exact Tissot factors, angular distortion should be 0)
  skip_if_not_installed("PROJ", "0.7.0")
  # check if PROJ can compute distortion factors (check for proj.db file)
  proj_db_check <- try(
    PROJ::proj_factors(cbind(0, 0), "EPSG:3857"),
    silent = TRUE
  )
  skip_if(
    inherits(proj_db_check, "try-error") ||
      # check if areal_scale is NA
      all(is.na(proj_db_check[, "areal_scale"])),
    "PROJ cannot compute distortion factors (proj.db not found)"
  )
  metrics_ell <- get_distortion_metrics(s_america_proj, method = "ellipsoid")
  expect_true(all(round(metrics_ell$angular_distortion, 1) == 0))
})

# check that the sphere method is not affected by the antimeridian
test_that("sphere method is stable across the antimeridian", {
  # Web Mercator: the map is cut at 180 degrees longitude
  location_merc <- sf::st_transform(location, "EPSG:3857")
  # two centres mirrored around 180 degrees; the east step of the first one
  # crosses the antimeridian and lands on the other side of the map
  metrics <- get_distortion_metrics(location_merc,
                                    centres = list(lng = c(179.999, -179.999),
                                                   lat = 0),
                                    radius = 1000)
  # Mercator has no distortion at the equator
  expect_equal(metrics$areal_scale, c(1, 1), tolerance = 1e-3)
  expect_equal(metrics$semi_major, c(1, 1), tolerance = 1e-3)

  # Pacific-centred Robinson: the map is cut at 0 degrees longitude
  location_pacific <- sf::st_transform(location, "+proj=robin +lon_0=180")
  # two centres mirrored around the cut are at the same distance from the
  # central meridian, so they should have the same distortion
  metrics <- get_distortion_metrics(location_pacific,
                                    centres = list(lng = c(-0.0001, 0.0001),
                                                   lat = 0),
                                    radius = 1000)
  expect_equal(unlist(metrics[1, -1]), unlist(metrics[2, -1]),
               tolerance = 1e-3)
})

# check that the sphere method is not affected by stepping over the North Pole
test_that("sphere method is stable near the poles", {
  # equidistant cylindrical: each pole is a line as long as the equator
  location_eqc <- sf::st_transform(location, "+proj=eqc")
  # centres mirrored around the equator; the north step of the northern
  # centre crosses the North Pole and lands on the opposite meridian
  metrics <- get_distortion_metrics(location_eqc,
                                    centres = list(lng = 0,
                                                   lat = c(-89.995, 89.995)),
                                    radius = 1000)
  # the scale along the meridian is always 1 in this projection
  expect_equal(metrics$semi_minor, c(1, 1), tolerance = 1e-3)
  # both hemispheres should have the same distortion
  expect_equal(metrics$areal_scale[2], metrics$areal_scale[1],
               tolerance = 1e-3)

  # same issue with a large step over Arctic data: a 500 km step north from
  # 86 degrees also crosses the North Pole
  metrics <- get_distortion_metrics(location_eqc,
                                    centres = list(lng = 0, lat = c(-86, 86)),
                                    radius = 5e5)
  expect_equal(metrics$semi_minor, c(1, 1), tolerance = 1e-2)
  expect_equal(metrics$areal_scale[2], metrics$areal_scale[1],
               tolerance = 1e-3)
})

# check that polar azimuthal projections, where crossing the pole does not
# create a jump on the map, still give the right values at the pole
test_that("sphere method works at the pole for polar projections", {
  location_polar <- sf::st_transform(location, "+proj=laea +lat_0=90")
  metrics <- get_distortion_metrics(location_polar,
                                    centres = list(lng = 0,
                                                   lat = c(89.995, 90)),
                                    radius = 1000)
  # Lambert azimuthal equal area keeps the areal scale at 1
  expect_equal(metrics$areal_scale, c(1, 1), tolerance = 1e-2)
  # and has no angular distortion at the pole
  expect_true(all(metrics$angular_distortion < 0.1))
})

# check that the sphere method does not depend on the linear unit of the CRS
test_that("sphere method gives the same results in any linear unit", {
  # same projection in metres and in feet, as returned by suggest_crs()
  crs_m <- suggest_crs(location, distortion = "equal_area", unit = "m")
  crs_ft <- suggest_crs(location, distortion = "equal_area", unit = "ft")
  metrics_m <- get_distortion_metrics(sf::st_transform(location, crs_m$proj4),
                                      centres = test_centres)
  metrics_ft <- get_distortion_metrics(sf::st_transform(location,
                                                        crs_ft$proj4),
                                       centres = test_centres)
  # if the projected steps were not converted to metres, areal_scale would be
  # about 10.76 times larger in feet
  expect_equal(metrics_ft, metrics_m, tolerance = 1e-6)

  # other units: kilometres and a custom unit (1 unit = 2 metres)
  laea <- "+proj=laea +lat_0=45 +lon_0=0 +datum=WGS84"
  metrics_laea <- get_distortion_metrics(
    sf::st_transform(location, paste(laea, "+units=m")),
    centres = test_centres
  )
  for (unit in c("+units=km", "+to_meter=2")) {
    metrics_unit <- get_distortion_metrics(
      sf::st_transform(location, paste(laea, unit)),
      centres = test_centres
    )
    expect_equal(metrics_unit, metrics_laea, tolerance = 1e-6)
  }

  # feet with a datum shift: PROJ wraps the CRS in a BOUNDCRS, where the
  # last unit in the WKT is the one of the datum shift (metres), not the one
  # of the coordinates
  laea_shift <- "+proj=laea +lat_0=45 +lon_0=0 +ellps=WGS84 +towgs84=0,0,0"
  location_shift_ft <- sf::st_transform(location,
                                        paste(laea_shift, "+units=ft"))
  expect_true(grepl("^BOUNDCRS", sf::st_crs(location_shift_ft)$wkt))
  metrics_shift_m <- get_distortion_metrics(
    sf::st_transform(location, paste(laea_shift, "+units=m")),
    centres = test_centres
  )
  metrics_shift_ft <- get_distortion_metrics(location_shift_ft,
                                             centres = test_centres)
  expect_equal(metrics_shift_ft, metrics_shift_m, tolerance = 1e-6)
})


