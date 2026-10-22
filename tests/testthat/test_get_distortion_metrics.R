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
  library(rnaturalearth)
  library(sf)
  s_america_sf <- ne_countries(continent = "South America", returnclass = "sf")
  s_am_equal_area <- suggest_crs(s_america_sf, distortion = "equal_area")
  s_america_proj <- st_transform(s_america_sf, s_am_equal_area$proj4)
  metrics <- get_distortion_metrics(s_america_proj)
  # check that areal_scale values are all equal to 1
  expect_true(all(round(metrics["areal_scale"], 1) == 1))
})


# check that for a conformal projection we `angular_distortion` values of 0
test_that("Check angular_distortion values for conformal projection", {
  library(rnaturalearth)
  library(sf)
  s_america_sf <- ne_countries(continent = "South America", returnclass = "sf")
  s_am_conformal <- suggest_crs(s_america_sf, distortion = "conformal")
  s_america_proj <- st_transform(s_america_sf, s_am_conformal$proj4)
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


