#################
## Sanity checks
#################

# check warning for unprojected locations
# THIS FAILS EVEN IF WE GET THE SAME ERROR, POTENTIALLY DUE TO THE WAY THE 
# ERROR IS CAPTURED IN THE FUNCTION.
# test_that("Warning unprojected points", {
#   location <- sf::st_as_sf(data.frame(lon = c(-10, 10), lat = c(35, 55)),
#                       coords = c("lon", "lat"),
#                       crs = 4326)
#   expect_error(tissot_metrics(location, centres = c(2, 2)),
#                  "data uses a geographic (longitude/latitude) CRS; distortion")
#   
# })

# check input data is supported format
test_that("Check input data", {
  location <- data.frame(lon = c(-10, 10), lat = c(35, 55))
  expect_error(tissot_metrics(location, centres = c(2,2)), 
               "data must be an sf, SpatRaster, or SpatVector object")
})

# check CRS is defined 
test_that("Check crs is defined", {
  location <- sf::st_as_sf(data.frame(lon = c(-10, 10), lat = c(35, 55)),
                           coords = c("lon", "lat"),
                           crs = 4326)
  # remove CRS
  sf::st_crs(location) <- NA
  expect_error(tissot_metrics(location, centres =c(2,2)), 
               "data must have a defined CRS")
})


# check that centres is a numeric vector of length 2
test_that("Check centres is a numeric vector of length 2", {
  location <- sf::st_as_sf(data.frame(lon = c(-10, 10), lat = c(35, 55)),
                           coords = c("lon", "lat"),
                           crs = 4326)
  expect_error(tissot_metrics(location, centres = c(2,2,2)), 
               "centres must be either a list with elements 'lng' and 'lat'")
  expect_error(tissot_metrics(location, centres = list(c(2,2))), 
               "centres must be either a list with elements 'lng' and 'lat'")
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
  metrics <- tissot_metrics(s_america_proj)
  # check that areal_scale values (rounded to first decimal) are all equal or 
  # smaller than 1
  expect_true(all(round(metrics["areal_scale"], 1) <= 1))
})


# THIS DOES NOT WORK, CHECK WHY I GET 1 IN SOME PART OF THE GRID
# check that for a conformal projection we `angular_distortion` values of 0
# test_that("Check angular_distortion values for conformal projection", {
#   library(rnaturalearth)
#   library(sf)
#   s_america_sf <- ne_countries(continent = "South America", returnclass = "sf")
#   s_am_conformal <- suggest_crs(s_america_sf, distortion = "conformal")
#   s_america_proj <- st_transform(s_america_sf, s_am_conformal$proj4)
#   metrics <- tissot_metrics(s_america_proj)
#   # check that angular_distortion values (rounded to first decimal) are all equal or 
#   # smaller than 0
#   expect_true(all(round(metrics["angular_distortion"], 1) <= 0))
# })


####################
## Functional checks
####################


# compare results between the `tissot_metrics` fucntion and those obtained 
# via PROJ
