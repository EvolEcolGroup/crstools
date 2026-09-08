#' Extract projection-induced distortion metrics
#'
#' This function computes Tissot's indicatrix distortion metrics (Snyder
#' 1987, pp. 20-26) for a grid of points across the extent of `data`,
#' evaluated in the CRS of `data`. Rather than drawing a circle and
#' measuring how it warps once reprojected (the approach [geom_tissot()]
#' uses to visualise distortion), this "takes a short step" due north and due
#' east of each grid point and reads the distortion off how those two steps
#' land once projected. 
#'
#' The following measures are return per each grid point: `areal_scale`, the
#' ratio of projected to true area (value of 1 means no area distortion, higher
#' values in dicate local inflation, while lower values indicate local
#' shrinkage), `angular_distortion`, Snyder's maximum angular deformation in
#' degrees (a value of 0 degrees means no shape distortion, a value of 180
#' degress  means complete shape distortion), the semi-major and semi-minor axes
#' of the indicatrix, and the intersection angle between the projected meridian
#' and parallel directions (90 degrees indicates no angular distortion).
#' 
#' Equation numbering follows Gimond's tissot R functions
#' (https://github.com/mgimond/tissot).
#'
#' @param data An sf, SpatRaster, or SpatVector object. This has to projected
#'   to the CRS intened to assess.
#' @param centres Either a list with elements "lng" and "lat", or a vector
#'   of length 2 with the number of rows/columns for an automatic grid, as
#'   in [geom_tissot()]. Default is c(5, 5).
#' @param radius The length of the probing step used to estimate local
#'   distortion, in metres. If NULL, estimated automatically
#'   as in [geom_tissot()].Default is NULL.
#' @return A data.frame with one row per grid point: `lon`, `lat` (centre of
#'   the point, in EPSG:4326), `areal_scale`, `angular_distortion` (degrees),
#'   `intersection_angle` (degrees), `semi_major`, and `semi_minor` (in the
#'   units of `data`'s CRS).
#' @export
#' @examplesIf rlang::is_installed("rnaturalearth")
#' # load required packages
#' library(rnaturalearth)
#' library(sf)
#' s_america_sf <- ne_countries(continent = "South America", returnclass = "sf")
#' s_am_equal_area <- suggest_crs(s_america_sf, distortion = "equal_area")
#' s_america_proj <- st_transform(s_america_sf, s_am_equal_area$proj4)
#' metrics <- tissot_metrics(s_america_proj)
#' summary(metrics[c("areal_scale", "angular_distortion")])

tissot_metrics <- function(data, centres = c(5, 5), radius = NULL) {
  # generate a grid of points across the extent of data
  grid <- tissot_grid_centres(data, centres = centres)
  coord_grid <- grid$centres
  orig_crs <- grid$crs
  
  # because the distortion metrics are only meaningful in a projected CRS,
  # check that the data is not in a geographic CRS
  if (sf::st_is_longlat(orig_crs)) {
    warning(
      paste0(
        "data uses a geographic (longitude/latitude) CRS; distortion ",
        "metrics will be uninformative. Please project data before", 
        "calling tissot_metrics()."
      )
    )
  }
  
  # if radius is null, estimate distance between two grid points, as in
  # geom_tissot()
  if (is.null(radius)) {
    coord_grid_sf <- sf::st_as_sf(
      as.data.frame(coord_grid),
      coords = c("lon", "lat"),
      crs = sf::st_crs("EPSG:4326")
    )
    dist_mat <- sf::st_distance(x = coord_grid_sf)
    diag(dist_mat) <- NA
    radius <- as.numeric(min(dist_mat, na.rm = TRUE)) / 4
  }
  
  # compute Tissot metrics for each grid point
  metrics <- lapply(seq_len(nrow(coord_grid)), function(i) {
    point_metrics <- tissot_point_metrics(
      lon = coord_grid[i, "lon"],
      lat = coord_grid[i, "lat"],
      crs = orig_crs,
      radius = radius
    )
    cbind(
      lon = coord_grid[i, "lon"],
      lat = coord_grid[i, "lat"],
      point_metrics
    )
  })
  
  # return data frame with metrics
  do.call(rbind, metrics)
}


#####################
## helper functions
#####################

# Function to calculate a destination point at a given bearing and distance
# from a lon/lat point using the standard spherical direct geodetic formula.
# The radius is the assumed radius of the Earth.
destination_point <- function(lon, lat, bearing, distance, radius = 6378137) {
  
  # unname the inputs
  lon <- unname(lon)
  lat <- unname(lat)
  
  # convert to radians
  lat1 <- lat * pi / 180
  lon1 <- lon * pi / 180
  brng <- bearing * pi / 180
  # calculate the angular distance
  delta <- distance / radius
  
  # calculate the destination point
  lat2 <- asin(sin(lat1) * cos(delta) + cos(lat1) * sin(delta) * cos(brng))
  lon2 <- lon1 + atan2(
    sin(brng) * sin(delta) * cos(lat1),
    cos(delta) - sin(lat1) * sin(lat2)
  )
  
  # return the destination point in degrees
  c(lon = lon2 * 180 / pi, lat = lat2 * 180 / pi)
}

# function to generate a grid of longitude/latitude centre points across
# the extent of data and returns its CRS.
tissot_grid_centres <- function(data, centres = c(5, 5)) {
  # check that data is an sf, SpatRaster, or SpatVector object
  if (!inherits(data, "sf")) {
    if (inherits(data, "SpatRaster") || inherits(data, "SpatVector")) {
      # convert to sf bbox
      data_bbox <- sf::st_bbox(terra::ext(data))
      sf::st_crs(data_bbox) <- terra::crs(data)
      data <- sf::st_as_sf(sf::st_as_sfc(data_bbox))
    } else {
      stop("data must be an sf, SpatRaster, or SpatVector object")
    }
  }
  # get the bounding box of the data and its CRS
  data_bbox <- sf::st_bbox(data)
  orig_crs <- sf::st_crs(data_bbox)
  
  # check that the data has a defined CRS
  if (is.na(orig_crs)) {
    stop("data must have a defined CRS")
  }
  
  # if the bbox is not in crs 4326, we should reproject it
  bbox_4326 <- data_bbox
  if (orig_crs != sf::st_crs("EPSG:4326")) {
    bbox_4326 <- sf::st_bbox(
      sf::st_transform(sf::st_as_sfc(data_bbox), sf::st_crs("EPSG:4326"))
    )
  }
  
  # if centres is a vector of two elements (not a list),
  # generate the grid of centres
  if (!inherits(centres, "list") && length(centres) == 2) {
    # generate sequences
    lon_seq <- pretty(c(bbox_4326$xmin, bbox_4326$xmax), n = centres[1] + 1)
    lat_seq <- pretty(c(bbox_4326$ymin, bbox_4326$ymax), n = centres[2] + 1)
    # pretty() rounds outward to "nice" numbers, so its first/last breaks can
    # fall on or beyond xmin/xmax. Remove first and last values.
    lon_seq <- lon_seq[-c(1, length(lon_seq))]
    lat_seq <- lat_seq[-c(1, length(lat_seq))]
    
    # create a grid of points from the sequences
    coord_grid <- as.matrix(expand.grid(lon_seq, lat_seq))
    # if we have a list, we use the values in the list
  } else if (
    inherits(centres, "list") && all(c("lng", "lat") %in% names(centres))
  ) {
    coord_grid <- as.matrix(expand.grid(centres$lng, centres$lat))
  } else {
    stop(
      paste0(
        "centres must be either a list with elements 'lng' ",
        "and 'lat' or a vector of length 2"
      )
    )
  }
  
  # set the column names of the grid to "lon" and "lat"
  colnames(coord_grid) <- c("lon", "lat")
  
  # return the complete output
  list(centres = coord_grid, crs = orig_crs)
}

# function to compute Tissot's indicatrix distortion parameters at a single
# longitude/latitude point for a given target CRS.
tissot_point_metrics <- function(lon, lat, crs, radius) {
  # move north (bearing 0) and east (bearing 90)
  north <- destination_point(lon, lat, bearing = 0, distance = radius)
  east <- destination_point(lon, lat, bearing = 90, distance = radius)
  
  # create an sf object with the three points (lon/lat) and project them
  # into the target CRS
  pts <- sf::st_as_sf(
    data.frame(
      lon = unname(c(lon, north["lon"], east["lon"])),
      lat = unname(c(lat, north["lat"], east["lat"]))
    ),
    coords = c("lon", "lat"),
    crs = sf::st_crs("EPSG:4326")
  )
  # get the projected coordinates of the three points
  pts_proj <- sf::st_coordinates(sf::st_transform(pts, crs))
  
  # get displacement of the projected north/east steps from the projected
  # centre, used to approximate the local partial derivative
  v_meridian <- pts_proj[2, ] - pts_proj[1, ]
  v_parallel <- pts_proj[3, ] - pts_proj[1, ]
  
  # scale factors along the meridian (h) and parallel (k)
  h <- sqrt(sum(v_meridian^2)) / radius
  k <- sqrt(sum(v_parallel^2)) / radius
  
  # calculate angle between the projected meridian and parallel directions
  cos_theta <- sum(v_meridian * v_parallel) /
    (sqrt(sum(v_meridian^2)) * sqrt(sum(v_parallel^2)))
  theta <- acos(pmin(pmax(cos_theta, -1), 1))
  
  # calculate areal scale factor, as the ratio of projected to true area
  areal_scale <- h * k * sin(theta)
  
  # calculate semi-major (a) and semi-minor (b) axes of the indicatrix
  a_prime <- sqrt(max(0, h^2 + k^2 + 2 * areal_scale))
  b_prime <- sqrt(max(0, h^2 + k^2 - 2 * areal_scale))
  semi_major <- (a_prime + b_prime) / 2
  semi_minor <- (a_prime - b_prime) / 2
  
  # calculate maximum angular deformation
  angular_distortion <-
    2 * asin((semi_major - semi_minor) / (semi_major + semi_minor)) *
    180 / pi
  
  # return the complete output
  data.frame(
    areal_scale = areal_scale,
    angular_distortion = angular_distortion,
    intersection_angle = theta * 180 / pi,
    semi_major = semi_major,
    semi_minor = semi_minor
  )
}