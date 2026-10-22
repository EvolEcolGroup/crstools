#' Extract projection-induced distortion metrics
#'
#' This function computes Tissot's indicatrix distortion metrics (Snyder 1987,
#' pp. 20-26) for a grid of points across the extent of `data`, evaluated in the
#' CRS of `data`.
#'
#' Distortion metrics can be computed using two different `method`:
#'
#' - `"sphere"`: for each GP, the function takes a short step, of lenght
#' `radius` due north and due eat on a sphere with fixed radius. Point and steps
#' are then projected to the CRS of `data`, and the distortion metrics are
#' computed. Because the Earth is not a perfect sphere, this method is less
#' accurate (below 1°) than the `"ellipsoid"` method, but it does not require
#' the `PROJ` library and the PROJ R package installed.
#' - `"ellipsoid"`: distortion metrics are computed using the `PROJ` R library
#' via the [PROJ::proj_factors()] fucntion, using the ellipsoid defined by the
#' CRS and therefore being more accurate than the `"sphere"` method. This method
#' requires the `PROJ` R package (>= 0.7.0) installed. Currently, the latter
#' works from binary for Windows and Linux, but not for MacOS. The latter can be
#' installed from source, but requires the PROJ library to be installed.
#'
#' The following measures are returned per each grid point: `areal_scale`, the
#' ratio of projected to true area (value of 1 means no area distortion, higher
#' values indicate local inflation, while lower values indicate local
#' shrinkage), `angular_distortion`, Snyder's maximum angular deformation in
#' degrees (a value of 0 degrees means no shape distortion, a value of 180
#' degrees  means complete shape distortion), the `semi-major` and `semi-minor`
#' axes of the indicatrix, and the intersection angle between the projected
#' meridian and parallel directions (90 degrees indicates no angular
#' distortion).
#'
#' @references Snyder, J.P. (1987) Map Projections—A Working Manual. U.S.
#'   Geological Survey Professional Paper 1395, pp. 20-26. Washington, D.C.:
#'   U.S. Government Printing Office. DOI: 10.3133/pp1395.
#'
#'   Sumner, M.D. (2026) PROJ: Generic Coordinate System Transformations Using
#'   'PROJ'. R package version 0.7.0. <https://CRAN.R-project.org/package=PROJ>
#'
#' @param data An sf, SpatRaster, or SpatVector object. This has to be projected
#'   to the CRS intened to assess.
#' @param centres Either a list with elements "lng" and "lat", or a vector of
#'   length 2 with the number of rows/columns for an automatic grid, as in
#'   [geom_tissot()]. Default is c(5, 5).
#' @param radius The length of the probing step used to estimate local
#'   distortion, in metres, used only when `method = "sphere"`. If NULL,
#'   estimated automatically as in [geom_tissot()]. Default is NULL.
#' @param method The Earth model used to compute the distortion metrics, either
#'   `"sphere"` or `"ellipsoid"`. `"sphere"` projects small steps north and east
#'   of each grid point, placed on a sphere, and estimates distortion from their
#'   displacement. `"ellipsoid"` uses [PROJ::proj_factors()], which accounts for
#'   the flattening of the ellipsoid and requires the suggested package 'PROJ'
#'   (>= 0.7.0); if this fails, a warning is issued and `"sphere"` is used
#'   instead. Default is `"sphere"`.
#' @return A data.frame with one row per grid point: `lon`, `lat` (centre of the
#'   point, in EPSG:4326), `areal_scale`, `angular_distortion` (degrees),
#'   `intersection_angle` (degrees), `semi_major`, and `semi_minor`.
#' @export
#' @examplesIf rlang::is_installed("rnaturalearth")
#' # load required packages
#' library(rnaturalearth)
#' library(sf)
#' s_america_sf <- ne_countries(continent = "South America", returnclass = "sf")
#' s_am_equal_area <- suggest_crs(s_america_sf, distortion = "equal_area")
#' s_america_proj <- st_transform(s_america_sf, s_am_equal_area$proj4)
#' metrics <- get_distortion_metrics(s_america_proj)
#' summary(metrics[c("areal_scale", "angular_distortion")])
get_distortion_metrics <- function(data, centres = c(5, 5), radius = NULL,
                                   method = c("sphere", "ellipsoid")) {
  # check that method is valid
  # use sphere (first element) as default if not specified
  method <- rlang::arg_match(method)

  # generate a grid of points across the extent of data
  grid <- tissot_grid_centres(data, centres = centres)
  coord_grid <- grid$centres
  orig_crs <- grid$crs

  # because the distortion metrics are only meaningful in a projected CRS,
  # check that the data is not in a geographic CRS
  if (sf::st_is_longlat(orig_crs)) {
    stop(
      paste0(
        "data uses a geographic (longitude/latitude) CRS; distortion ",
        "metrics will be uninformative. Please project data before ",
        "calling get_distortion_metrics()."
      )
    )
  }

  # if method is "ellipsoid", check that PROJ is installed and can be used
  if (method == "ellipsoid") {
    # check if PROJ is installed
    if (!rlang::is_installed("PROJ", version = "0.7.0")) {
      # stop if not
      stop(paste0(
        "Package 'PROJ' version >= 0.7.0 is required to run this ",
        "fucntion Please install it."
      ), call. = FALSE)
    }
    metrics <- tryCatch(
      # use ellopsoid via the PROJ package to estimate distortion
      distortion_ellipsoid(coord_grid, orig_crs),
      # if PROJ fails, fall back to sphere method
      error = function(e) {
        stop(
          paste0(
            "PROJ could not compute the distortion metrics (",
            conditionMessage(e), "). Please use method = \"sphere\"."
          ),
          # don't show the call in the warning message
          call. = FALSE
        )
      }
    )
    return(metrics)
  }

  # if PROJ failed or default used, estimate distortion on sphere
  distortion_sphere(coord_grid, orig_crs, radius = radius)
}

#####################
## helper function
#####################


# function to estimate distortion metrics using the "walking" north and east
# approach on a sphere
distortion_sphere <- function(coord_grid, crs, radius = NULL) {
  # if radius is null, estimate distance between two grid points
  # as in geom_tissot()
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
    cbind(
      lon = coord_grid[i, "lon"],
      lat = coord_grid[i, "lat"],
      tissot_point_metrics(
        lon = coord_grid[i, "lon"],
        lat = coord_grid[i, "lat"],
        crs = crs,
        radius = radius
      )
    )
  })

  # return data frame with metrics
  metrics <- do.call(rbind, metrics)
  rownames(metrics) <- NULL
  metrics
}


# function to compute distortion metrid on the elipsoid usign
# the PROJ package
distortion_ellipsoid <- function(coord_grid, crs) {
  # use PROJ to estimate distortion
  factors <- PROJ::proj_factors(coord_grid, crs$wkt)

  # check the values are not all NaN
  if (all(is.na(factors[, "areal_scale"]))) {
    stop("PROJ returned only missing values", call. = FALSE)
  }

  # return a data frame with the metrics
  metrics <- data.frame(
    # point location
    lon = coord_grid[, "lon"],
    lat = coord_grid[, "lat"],
    # areal scale (1 no distortion, >1 local inflation, <1 local shrinkage)
    areal_scale = factors[, "areal_scale"],
    # angular distortion (0 no distortion, 180 complete distortion)
    # convert to degrees from radians
    angular_distortion = factors[, "angular_distortion"] * 180 / pi,
    # intersection angle between projected meridian and parallel
    # (90 no distortion)
    intersection_angle = factors[, "meridian_parallel_angle"] * 180 / pi,
    # semi axes of elipses (1 no distorion)
    semi_major = factors[, "tissot_semimajor"],
    semi_minor = factors[, "tissot_semiminor"],
    row.names = NULL
  )

  # return the metrics
  return(metrics)
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

  # if the bbox is not in crs 4326, we should reproject it; as in
  # geom_tissot(), transform the bbox itself, as its edges are densified
  # (transforming only its corners can underestimate the extent)
  bbox_4326 <- data_bbox
  if (orig_crs != sf::st_crs("EPSG:4326")) {
    bbox_4326 <- sf::st_transform(data_bbox, sf::st_crs("EPSG:4326"))
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
    2 * asin((semi_major - semi_minor) / (semi_major + semi_minor)) * 180 / pi

  # return the complete output
  data.frame(
    areal_scale = areal_scale,
    angular_distortion = angular_distortion,
    intersection_angle = theta * 180 / pi,
    semi_major = semi_major,
    semi_minor = semi_minor
  )
}
