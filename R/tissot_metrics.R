#' Extract projection-induced distortion metrics
#' 
#' This function computes Tissot's indicatrix distortion metrics (Snyder 1987,
#' pp. 20-26) for a grid of points across the extent of `data`, evaluated in the
#' CRS of `data`. Rather than drawing a circle and measuring how it warps once
#' reprojected (the approach [geom_tissot()] uses to visualise distortion), this
#' "takes a short step" due north and due east of each grid point and reads the
#' distortion off how those two steps land once projected. The metrics are
#' computed using the PROJ library R wrapper, which uses the appropriate shape
#' of the Earth based on the used projection.
#' 
#' The following measures are returned per each grid point: `areal_scale`, the
#' ratio of projected to true area (value of 1 means no area distortion,
#' higher values indicate local inflation, while lower values indicate local
#' shrinkage), `angular_distortion`, Snyder's maximum angular deformation in
#' degrees (a value of 0 degrees means no shape distortion, a value of 180
#' degrees  means complete shape distortion), the semi-major and semi-minor
#' axes of the indicatrix, and the intersection angle between the projected
#' meridian and parallel directions (90 degrees indicates no angular
#' distortion).
#'   
#' @references Snyder, J.P. (1987) Map Projections—A Working Manual. U.S.
#'   Geological Survey Professional Paper 1395, pp. 20-26. Washington, D.C.:
#'   U.S. Government Printing Office. DOI: 10.3133/pp1395.
#'   
#' @param data An sf, SpatRaster, or SpatVector object. This has to be projected
#'   to the CRS intened to assess.
#' @param centres Either a list with elements "lng" and "lat", or a vector
#'   of length 2 with the number of rows/columns for an automatic grid, as
#'   in [geom_tissot()]. Default is c(5, 5).
#' @return A data.frame with one row per grid point: `lon`, `lat` (centre of
#'   the point, in EPSG:4326), `areal_scale`, `angular_distortion` (degrees),
#'   `intersection_angle` (degrees), `semi_major`, and `semi_minor`.
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

tissot_metrics <- function(data, centres = c(5, 5)) {
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
        "calling tissot_metrics()."
      )
    )
  }
  
  # use PROJ to estimate distortion using the "walking" north and eat approach
  factors <- PROJ::proj_factors(coord_grid, orig_crs$wkt)
  
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

#####################
## helper function
#####################

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
