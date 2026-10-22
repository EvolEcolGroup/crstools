################################################################################
# Normalizing longitude values
normalise_lon <- function(lon, lon0) {
  while (lon < (lon0 - 180.0)) {
    lon <- lon + 360.0
  }
  while (lon > (lon0 + 180.0)) {
    lon <- lon - 360.0
  }
  return(lon)
}

################################################################################
# Get the extent (xmin, xmax, ymin, ymax) of a bbox in longitude and latitude,
# reprojecting it if it is in a projected CRS
bbox_lonlat_ext <- function(x_bbox) {
  x_crs <- sf::st_crs(x_bbox)
  if (!is.na(x_crs) && !isTRUE(sf::st_is_longlat(x_crs))) {
    # transforming the bbox densifies its edges, so that its extent is not
    # underestimated
    x_bbox <- sf::st_transform(x_bbox, sf::st_crs("EPSG:4326"))
  }
  # note that sf::st_bbox returns c(xmin, ymin, xmax, ymax)
  as.vector(x_bbox)[c(1, 3, 2, 4)]
}

################################################################################
# Function to round values for world maps
round_world_coords <- function(value, scale, round_cm) {
  val <- NULL

  if (round_cm || scale < 1.15) {
    val <- round(value)
  } else if (scale < 1.32) {
    val <- round(value * 2) / 2
  } else {
    val <- round(value * 10) / 10
  }

  return(val)
}
