################################################################################
# Normalizing longitude values
normalise_lon <- function(lon, lon0) {
  while (lon < (lon0 - 180.0)) {
    lon <- lon + 360.0
  }
  while (lon > (lon0 + 180.0)) {
    lon <- lon - 360.0
  }
  return(lon) # nolint
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

  return(val) # nolint
}
