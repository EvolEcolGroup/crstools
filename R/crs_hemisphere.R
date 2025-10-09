#' @title Projection of a hemisphere map
#' @description Function to suggest a projection for maps showing a hemisphere
#' @param distortion character string (e.g., "equal_area", "equidistant",
#'   "conformal").
#' @param center data.frame with two numeric values, latitude and longitude of
#'   the center of the map.
#' @param scale numeric value, scale of the map.
#' @param latmin numeric value, minimum latitude of the map.
#' @param latmax numeric value, maximum latitude of the map.
#' @param quiet logical, whether to suppress messages.
#' @return data.frame with the suggested projection.
#' @keywords internal
#' @noRd
#'
#'

################################################################################
# maps showing a hemisphere
crs_hemisphere <- function(
    distortion,
    center,
    scale,
    latmin,
    latmax,
    quiet = FALSE) {
  # Formatting central meridian
  lon <- round(center$lng * 100) / 100

  # Checking if within tropics
  if (abs(latmax) < 23.43665 && abs(latmin) < 23.43665) {
    # Defining standard parallel
    interval <- (latmax - latmin) / 4
    lat_s1 <- center$lat + interval
    lat_s2 <- center$lat - interval

    lat_std <- ifelse(
      (lat_s1 > 0 && lat_s2 > 0) || (lat_s1 < 0 && lat_s2 < 0),
      max(abs(latmax), abs(latmin)) / 2,
      0
    )
    lat_std <- round(lat_std * 100) / 100

    # Adding projection output
    if (distortion == "equal_area") {
      crs_suggestions <- data.frame(
        prj = "cea",
        x0 = NA_real_,
        lat0 = NA_real_,
        lat1 = lat_std,
        lat2 = NA_real_,
        lon0 = lon,
        k0 = NA_real_,
        description = "Cylindrical equal-area",
        notes = "Equal-area projection for maps showing the tropics"
      )
    } else if (distortion == "conformal") {
      crs_suggestions <- data.frame(
        prj = "merc",
        x0 = NA_real_,
        lat0 = NA_real_,
        lat1 = lat_std,
        lat2 = NA_real_,
        lon0 = lon,
        k0 = NA_real_,
        description = "Mercator",
        notes = "Conformal projection for maps showing the tropics"
      )
    } else if (distortion == "equidistant") {
      crs_suggestions <- data.frame(
        prj = "eqc",
        x0 = NA_real_,
        lat0 = NA_real_,
        lat1 = lat_std,
        lat2 = NA_real_,
        lon0 = lon,
        k0 = NA_real_,
        description = "Equidistant cylindrical",
        notes =
          paste0("Equidistant projection for maps showing the tropics ",
                 "- distance correct along meridians")
      )
    }
  } else {
    # Formatting central latitude
    lat <- if (center$lat > 85) {
      90
    } else if (center$lat < -85) {
      -90
    } else {
      round(center$lat * 100) / 100
    }

    # Adding projection output
    if (distortion == "equal_area") {
      crs_suggestions <- data.frame(
        prj = "laea",
        x0 = NA_real_,
        lat0 = lat,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = lon,
        k0 = NA_real_,
        description = "Lambert azimuthal equal-area",
        notes = "Equal-area projection for maps showing a hemisphere"
      )
    } else if (distortion == "equidistant") {
      crs_suggestions <- data.frame(
        prj = "aeqd",
        x0 = NA_real_,
        lat0 = lat,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = lon,
        k0 = NA_real_,
        description = "Azimuthal equidistant",
        notes = "Equidistant projection for maps showing a hemisphere"
      )
    }
  }

  return(crs_suggestions)
}
