#' @title Projection of a square map
#' @description Projections for a small map with square format
#' @param distortion The distortion of the world map projection (e.g.,
#'   "equal_area", "equidistant", "conformal").
#' @param center data.frame with two numeric values, latitude and longitude of
#'   the center of the map.
#' @param latmin The minimum latitude of the map. Default is -90.
#' @param latmax The maximum latitude of the map. Default is 90.
#' @param quiet logical, whether to suppress messages.
#' @return data.frame with the suggested projection.
#' @keywords internal
#'

################################################################################
## crs for regional maps with square extent
crs_square_format <- function(
    distortion,
    center,
    latmin,
    latmax,
    quiet = FALSE) {
  # Handle cases based on the latitude of the center
  if (center$lat > 75) {
    if (distortion == "conformal") {
      crs_suggestions <- data.frame(
        prj = "stere",
        x0 = NA_real_,
        lat0 = 90,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Polar stereographic",
        notes = paste0(
          "Conformal projection for regional maps in square format;",
          " central meridian ",
          center$lng
        )
      )
    } else if (distortion == "equal_area") {
      crs_suggestions <- data.frame(
        prj = "laea",
        x0 = NA_real_,
        lat0 = 90,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Polar Lambert azimuthal equal-area",
        notes = paste0(
          "Equal-area projection for regional maps in square format;",
          " central meridian ",
          center$lng
        )
      )
    }
  } else if (center$lat < -75) {
    if (distortion == "conformal") {
      crs_suggestions <- data.frame(
        prj = "stere",
        x0 = NA_real_,
        lat0 = -90,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Polar stereographic",
        notes = paste0(
          "Conformal projection for regional maps in square format; ",
          "central meridian ",
          center$lng
        )
      )
    } else if (distortion == "equal_area") {
      crs_suggestions <- data.frame(
        prj = "laea",
        x0 = NA_real_,
        lat0 = -90,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Polar Lambert azimuthal equal-area",
        notes =
          paste0("Equal-area projection for regional maps in square format; ",
            "central meridian ", center$lng
          )
      )
    }
  } else if (abs(center$lat) < 15 && (latmax * latmin) <= 0) {
    if (distortion == "conformal") {
      crs_suggestions <- data.frame(
        prj = "stere",
        x0 = NA_real_,
        lat0 = 0,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Equatorial stereographic",
        notes = paste0(
          "Conformal projection for regional maps in square format; ",
          "central meridian ",
          center$lng
        )
      )
    } else if (distortion == "equal_area") {
      crs_suggestions <- data.frame(
        prj = "laea",
        x0 = NA_real_,
        lat0 = 0,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Equatorial Lambert azimuthal equal-area",
        notes = paste0(
          "Equal-area projection for regional maps in square format; ",
          "central meridian ",
          center$lng
        )
      )
    }
  } else {
    # Case: Between the pole and equator
    if (distortion == "conformal") {
      crs_suggestions <- data.frame(
        prj = "stere",
        x0 = NA_real_,
        lat0 = center$lat,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Oblique stereographic",
        notes = paste0(
          "Conformal projection for regional maps in square format; ",
          "central meridian ",
          center$lng
        )
      )
    } else if (distortion == "equal_area") {
      crs_suggestions <- data.frame(
        prj = "laea",
        x0 = NA_real_,
        lat0 = center$lat,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Oblique Lambert azimuthal equal-area",
        notes = paste0(
          "Equal-area projection for regional maps in square format; ",
          "central meridian ",
          center$lng
        )
      )
    }
  }

  # Include any notes about the scale factor
  if (distortion == "conformal" && !quiet) {
    message(
      paste0("To reduce overall area distortion on the map, ",
             "one can also apply a scale factor. Various values for can be ",
             "applied and the area distortion patterns along the center and ",
             "at the border of the map are compared to select ",
             "most appropriate value.")
    )
  }

  # Return the dataframe of suggestions
  return(crs_suggestions)
}
