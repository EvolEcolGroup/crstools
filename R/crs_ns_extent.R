#' @title North-South extent
#' @description Checking the North-South extent of a regional map
#' @param distortion character string (e.g., "equal_area", "equidistant",
#'   "conformal").
#' @param center The center of the map projection.
#' @param quiet logical, whether to suppress messages.
#' @return data.frame with the suggested projection.
#' @keywords internal
#' @noRd

################################################################################
# regional map with a north-south extent
crs_ns_extent <- function(distortion, center, quiet = FALSE) {
  # Formatting the output based on the distortion
  if (distortion == "conformal") {
    crs_suggestions <- data.frame(
      prj = "tmerc",
      x0 = NA_real_,
      lat0 = NA_real_,
      lat1 = NA_real_,
      lat2 = NA_real_,
      lon0 = center$lng,
      k0 = NA_real_,
      description = "Transverse Mercator",
      notes = "Conformal projection for regional maps with a north-south extent"
    )
  } else if (distortion == "equal_area") {
    crs_suggestions <- data.frame(
      prj = "tcea",
      x0 = NA_real_,
      lat0 = NA_real_,
      lat1 = NA_real_,
      lat2 = NA_real_,
      lon0 = center$lng,
      k0 = NA_real_,
      description = "Transverse cylindrical equal-area",
      notes =
        "Equal-area projection for regional maps with a north-south extent"
    )
  }

  # Additional notes
  if (distortion == "equal_area" && !quiet) {
    message(
      paste0("To reduce overall distortion on the map, one can also compress ",
             "the map in the north-south direction (with a factor s) and ",
             "expand the map in the east-west direction (with a factor 1 / s).",
             " The factor s can be determined with a trial-and-error approach ",
             "comparing the distortion patterns along the center and at ",
             "the border of the map.")
    )
  } else if (distortion == "conformal" && !quiet) {
    message(
      paste0("To reduce overall area distortion on the map, one can also apply",
             " a scale factor k. Various values for k can be applied and the ",
             "area distortion patterns along the center and at the border ",
             "of the map are compared to select most appropriate value.")
    )
  }

  # Return the complete output
  return(crs_suggestions)
}
