#' @title North-South extent
#' @description Checking the North-South extent of a regional map
#' @param distortion character string (e.g., "equal_area", "equidistant", "compromise").
#' @param center The center of the map projection.
#' @param quiet logical, whether to suppress messages.
#' @return data.frame with the suggested projection.
#' @keywords internal

################################################################################
# regional map with a north-south extent
crs_ns_extent <- function(distortion, center, quiet = FALSE) {
  # Initialize output as an empty vector for storing HTML strings
  outputText <- c()


  # Formatting the output based on the distortion
  if (distortion == "conformal") {
    # previewMapProjection <- activeProjection <- "Transverse Mercator"
    # outputText <- c(
    #   outputText,
    #   "<p><b>conformal projection for regional maps with a north-south extent</b></p>",
    #   sprintf(
    #     "<p class='outputText'><span data-proj-name='%s'>Transverse Mercator</span>%s</p>",
    #     activeProjection, stringLinks("tmerc", NA, NA, NA, NA, center$lng, NA)
    #   )
    # )
    crs_suggestions <- data.frame(
      prj = "tmerc", x0 = NA_real_, lat0 = NA_real_, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
      description = "Transverse Mercator", notes = "Conformal projection for regional maps with a north-south extent"
    )
  } else if (distortion == "equal_area") {
    # previewMapProjection <- activeProjection <- "Transverse cylindrical equal area"
    # outputText <- c(
    #   outputText,
    #   "<p><b>Equal-area projection for regional maps with a north-south extent</b></p>",
    #   sprintf(
    #     "<p class='outputText'><span data-proj-name='%s'>Transverse cylindrical equal-area</span>%s</p>",
    #     activeProjection, stringLinks("tcea", NA, NA, NA, NA, center$lng, NA)
    #   )
    # )
    crs_suggestions <- data.frame(
      prj = "tcea", x0 = NA_real_, lat0 = NA_real_, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
      description = "Transverse cylindrical equal-area", notes = "Equal-area projection for regional maps with a north-south extent"
    )
  }

  # Add the central meridian information
  # outputText <- c(outputText, sprintf("<p class='outputText'>Central meridian: %s</p>", lng))

  # Set the latitude origin for the map preview
  # previewMapLat0 <- 0

  # Include notes about the scale factor
  # outputText <- c(outputText, printScaleFactorNote(distortion))
  # printScaleFactorNote(distortion)

  # Additional note for equal-area projection
  if (distortion == "equal_area" && !quiet) {
    # outputText <- c(
    #   outputText,
    #   "<p><b>Note:</b> To reduce overall distortion on the map, one can also compress the map in the north-south direction (with a factor <i>s</i>) and expand the map in the east-west direction (with a factor 1 / <i>s</i>). The factor <i>s</i> can be determined with a trial-and-error approach, comparing the distortion patterns along the center and at the border of the map.</p>"
    # )
    message("To reduce overall distortion on the map, one can also compress the map in the north-south direction (with a factor s) and expand the map in the east-west direction (with a factor 1 / s). The factor s can be determined with a trial-and-error approach, comparing the distortion patterns along the center and at the border of the map.")
  }

  # Return the complete output
  return(crs_suggestions)
}
