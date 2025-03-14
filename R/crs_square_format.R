#' @title Projection of a square map
#' @description Projections for a small map with square format
#' @param distortion The distortion of the world map projection (e.g., "equal_area", "equidistant", "compromise").
#' @param center data.frame with two numeric values, latitude and longitude of the center of the map.
#' @param latmin The minimum latitude of the map. Default is -90.
#' @param latmax The maximum latitude of the map. Default is 90.
#' @return data.frame with the suggested projection.
#' @keywords internal
#'

################################################################################
## crs for regional maps with square extent
crs_square_format <- function(distortion, center, latmin, latmax) {
  # Initialize output as an empty vector for storing HTML strings
  # outputText <- c()


  # Add conformal or equal-area description
  # if (distortion == "conformal") {
  #   outputText <- c(outputText, "<p><b>conformal projection for regional maps in square format</b></p>")
  #   previewMapProjection <- activeProjection <- "Stereographic"
  # } else if (distortion == "equal_area") {
  #   outputText <- c(outputText, "<p><b>Equal-area projection for regional maps in square format</b></p>")
  #   previewMapProjection <- activeProjection <- "Lambert azimuthal equal area"
  # }

  # Handle cases based on the latitude of the center
  if (center$lat > 75) {
    previewMapLat0 <- 90
    if (distortion == "conformal") {
      # outputText <- c(
      #   outputText,
      #   sprintf(
      #     "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
      #     activeProjection, stringLinks("stere", NA, 90.0, NA, NA, center$lng, NA)
      #   )
      # )
      prj_suggestions <- data.frame(
        prj = "stere", x0 = NA_real_, lat0 = 90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar stereographic", notes = "Conformal projection for regional maps in square format"
      )
    } else if (distortion == "equal_area") {
      #   outputText <- c(
      #     outputText,
      #     sprintf(
      #       "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
      #       activeProjection, stringLinks("laea", NA, 90.0, NA, NA, center$lng, NA)
      #     )
      #   )
      prj_suggestions <- data.frame(
        prj = "laea", x0 = NA_real_, lat0 = 90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps in square format"
      )
    }
    # outputText <- c(outputText, sprintf("<p class='outputText'>Central meridian: %s</p>", lng))
  } else if (center$lat < -75) {
    previewMapLat0 <- -90
    if (distortion == "conformal") {
      # outputText <- c(
      #   outputText,
      #   sprintf(
      #     "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
      #     activeProjection, stringLinks("stere", NA, -90.0, NA, NA, center$lng, NA)
      #   )
      # )
      prj_suggestions <- data.frame(
        prj = "stere", x0 = NA_real_, lat0 = -90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar stereographic", notes = "Conformal projection for regional maps in square format"
      )
    } else if (distortion == "equal_area") {
      #   outputText <- c(
      #     outputText,
      #     sprintf(
      #       "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
      #       activeProjection, stringLinks("laea", NA, -90.0, NA, NA, center$lng, NA)
      #     )
      #   )
      prj_suggestions <- data.frame(
        prj = "laea", x0 = NA_real_, lat0 = -90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps in square format"
      )
    }
    # outputText <- c(outputText, sprintf("<p class='outputText'>Central meridian: %s</p>", lng))
  } else if (abs(center$lat) < 15 && (latmax * latmin) <= 0) {
    previewMapLat0 <- 0
    if (distortion == "conformal") {
      # outputText <- c(
      #   outputText,
      #   sprintf(
      #     "<p class='outputText'><span data-proj-name='%s'>Equatorial stereographic</span>%s</p>",
      #     activeProjection, stringLinks("stere", NA, 0.0, NA, NA, center$lng, NA)
      #   )
      # )
      prj_suggestions <- data.frame(
        prj = "stere", x0 = NA_real_, lat0 = 0, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Equatorial stereographic", notes = "Conformal projection for regional maps in square format"
      )
    } else if (distortion == "equal_area") {
      # outputText <- c(
      #   outputText,
      #   sprintf(
      #     "<p class='outputText'><span data-proj-name='%s'>Equatorial Lambert azimuthal equal-area</span>%s</p>",
      #     activeProjection, stringLinks("laea", NA, 0.0, NA, NA, center$lng, NA)
      #   )
      # )
      prj_suggestions <- data.frame(
        prj = "laea", x0 = NA_real_, lat0 = 0, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Equatorial Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps in square format"
      )
    }
    # outputText <- c(outputText, sprintf("<p class='outputText'>Central meridian: %s</p>", lng))
  } else {
    # Case: Between the pole and equator
    # centerText <- sprintf(
    #   "Center latitude: %.2f<br>Center longitude: %s",
    #   center$lat, lng
    # )
    # previewMapLat0 <- center$lat

    if (distortion == "conformal") {
      # outputText <- c(
      #   outputText,
      #   sprintf(
      #     "<p class='outputText'><span data-proj-name='%s'>Oblique stereographic</span>%s</p>",
      #     activeProjection, stringLinks("stere", NA, center$lat, NA, NA, center$lng, NA)
      #   )
      # )
      prj_suggestions <- data.frame(
        prj = "stere", x0 = NA_real_, lat0 = center$lat, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Oblique stereographic", notes = "Conformal projection for regional maps in square format"
      )
    } else if (distortion == "equal_area") {
      # outputText <- c(
      #   outputText,
      #   sprintf(
      #     "<p class='outputText'><span data-proj-name='%s'>Oblique Lambert azimuthal equal-area</span>%s</p>",
      #     activeProjection, stringLinks("laea", NA, center$lat, NA, NA, center$lng, NA)
      #   )
      # )
      prj_suggestions <- data.frame(
        prj = "laea", x0 = NA_real_, lat0 = center$lat, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Oblique Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps in square format"
      )
    }
    # outputText <- c(outputText, sprintf("<p class='outputText'>%s</p>", centerText))
  }

  # Include any notes about the scale factor
  if (distortion == "conformal") {
    message("To reduce overall area distortion on the map, one can also apply a scale factor.
  Various values for can be applied and the area distortion patterns along the center
  and at the border of the map are compared to select most appropriate value.")
  }


  # Return the dataframe of suggestions
  return(prj_suggestions)
}
