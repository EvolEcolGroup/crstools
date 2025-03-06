#' @title World map projections
#' @description This function provides a list of world map projections.
#' @param distortion The distortion of the world map projection (e.g., "equal_area", "equidistant", "compromise").
#' @param center The center of the map projection.
#' @param scale The scale of the map projection.
#' @param round_cm The round central meridian.
#' @return A data frame with the suggested world map projections.
#' @keywords internal

################################################################################
# Main small-scale (whole world) output function
crs_world <- function(distortion, center, scale, round_cm) {
  # Global list of world map projections
  listWorld <- list(
    # Equal-area world map projections with poles represented as points
    list(projection = "Mollweide", PROJ4 = "moll"),
    list(projection = "Hammer (or Hammer-Aitoff)", PROJ4 = "hammer"),

    # Equal-area world map projections with poles represented as lines
    list(projection = "Equal Earth", PROJ4 = "eqearth"),
    list(projection = "Eckert IV", PROJ4 = "eck4"),
    list(projection = "Wagner IV (or Putnins P2`)", PROJ4 = "wag4"),
    list(projection = "Wagner VII (or Hammer-Wagner)", PROJ4 = "wag7"),

    # compromise world map projections
    list(projection = "Natural Earth", PROJ4 = "natearth"),
    list(projection = "Robinson", PROJ4 = "robin"),
    list(projection = "Winkel Tripel", PROJ4 = "wintri"),
    list(projection = "Patterson", PROJ4 = "patterson"),
    list(projection = "Plate Carree", PROJ4 = "latlong"),
    list(projection = "Miller cylindrical I", PROJ4 = "mill")
  )

  prj_suggestions <- data.frame(
    prj = character(), x0 = double(), lat0 = double(), lat1 = double(), lat2 = double(), lon0 = double(), k0 = double(),
    description = character(), notes = character()
  )

  # Cleaning the output (initialize output text)
  outputTEXT <- ""

  # Formatting central meridian
  lng <- round_world_coords(center$lng, scale, round_cm)

  if (distortion == "equal_area") {
    # Equal-area projections with poles as lines
    for (i in 3:6) {
      projectionName <- listWorld[[i]]$projection
      proj4 <- listWorld[[i]]$PROJ4
      prj_suggestions <- rbind(
        prj_suggestions,
        data.frame(
          prj = proj4, x0 = NA_real_, lat0 = NA_real_, lat1 = NA_real_, lat2 = NA_real_, lon0 = lng, k0 = NA_real_,
          description = projectionName, notes = "Equal-area world map projection with poles represented as lines"
        )
      )
    }
    # Equal-area projections with poles as points
    for (i in 1:2) {
      projectionName <- listWorld[[i]]$projection
      proj4 <- listWorld[[i]]$PROJ4
      prj_suggestions <- rbind(
        prj_suggestions,
        data.frame(
          prj = proj4, x0 = NA_real_, lat0 = NA_real_, lat1 = NA_real_, lat2 = NA_real_, lon0 = lng, k0 = NA_real_,
          description = projectionName, notes = "Equal-area world map projection with poles represented as points"
        )
      )
    }
    # Add central meridian information
    #    worldCM(lng, outputTEXT)
    # TODO do we want to add central meridian information as an attribute to the prj_suggestions data frame?
  } else if (distortion == "equidistant") {
    # TODO DEBUG
    stop("equidistant for the whole world not implemented yet")

    # equidistant projections
    outputTEXT <- paste(outputTEXT, "<p><b>equidistant world map projections</b></p>", sep = "")

    # Dropdown menu for equidistant projections
    outputTEXT <- paste(outputTEXT,
      "<div><div><select name='worldequidistantMenu' id='worldequidistantMenu'>",
      "<option value='Polar azimuthal equidistant'><b>Polar azimuthal equidistant</b></option>",
      "<option value='Oblique azimuthal equidistant'><b>Oblique azimuthal equidistant</b></option>",
      "<option value='Two-point equidistant'><b>Two-point equidistant</b></option>",
      "</select></div><div id='worldequidistantBox'></div></div>",
      sep = ""
    )

    # Update active projection and preview
    outputWorldequidistantOption(center, scale)
  } else {
    # compromise projections

    # compromise projections loop
    for (i in 7:9) {
      projectionName <- listWorld[[i]]$projection
      proj4 <- listWorld[[i]]$PROJ4
      prj_suggestions <- rbind(
        prj_suggestions,
        data.frame(
          prj = proj4, x0 = NA_real_, lat0 = NA_real_, lat1 = NA_real_, lat2 = NA_real_, lon0 = lng, k0 = NA_real_,
          description = projectionName, notes = "compromise world map projection"
        )
      )
    }
    for (i in 10:12) {
      projectionName <- listWorld[[i]]$projection
      proj4 <- listWorld[[i]]$PROJ4
      prj_suggestions <- rbind(
        prj_suggestions,
        data.frame(
          prj = proj4, x0 = NA_real_, lat0 = NA_real_, lat1 = NA_real_, lat2 = NA_real_, lon0 = lng, k0 = NA_real_,
          description = projectionName, notes = "compromise rectangular world map projection"
        )
      )
    }

    # Add central meridian and additional notes
    #    worldCM(lng, outputTEXT)
    message("Rectangular projections are not generally recommended for most world maps.")
  }

  # Return the output
  return(prj_suggestions)
}
