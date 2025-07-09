#' Function to create the world equidistant projection
#'
#' @param center The center of the map (named vector with lat and long)
#' @param scale The scale of the map
#' @param round_cm The round central meridian.
#' @param prj_details a list of options defining the desired projection
#' @param quiet Whether to suppress messages and warnings
#' @return The projection object
#' @keywords internal

crs_world_equidistant <- function(center,
                                  scale,
                                  round_cm = FALSE,
                                  prj_details,
                                  quiet = FALSE) {
  # make sure that we have a projection element in prj_details
  if (!inherits(prj_details, "list") || !"prj" %in% names(prj_details)) {
    stop(paste0(
      "`world_equidistant` must be a list with a `prj` element",
      "and the appropriate projection details"
    ))
  }

  # Formatting output
  if (prj_details$prj == "polar") {
    # Polar azimuthal equidistant
    if (!"pole" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `pole` element")
    }
    # pole should be either 90 or -90
    if (prj_details$pole != 90 && prj_details$pole != -90) {
      stop("`pole` must be either 90 or -90")
    }
    pole_eq <- prj_details$pole
    if (!"lng_central" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lng_central` element")
    }
    lng_central <- prj_details$lng_central
    pole_str <- ifelse(pole_eq > 0, "North Pole", "South Pole")

    crs_suggestions <- data.frame(
      prj = "aeqd", x0 = NA_real_, lat0 = pole_eq, lat1 = NA_real_,
      lat2 = NA_real_, lon0 = lng_central, k0 = NA_real_,
      description = "Polar azimuthal equidistant",
      notes = paste0(
        "Distance correct through or from the ", pole_str,
        " Pole; Central meridian: ", lng_central, "."
      )
    )
  } else if (prj_details$prj == "oblique") {
    # Oblique azimuthal equidistant
    if (!"lat_center" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lat_center` element")
    }
    lat_center <- prj_details$lat_center
    if (!"lng_center" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lng_center` element")
    }
    lng_center <- prj_details$lng_center

    crs_suggestions <- data.frame(
      prj = "aeqd", x0 = NA_real_, lat0 = lat_center, lat1 = NA_real_,
      lat2 = NA_real_, lon0 = lng_center, k0 = NA_real_,
      description = "Oblique azimuthal equidistant",
      notes = paste0(
        "Distance correct through or from the center (",
        lng_center, ", ", lat_center, ")"
      )
    )
  } else if (prj_details$prj == "two_points") {
    # Two-point azimuthal equidistant
    if (!"lat1" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lat1` element")
    }
    lat1_eq <- round_world_coords(prj_details$lat1,
      scale = scale,
      round_cm = round_cm
    )
    if (!"lng1" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lng1` element")
    }
    lng1_eq <- round_world_coords(prj_details$lng1,
      scale = scale,
      round_cm = round_cm
    )
    if (!"lat2" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lat2` element")
    }
    lat2_eq <- round_world_coords(prj_details$lat2,
      scale = scale,
      round_cm = round_cm
    )
    if (!"lng2" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lng2` element")
    }
    lng2_eq <- round_world_coords(prj_details$lng2,
      scale = scale,
      round_cm = round_cm
    )

    crs_suggestions <- data.frame(
      prj = "tpeqd", x0 = NA_real_, lat0 = lat1_eq, lat1 = lng1_eq,
      lat2 = lat2_eq, lon0 = lng2_eq, k0 = NA_real_,
      description = "Two-point azimuthal equidistant",
      notes = paste0(
        "Distances are correct from two points: ",
        lng1_eq, ", ", lat1_eq, " and ", lng2_eq, ", ", lat2_eq
      )
    )
  } else {
    stop(
      "the `prj` element of world_equidistant` should be one of:\n",
      "'polar', 'oblique', 'two_points'"
    )
  }

  return(crs_suggestions)
}
