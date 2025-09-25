#' @title Projection of a small area (region or country)
#' @description maps showing a smaller area
#' @param distortion character string (e.g., "equal_area", "equidistant", "conformal").
#' @param center data.frame with two numeric values, latitude and longitude of the center of the map.
#' @param scale numeric value, scale of the map.
#' @param lonmin The minimum longitude of the map.
#' @param lonmax The maximum longitude of the map.
#' @param latmin numeric value, minimum latitude of the map.
#' @param latmax numeric value, maximum latitude of the map.
#' @param quiet logical, whether to suppress messages.
#' @return data.frame with the suggested projection.
#' @keywords internal

################################################################################
# maps showing a smaller area
crs_small_area <- function(
  distortion,
  center,
  scale,
  lonmin,
  lonmax,
  latmin,
  latmax,
  quiet = FALSE
) {
  # Computing longitude extent
  dlon <- lonmax - lonmin

  # Computing height-to-width ratio
  ratio <- (latmax - latmin) / dlon
  if (latmin > 0) {
    ratio <- ratio / cos(latmin * pi / 180)
  } else if (latmax < 0) {
    ratio <- ratio / cos(latmax * pi / 180)
  }

  # Handling various map properties and conditions
  if (distortion == "equidistant") {
    if (center$lat > 70) {
      crs_suggestions <- data.frame(
        prj = "aeqd",
        x0 = NA_real_,
        lat0 = 90,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Polar azimuthal equidistant",
        notes = "Distance correct along any line passing through the pole (i.e., meridian)"
      )
    } else if (center$lat < -70) {
      crs_suggestions <- data.frame(
        prj = "aeqd",
        x0 = NA_real_,
        lat0 = -90,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Polar azimuthal equidistant",
        notes = "Distance correct along any line passing through the pole (i.e., meridian)"
      )
    } else if (ratio > 1.25) {
      crs_suggestions <- data.frame(
        prj = "cass",
        x0 = NA_real_,
        lat0 = 0,
        lat1 = NA_real_,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Cassini",
        notes = "Distance correct along any line perpendicular to the central meridian"
      )
    } else if (abs(center$lat) < 15) {
      latS <- ifelse(
        latmax * latmin <= 0,
        max(abs(latmax), abs(latmin)) / 2,
        center$lat
      )
      crs_suggestions <- data.frame(
        prj = "eqc",
        x0 = NA_real_,
        lat0 = NA_real_,
        lat1 = latS,
        lat2 = NA_real_,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Equidistant cylindrical",
        notes = "Distance correct along meridians"
      )
    } else {
      # case: between pole and equator
      interval <- (latmax - latmin) / 6
      crs_suggestions <- data.frame(
        prj = "eqdc",
        x0 = NA_real_,
        lat0 = center$lat,
        lat1 = latmin + interval,
        lat2 = latmax - interval,
        lon0 = center$lng,
        k0 = NA_real_,
        description = "Equidistant conic",
        notes = "Distance correct along meridians"
      )

      crs_suggestions <- rbind(
        crs_suggestions,
        data.frame(
          prj = "aeqd",
          x0 = NA_real_,
          lat0 = center$lat,
          lat1 = NA_real_,
          lat2 = NA_real_,
          lon0 = center$lng,
          k0 = NA_real_,
          description = "Azimuthal equidistant",
          notes = "distance correct along any line passing through the center of the map (i.e., great circle)"
        )
      )
    }
  } else if (
    (latmin >= 84 && distortion == "conformal") ||
      (latmax <= -80 && distortion == "conformal")
  ) {
    # case: very large scale, Universal Polar Stereographic - North Pole
    previewMapLat0 <- ifelse(latmin >= 84, 90, -90)
    scaleFactor <- 0.994
    crs_suggestions <- data.frame(
      prj = "stere",
      x0 = NA_real_,
      lat0 = previewMapLat0,
      lat1 = NA_real_,
      lat2 = NA_real_,
      lon0 = center$lng,
      k0 = scaleFactor,
      description = "Polar stereographic",
      notes = "Conformal projection at very large map scale"
    )
  } else if (dlon <= 3 && distortion == "conformal") {
    previewMapLat0 <- 0
    scaleFactor <- 0.9999
    crs_suggestions <- data.frame(
      prj = "tmerc",
      x0 = 500000,
      lat0 = previewMapLat0,
      lat1 = NA_real_,
      lat2 = NA_real_,
      lon0 = center$lng,
      k0 = scaleFactor,
      description = "Transverse Mercator",
      notes = "Conformal projection at very large map scale"
    )
  } else if (dlon <= 6 && distortion == "conformal") {
    previewMapLat0 <- 0
    scaleFactor <- 0.9996
    crs_suggestions <- data.frame(
      prj = "tmerc",
      x0 = 500000,
      lat0 = previewMapLat0,
      lat1 = NA_real_,
      lat2 = NA_real_,
      lon0 = center$lng,
      k0 = scaleFactor,
      description = "Transverse Mercator",
      notes = "Conformal projection at very large map scale"
    )
  } else {
    if (ratio > 1.25) {
      # Regional map with north-south extent
      crs_suggestions <- crs_ns_extent(distortion, center)
    } else if (ratio < 0.8) {
      # Regional map with east-west extent
      crs_suggestions <- crs_ew_extent(
        distortion,
        center,
        scale,
        lonmin = lonmin,
        lonmax = lonmax,
        latmin = latmin,
        latmax = latmax
      )
    } else {
      # Regional map in square format
      crs_suggestions <- crs_square_format(distortion, center, latmin, latmax)
    }
  }

  if (scale > 260 && !quiet) {
    message("For maps at this scale, consider the state's official projection.")
  }

  return(crs_suggestions)
}
