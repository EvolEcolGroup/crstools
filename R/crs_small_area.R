#' @title Projection of a small area (region or country)
#' @description maps showing a smaller area
#' @param distortion character string (e.g., "Equalarea", "Equidistant", "Compromise").
#' @param center data.frame with two numeric values, latitude and longitude of the center of the map.
#' @param scale numeric value, scale of the map.
#' @param lonmin The minimum longitude of the map. Default is -180.
#' @param lonmax The maximum longitude of the map. Default is 180.
#' @param latmin numeric value, minimum latitude of the map.
#' @param latmax numeric value, maximum latitude of the map.
#' @return data.frame with the suggested projection.
#' @keywords internal

################################################################################
# maps showing a smaller area
crs_small_area <- function(distortion, center, scale, lonmin, lonmax, latmin, latmax) {

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
  if (distortion == "Equidistant") {
    #    outputText <- c(outputText, "<p><b>Regional map projection with correct scale along some lines.</b></p>")

    if (center$lat > 70) {
      # previewMapProjection <- "Azimuthal equidistant"
      # previewMapLat0 <- 90
      # outputText <- c(outputText, sprintf(
      #   "<p><span data-proj-name='%s'>Polar azimuthal equidistant</span> - distance correct along any line passing through the pole (i.e., meridian)<br>Central meridian: %s</p>",
      #   previewMapProjection, lng
      # ))
      prj_suggestions <- data.frame(prj="aeqd", x0=NA_real_, lat0=90, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Polar azimuthal equidistant", notes = "Distance correct along any line passing through the pole (i.e., meridian)")
    } else if (center$lat < -70) {
      # previewMapProjection <- "Azimuthal equidistant"
      # previewMapLat0 <- -90
      # outputText <- c(outputText, sprintf(
      #   "<p><span data-proj-name='%s'>Polar azimuthal equidistant</span> - distance correct along any line passing through the pole (i.e., meridian)<br>Central meridian: %s</p>",
      #   previewMapProjection, lng
      # ))
      prj_suggestions <- data.frame(prj="aeqd", x0=NA_real_, lat0=-90, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Polar azimuthal equidistant", notes = "Distance correct along any line passing through the pole (i.e., meridian)")
    } else if (ratio > 1.25) {
      # previewMapProjection <- "Cassini"
      # previewMapLat0 <- 0
      # outputText <- c(outputText, sprintf(
      #   "<p><span data-proj-name='%s'>Cassini</span> - distance correct along any line perpendicular to the central meridian<br>Central meridian: %s</p>",
      #   previewMapProjection, lng
      # ))
      prj_suggestions <- data.frame(prj="cass", x0=NA_real_, lat0=0, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Cassini", notes = "Distance correct along any line perpendicular to the central meridian")
    } else if (abs(center$lat) < 15) {
      # previewMapProjection <- "Equidistant cylindrical"
      # previewMapLat0 <- 0
      # latS <- ifelse(latmax * latmin <= 0, max(abs(latmax), abs(latmin)) / 2, center$lat)
      # outputText <- c(outputText, sprintf(
      #   "<p><span data-proj-name='%s'>Equidistant cylindrical</span> - distance correct along meridians<br>Standard parallel: %.2f<br>Central meridian: %s</p>",
      #   previewMapProjection, latS, lng
      # ))
      prj_suggestions <- data.frame(prj="eqc", x0=NA_real_, lat0=latS, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Equidistant cylindrical", notes = "Distance correct along meridians")
    } else { # case: between pole and equator
      interval <- (latmax - latmin) / 6
      #latOr <- center$lat
      #latS1 <- latmin + interval
      #latS2 <- latmax - interval
      # activeProjection <- activeEqDistProj
      # previewMapLat0 <- center$lat
      # outputText <- c(outputText, sprintf(
      #   "<p><span data-proj-name='Equidistant conic'><b>Equidistant conic</b></span> - distance correct along meridians</span></p><p>Latitude of origin: %.2f<br>Standard parallel 1: %.2f<br>Standard parallel 2: %.2f<br>Central meridian: %s</p>",
      #   latOr, latS1, latS2, lng
      # ))
      prj_suggestions <- data.frame(prj="eqdc", x0=NA_real_, lat0=center$lat,  lat1=latmin + interval, lat2=latmax - interval, lon0=center$lng, k0=NA_real_,
                                    description = "Equidistant conic", notes = "Distance correct along meridians")
    }

  } else if ((latmin >= 84 && distortion == "Conformal") || (latmax <= -80 && distortion == "Conformal")) {
    # case: very large scale, Universal Polar Stereographic - North Pole
    # previewMapProjection <- "Stereographic"
    # previewMapLat0 <- ifelse(latmin >= 84, 90, -90)
    # scaleFactor <- 0.994
    # outputText <- c(outputText, sprintf(
    #   "<p><b>Conformal projection at very large map scale</b></p><p><span data-proj-name='%s'>Polar stereographic</span><br>Central meridian: %s<br>Scale factor: %.3f</p>",
    #   previewMapProjection, lng, scaleFactor
    # ))
    prj_suggestions <- data.frame(prj="stere", x0=NA_real_, lat0=previewMapLat0, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=scaleFactor,
                                  description = "Polar stereographic", notes = "Conformal projection at very large map scale")
  } else if (dlon <= 3 && distortion == "Conformal") {
    # case: very large scale, Universal Polar Stereographic - South Pole
    # previewMapProjection <- "Transverse Mercator"
    # previewMapLat0 <- 0
    # scaleFactor <- 0.9999
    # outputText <- c(outputText, sprintf(
    #   "<p><b>Conformal projection at very large map scale</b></p><p><span data-proj-name='%s'>Transverse Mercator</span><br>Central meridian: %s<br>Scale factor: %.4f</p>",
    #   previewMapProjection, lng, scaleFactor
    # ))
    prj_suggestions <- data.frame(prj="tmerc", x0=NA_real_, lat0=previewMapLat0, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=scaleFactor,
                                  description = "Transverse Mercator", notes = "Conformal projection at very large map scale")

  } else if (dlon <= 3 && distortion == "Conformal") {
    #    previewMapProjection <- "Transverse Mercator"
    previewMapLat0 <- 0
    scaleFactor <- 0.9999
    # outputText <- c(outputText, sprintf(
    #   "<p><b>Conformal projection at very large map scale</b></p><p><span data-proj-name='%s'>Transverse Mercator</span><br>Central meridian: %s<br>Scale factor: %.4f</p>",
    #   previewMapProjection, lng, scaleFactor
    # ))
    prj_suggestions <- data.frame(prj="tmerc", x0=NA_real_, lat0=previewMapLat0, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=scaleFactor,
                                  description = "Transverse Mercator", notes = "Conformal projection at very large map scale")
  } else if (dlon <= 6 && distortion == "Conformal") {
    #    previewMapProjection <- "Transverse Mercator"
    previewMapLat0 <- 0
    scaleFactor <- 0.9996
    # outputText <- c(outputText, sprintf(
    #   "<p><b>Conformal projection at very large map scale</b></p><p><span data-proj-name='%s'>Transverse Mercator</span><br>Central meridian: %s<br>Scale factor: %.4f</p>",
    #   previewMapProjection, lng, scaleFactor
    # ))
    prj_suggestions <- data.frame(prj="tmerc", x0=NA_real_, lat0=previewMapLat0, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=scaleFactor,
                                  description = "Transverse Mercator", notes = "Conformal projection at very large map scale")
  } else {
    if (ratio > 1.25) {
      # Regional map with north-south extent
      prj_suggestions <- crs_ns_extent(distortion, center)
    } else if (ratio < 0.8) {
      # Regional map with east-west extent
      prj_suggestions <- crs_ew_extent(distortion, center, scale, lonmin =lonmin, lonmax = lonmax, latmin = latmin, latmax = latmax)
    } else {
      # Regional map in square format
      prj_suggestions <- crs_square_format(distortion, center)
    }
  }

  if (scale > 260) {
    message("For maps at this scale, consider the state's official projection.")
  }

  return(prj_suggestions)
}
