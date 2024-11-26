#' Hemisphere projection
#'
#' Function to suggest a projection for maps showing a hemisphere
#'
#' @param distortion character string (e.g., "Equalarea", "Equidistant", "Compromise").
#' @param center data.frame with two numeric values, latitude and longitude of the center of the map.
#' @param scale numeric value, scale of the map.
#' @param latmin numeric value, minimum latitude of the map.
#' @param latmax numeric value, maximum latitude of the map.
#' @return data.frame with the suggested projection.
#' @keywords internal
#'
#'

################################################################################
# maps showing a hemisphere
crs_hemisphere <- function(distortion, center, scale, latmin, latmax) {

  angUnit <- "degrees" # TODO arbitrarily set to degrees, check where it comes from
  # we probably don't want to do any pretty formatting, so this could be removed.

  # Formatting central meridian
  lon <- round(center$lng * 100) / 100
  lonStr <- ""
  latStr <- ""

  # Checking if within tropics
  if (abs(latmax) < 23.43665 && abs(latmin) < 23.43665) {
    # Defining standard parallel
    interval <- (latmax - latmin) / 4
    latS1 <- center$lat + interval
    latS2 <- center$lat - interval

    latStd <- ifelse(
      (latS1 > 0 && latS2 > 0) || (latS1 < 0 && latS2 < 0),
      max(abs(latmax), abs(latmin)) / 2,
      0
    )
    latStd <- round(latStd * 100) / 100

    # Adding projection output
    if (distortion == "Equalarea") {
      prj_suggestions <- data.frame(prj="cea", x0=NA_real_, lat0=latStd, lat1=NA_real_, lat2=NA_real_, lon0=lon, k0=NA_real_,
                                    description = "Cylindrical equal-area", notes = "Equal-area projection for maps showing the tropics")
    } else if (distortion == "Conformal") {
      prj_suggestions <- data.frame(prj="merc", x0=NA_real_, lat0=latStd, lat1=NA_real_, lat2=NA_real_, lon0=lon, k0=NA_real_,
                                    description = "Mercator", notes = "Conformal projection for maps showing the tropics")
    } else if (distortion == "Equidistant") {
      prj_suggestions <- data.frame(prj="eqc", x0=NA_real_, lat0=latStd, lat1=NA_real_, lat2=NA_real_, lon0=lon, k0=NA_real_,
                                    description = "Equidistant cylindrical", notes = "Equidistant projection for maps showing the tropics - distance correct along meridians")
    }
    # TODO do we add these as attributed to the prj_summary data frame?
    #    outputText <- append(outputText, paste0("<p class='outputText'>Standard parallel: ", latStr, "</p>"))
    #    outputText <- append(outputText, paste0("<p class='outputText'>Central meridian: ", lonStr, "</p>"))

    #    previewMapLat0 <- 0
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
    if (distortion == "Equalarea") {
      prj_suggestions <- data.frame(prj="laea", x0=NA_real_, lat0=lat, lat1=NA_real_, lat2=NA_real_, lon0=lon, k0=NA_real_,
                                    description = "Lambert azimuthal equal-area", notes = "Equal-area projection for maps showing a hemisphere")
    } else if (distortion == "Equidistant") {
      prj_suggestions <- data.frame(prj="aeqd", x0=NA_real_, lat0=lat, lat1=NA_real_, lat2=NA_real_, lon0=lon, k0=NA_real_,
                                    description = "Azimuthal equidistant", notes = "Equidistant projection for maps showing a hemisphere")
    }
    # TODO do we add these as attributed to the prj_summary data frame?
    #    outputText <- append(outputText, paste0("<p class='outputText'>Center latitude: ", latStr, "</p>"))
    #    outputText <- append(outputText, paste0("<p class='outputText'>Center longitude: ", lonStr, "</p>"))

    #    previewMapLat0 <- lat
  }

  return(prj_suggestions)
}

