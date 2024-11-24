#' Suggestions for map projections depending on the extent
#'
#' This function provides suggestions for map projections depending on the extent
#' of the map. For SDMs, an equal area projection is generally favoured, but
#' it is also possible to get suggestions for conformal, equidistant or
#' compromise projections. The algorithm is a reimplementation in R of
#' Projection Wizard (Bojan et al., 2016), version 2.1.
#'
#' @references Bojan, S, Bernhard, J, & Helen, J (2016) Projection Wizard - An
#' Online Map Projection Selection Tool. The Cartographic Journal 53(2):177-185
#' DOI: 10.1080/00087041.2015.1131938
#' @param distortion The type of distortion to be minimized. Options are
#' "Equalarea", "Conformal", "Equidistant" and "Compromise". Default is
#' "Equalarea".
#' @param lonmin The minimum longitude of the map. Default is -180.
#' @param lonmax The maximum longitude of the map. Default is 180.
#' @param latmin The minimum latitude of the map. Default is -90.
#' @param latmax The maximum latitude of the map. Default is 90.
#' @param roundCM Logical. If TRUE, the central meridian is rounded to the
#' nearest degree. Default is FALSE.
#' @return A data frame with the suggested map projections.
#' @examples
#' # Whole map
#' crs_wizard(lonmin = -180, lonmax = 180,latmin = -90, latmax = 90)
#' # Northen Hemisphere
#' crs_wizard(lonmin = -180, lonmax = 180, latmin = 21, latmax = 70)
#' # Hemisphere showing the tropics
#' crs_wizard(lonmin = -180, lonmax = 180, latmin = -7, latmax = 21)
#' # Regional map for EW extent
#' crs_wizard(lonmin = -60, lonmax = 20, latmin = 40, latmax = 70)
#' @export
#'

crs_wizard <- function(distortion = c("Equalarea","Comformal","Equidistant","Compromise"),
                       lonmin = -180, lonmax = 180, latmin = -90, latmax = 90,
                       roundCM = FALSE
                       ) {
  # check if the input is correct
  distortion <- match.arg(distortion)

  # Computing the scale of the map
  scale <- 720 / (lonmax - lonmin) / (sin(latmax * pi / 180) - sin(latmin * pi / 180))

  # Getting the center of the map
  center <- data.frame(lat = (latmax + latmin) / 2, lng = (lonmax + lonmin) / 2)

  # Normalizing central meridian value
  center$lng <- normalizeLON(center$lng, 0)

  # Rounding central meridian
  if (roundCM) {
    center$lng <- round(center$lng)
  }

  # Choose the appropriate type of map
  if (scale < 1.5) {  # World (small-scale) map
    if (distortion=="Conformal") {
      stop("Conformal is not available for maps covering the whole world; try Equalarea instead")
    }
    crs_world(distortion, center, scale, roundCM)

  } else if (scale < 6) { # Hemisphere (medium-scale) map
    # if the map is NOT focussing on the tropics
    if (!(abs(latmax) < 23.43665 && abs(latmin) < 23.43665)) {
      if (distortion=="Conformal") {
        stop("Conformal is not available for maps covering a whole hemisphere; try Equalarea instead")
      }
    }
    if (distortion=="Compromise") {
      stop("Compromise is not available for maps covering a whole hemisphere; try Equalarea instead")
    }
    crs_hemisphere(distortion, center, scale,  latmin = latmin, latmax =latmax)

  } else { # Continent or a smaller area (large-scale) map
    if (distortion=="Compromise") {
      stop("Compromise is not available for maps focussing on a single continent or small area; try Equalarea instead")
    }
    crs_small_area(distortion, center, scale,
                   lonmin = lonmin, lonmax = lonmax, latmin = latmin, latmax =latmax)
  }
}
################################################################################
# Main small-scale (whole world) output function
crs_world <- function(property, center, scale, roundCM) {
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

    # Compromise world map projections
    list(projection = "Natural Earth", PROJ4 = "natearth"),
    list(projection = "Robinson", PROJ4 = "robin"),
    list(projection = "Winkel Tripel", PROJ4 = "wintri"),
    list(projection = "Patterson", PROJ4 = "patterson"),
    list(projection = "Plate Carree", PROJ4 = "latlong"),
    list(projection = "Miller cylindrical I", PROJ4 = "mill")
  )

  prj_suggestions <- data.frame(prj=character(), x0=double(), lat0=double(), lat1=double(), lat2=double(), lon0=double(), k0=double(),
                                description = character(), notes=character())

  # Cleaning the output (initialize output text)
  outputTEXT <- ""

  # Formatting central meridian
  lng <- worldValues(center$lng, scale, roundCM)

  if (property == "Equalarea") {
    # Equal-area projections with poles as lines
    for (i in 3:6) {
      projectionName <- listWorld[[i]]$projection
      proj4 <- listWorld[[i]]$PROJ4
      prj_suggestions <- rbind(prj_suggestions,
                              data.frame(prj=proj4, x0=NA_real_, lat0=NA_real_, lat1=NA_real_, lat2=NA_real_, lon0=lng, k0=NA_real_,
                                         description = projectionName, notes = "Equal-area world map projection with poles represented as lines"))
    }
    # Equal-area projections with poles as points
    for (i in 1:2) {
      projectionName <- listWorld[[i]]$projection
      proj4 <- listWorld[[i]]$PROJ4
      prj_suggestions <- rbind(prj_suggestions,
                               data.frame(prj=proj4, x0=NA_real_, lat0=NA_real_, lat1=NA_real_, lat2=NA_real_, lon0=lng, k0=NA_real_,
                                          description = projectionName, notes = "Equal-area world map projection with poles represented as points"))
    }
    # Add central meridian information
#    worldCM(lng, outputTEXT)
    # TODO do we want to add central meridian information as an attribute to the prj_suggestions data frame?

  } else if (property == "Equidistant") {
    # TODO DEBUG
    stop("Equidistant for the whole world not implemented yet")

    # Equidistant projections
    outputTEXT <- paste(outputTEXT, "<p><b>Equidistant world map projections</b></p>", sep = "")

    # Dropdown menu for equidistant projections
    outputTEXT <- paste(outputTEXT,
                        "<div><div><select name='worldEquidistantMenu' id='worldEquidistantMenu'>",
                        "<option value='Polar azimuthal equidistant'><b>Polar azimuthal equidistant</b></option>",
                        "<option value='Oblique azimuthal equidistant'><b>Oblique azimuthal equidistant</b></option>",
                        "<option value='Two-point equidistant'><b>Two-point equidistant</b></option>",
                        "</select></div><div id='worldEquidistantBox'></div></div>",
                        sep = ""
    )

    # Update active projection and preview
    outputWorldEquidistantOption(center, scale)
  } else {
    # Compromise projections

    # Compromise projections loop
    for (i in 7:9) {
      projectionName <- listWorld[[i]]$projection
      proj4 <- listWorld[[i]]$PROJ4
      prj_suggestions <- rbind(prj_suggestions,
                              data.frame(prj=proj4, x0=NA_real_, lat0=NA_real_, lat1=NA_real_, lat2=NA_real_, lon0=lng, k0=NA_real_,
                                         description = projectionName, notes = "Compromise world map projection"))
    }
    for (i in 10:12) {
      projectionName <- listWorld[[i]]$projection
      proj4 <- listWorld[[i]]$PROJ4
      prj_suggestions <- rbind(prj_suggestions,
                              data.frame(prj=proj4, x0=NA_real_, lat0=NA_real_, lat1=NA_real_, lat2=NA_real_, lon0=lng, k0=NA_real_,
                                         description = projectionName, notes = "Compromise rectangular world map projection"))
    }

    # Add central meridian and additional notes
#    worldCM(lng, outputTEXT)
      message("Rectangular projections are not generally recommended for most world maps.")
      }

  # Return the output
  return(prj_suggestions)
}

################################################################################
# maps showing a hemisphere
crs_hemisphere <- function(property, center, scale, latmin, latmax) {

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
    if (property == "Equalarea") {
      prj_suggestions <- data.frame(prj="cea", x0=NA_real_, lat0=latStd, lat1=NA_real_, lat2=NA_real_, lon0=lon, k0=NA_real_,
                                description = "Cylindrical equal-area", notes = "Equal-area projection for maps showing the tropics")
    } else if (property == "Conformal") {
      prj_suggestions <- data.frame(prj="merc", x0=NA_real_, lat0=latStd, lat1=NA_real_, lat2=NA_real_, lon0=lon, k0=NA_real_,
                                description = "Mercator", notes = "Conformal projection for maps showing the tropics")
    } else if (property == "Equidistant") {
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
    if (property == "Equalarea") {
      prj_suggestions <- data.frame(prj="laea", x0=NA_real_, lat0=lat, lat1=NA_real_, lat2=NA_real_, lon0=lon, k0=NA_real_,
                                    description = "Lambert azimuthal equal-area", notes = "Equal-area projection for maps showing a hemisphere")
    } else if (property == "Equidistant") {
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

################################################################################
# maps showing a smaller area
crs_small_area <- function(property, center, scale, lonmin, lonmax, latmin, latmax,  angUnit = "degrees") {

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
  if (property == "Equidistant") {
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

  } else if ((latmin >= 84 && property == "Conformal") || (latmax <= -80 && property == "Conformal")) {
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
  } else if (dlon <= 3 && property == "Conformal") {
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

  } else if (dlon <= 3 && property == "Conformal") {
#    previewMapProjection <- "Transverse Mercator"
    previewMapLat0 <- 0
    scaleFactor <- 0.9999
    # outputText <- c(outputText, sprintf(
    #   "<p><b>Conformal projection at very large map scale</b></p><p><span data-proj-name='%s'>Transverse Mercator</span><br>Central meridian: %s<br>Scale factor: %.4f</p>",
    #   previewMapProjection, lng, scaleFactor
    # ))
    prj_suggestions <- data.frame(prj="tmerc", x0=NA_real_, lat0=previewMapLat0, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=scaleFactor,
                                  description = "Transverse Mercator", notes = "Conformal projection at very large map scale")
  } else if (dlon <= 6 && property == "Conformal") {
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
      prj_suggestions <- printNSextent(property, center)
    } else if (ratio < 0.8) {
      # Regional map with east-west extent
      prj_suggestions <- printEWextent(property, center, scale, lonmin =lonmin, lonmax = lonmax, latmin = latmin, latmax = latmax)
    } else {
      # Regional map in square format
      prj_suggestions <- printSquareFormat(property, center)
    }
  }

  if (scale > 260) {
    message("For maps at this scale, consider the state's official projection.")
  }

  return(prj_suggestions)
}

################################################################################
## crs for regional maps with square extent
printSquareFormat <- function(property, center, latmin, latmax) {
  # Initialize output as an empty vector for storing HTML strings
  outputText <- c()


  # Add conformal or equal-area description
  if (property == "Conformal") {
    outputText <- c(outputText, "<p><b>Conformal projection for regional maps in square format</b></p>")
    previewMapProjection <- activeProjection <- "Stereographic"
  } else if (property == "Equalarea") {
    outputText <- c(outputText, "<p><b>Equal-area projection for regional maps in square format</b></p>")
    previewMapProjection <- activeProjection <- "Lambert azimuthal equal area"
  }

  # Handle cases based on the latitude of the center
  if (center$lat > 75) {
    previewMapLat0 <- 90
    if (property == "Conformal") {
      outputText <- c(
        outputText,
        sprintf(
          "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
          activeProjection, stringLinks("stere", NA, 90.0, NA, NA, center$lng, NA)
        )
      )
    } else if (property == "Equalarea") {
      outputText <- c(
        outputText,
        sprintf(
          "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
          activeProjection, stringLinks("laea", NA, 90.0, NA, NA, center$lng, NA)
        )
      )
    }
    outputText <- c(outputText, sprintf("<p class='outputText'>Central meridian: %s</p>", lng))
  } else if (center$lat < -75) {
    previewMapLat0 <- -90
    if (property == "Conformal") {
      outputText <- c(
        outputText,
        sprintf(
          "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
          activeProjection, stringLinks("stere", NA, -90.0, NA, NA, center$lng, NA)
        )
      )
    } else if (property == "Equalarea") {
      outputText <- c(
        outputText,
        sprintf(
          "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
          activeProjection, stringLinks("laea", NA, -90.0, NA, NA, center$lng, NA)
        )
      )
    }
    outputText <- c(outputText, sprintf("<p class='outputText'>Central meridian: %s</p>", lng))
  } else if (abs(center$lat) < 15 && (latmax * latmin) <= 0) {
    previewMapLat0 <- 0
    if (property == "Conformal") {
      outputText <- c(
        outputText,
        sprintf(
          "<p class='outputText'><span data-proj-name='%s'>Equatorial stereographic</span>%s</p>",
          activeProjection, stringLinks("stere", NA, 0.0, NA, NA, center$lng, NA)
        )
      )
    } else if (property == "Equalarea") {
      outputText <- c(
        outputText,
        sprintf(
          "<p class='outputText'><span data-proj-name='%s'>Equatorial Lambert azimuthal equal-area</span>%s</p>",
          activeProjection, stringLinks("laea", NA, 0.0, NA, NA, center$lng, NA)
        )
      )
    }
    outputText <- c(outputText, sprintf("<p class='outputText'>Central meridian: %s</p>", lng))
  } else {
    # Case: Between the pole and equator
    centerText <- sprintf(
      "Center latitude: %.2f<br>Center longitude: %s",
      center$lat, lng
    )
    previewMapLat0 <- center$lat

    if (property == "Conformal") {
      outputText <- c(
        outputText,
        sprintf(
          "<p class='outputText'><span data-proj-name='%s'>Oblique stereographic</span>%s</p>",
          activeProjection, stringLinks("stere", NA, center$lat, NA, NA, center$lng, NA)
        )
      )
    } else if (property == "Equalarea") {
      outputText <- c(
        outputText,
        sprintf(
          "<p class='outputText'><span data-proj-name='%s'>Oblique Lambert azimuthal equal-area</span>%s</p>",
          activeProjection, stringLinks("laea", NA, center$lat, NA, NA, center$lng, NA)
        )
      )
    }
    outputText <- c(outputText, sprintf("<p class='outputText'>%s</p>", centerText))
  }

  # Include any notes about the scale factor
  if (property == "Conformal") {
    message("To reduce overall area distortion on the map, one can also apply a scale factor <i>k</i>.
  Various values for <i>k</i> can be applied and the area distortion patterns along the center
  and at the border of the map are compared to select most appropriate value.")
  }


  # Return the complete output
  return(paste(outputText, collapse = "\n"))
}


################################################################################
# regional map with a north-south extent
printNSextent <- function(property, center) {
  # Initialize output as an empty vector for storing HTML strings
  outputText <- c()


  # Formatting the output based on the property
  if (property == "Conformal") {
    # previewMapProjection <- activeProjection <- "Transverse Mercator"
    # outputText <- c(
    #   outputText,
    #   "<p><b>Conformal projection for regional maps with a north-south extent</b></p>",
    #   sprintf(
    #     "<p class='outputText'><span data-proj-name='%s'>Transverse Mercator</span>%s</p>",
    #     activeProjection, stringLinks("tmerc", NA, NA, NA, NA, center$lng, NA)
    #   )
    # )
    crs_suggestions <- data.frame(prj="tmerc", x0=NA_real_, lat0=NA_real_, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                  description = "Transverse Mercator", notes = "Conformal projection for regional maps with a north-south extent")
  } else if (property == "Equalarea") {
    # previewMapProjection <- activeProjection <- "Transverse cylindrical equal area"
    # outputText <- c(
    #   outputText,
    #   "<p><b>Equal-area projection for regional maps with a north-south extent</b></p>",
    #   sprintf(
    #     "<p class='outputText'><span data-proj-name='%s'>Transverse cylindrical equal-area</span>%s</p>",
    #     activeProjection, stringLinks("tcea", NA, NA, NA, NA, center$lng, NA)
    #   )
    # )
    crs_suggestions <- data.frame(prj="tcea", x0=NA_real_, lat0=NA_real_, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                  description = "Transverse cylindrical equal-area", notes = "Equal-area projection for regional maps with a north-south extent")
  }

  # Add the central meridian information
  #outputText <- c(outputText, sprintf("<p class='outputText'>Central meridian: %s</p>", lng))

  # Set the latitude origin for the map preview
  #previewMapLat0 <- 0

  # Include notes about the scale factor
  #outputText <- c(outputText, printScaleFactorNote(property))
  printScaleFactorNote(property)

  # Additional note for equal-area projection
  if (property == "Equalarea") {
    # outputText <- c(
    #   outputText,
    #   "<p><b>Note:</b> To reduce overall distortion on the map, one can also compress the map in the north-south direction (with a factor <i>s</i>) and expand the map in the east-west direction (with a factor 1 / <i>s</i>). The factor <i>s</i> can be determined with a trial-and-error approach, comparing the distortion patterns along the center and at the border of the map.</p>"
    # )
    message("To reduce overall distortion on the map, one can also compress the map in the north-south direction (with a factor s) and expand the map in the east-west direction (with a factor 1 / s). The factor s can be determined with a trial-and-error approach, comparing the distortion patterns along the center and at the border of the map.")
  }

  # Return the complete output
  return(paste(outputText, collapse = "\n"))
}

################################################################################
printEWextent <- function(property, center, scale,
                          lonmin, lonmax, latmin, latmax) {

  # Flag to determine if scale note should be included
  scaleNote <- FALSE

  # Add the initial output based on the property
  if (property == "Conformal") {
    message("Conformal projection for regional maps with an east-west extent")
  } else if (property == "Equalarea") {
    message("Equal-area projection for regional maps with an east-west extent")
  }


  # Case: Close to poles
  if (center$lat > 70) {
 #   previewMapLat0 <- 90
    if (property == "Conformal") {
  #    previewMapProjection <- activeProjection <- "Stereographic"
      scaleNote <- TRUE
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
      #   activeProjection, stringLinks("stere", NA, 90.0, NA, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(prj="stere", x0=NA_real_, lat0=90, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Polar stereographic", notes = "Conformal projection for regional maps with an east-west extent")
    } else if (property == "Equalarea") {
      # previewMapProjection <- activeProjection <- "Lambert azimuthal equal area"
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
      #   activeProjection, stringLinks("laea", NA, 90.0, NA, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(prj="laea", x0=NA_real_, lat0=90, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Polar Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps with an east-west extent")
    }
  } else if (center$lat < -70) {
    previewMapLat0 <- -90
    if (property == "Conformal") {
#      previewMapProjection <- activeProjection <- "Stereographic"
      scaleNote <- TRUE
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
      #   activeProjection, stringLinks("stere", NA, -90.0, NA, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(prj="stere", x0=NA_real_, lat0=-90, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Polar stereographic", notes = "Conformal projection for regional maps with an east-west extent")
    } else if (property == "Equalarea") {
      # previewMapProjection <- activeProjection <- "Lambert azimuthal equal area"
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
      #   activeProjection, stringLinks("laea", NA, -90.0, NA, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(prj="laea", x0=NA_real_, lat0=-90, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Polar Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps with an east-west extent")
    }
  } else if (abs(center$lat) < 15) {# Case: Close to the equator
#    previewMapLat0 <- 0

    # Determine the standard parallel
    if ((latmax * latmin) <= 0) {
      latS <- max(abs(latmax), abs(latmin)) / 2
    } else {
      latS <- center$lat
    }

    if (property == "Conformal") {
      # previewMapProjection <- activeProjection <- "Mercator"
      scaleNote <- TRUE
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Mercator</span>%s</p>",
      #   activeProjection, stringLinks("merc", NA, NA, latS, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(prj="merc", x0=NA_real_, lat0=latS, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Mercator", notes = "Conformal projection for regional maps with an east-west extent")
    } else if (property == "Equalarea") {
      # previewMapProjection <- activeProjection <- "Cylindrical equal area"
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Cylindrical equal-area</span>%s</p>",
      #   activeProjection, stringLinks("cea", NA, NA, latS, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(prj="cea", x0=NA_real_, lat0=latS, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Cylindrical equal-area", notes = "Equal-area projection for regional maps with an east-west extent")
    }

    # outputText <- c(outputText, sprintf(
    #   "<p class='outputText'>Standard parallel: %s</p>",
    #   outputLAT(latS, FALSE)
    # ))
  }

  # Case: Between pole and equator
  else {
    interval <- (latmax - latmin) / 6
    # latOr <- outputLAT(center$lat, FALSE)
    # latS1 <- outputLAT(latmin + interval, FALSE)
    # latS2 <- outputLAT(latmax - interval, FALSE)
    # previewMapLat0 <- center$lat

    if (property == "Conformal") {
      previewMapProjection <- activeProjection <- "Lambert conformal conic"

      # Check if the cone opens at a pole
      # TODO the checkConicOK function is not working yet
      #conic_check <- checkConicOK(center$lat, center$lng, previewMapProjection)
      conic_check <- TRUE # DEBUG this is currently set to TRUE without checking
      if (conic_check > 0) {
        # outputText <- c(outputText, sprintf(
        #   "<p class='outputText'><span data-proj-name='%s'>Lambert conformal conic</span>%s</p>",
        #   activeProjection, stringLinks("lcc", NA, center$lat, latmin + interval, latmax - interval, center$lng, NA)
        # ))
        # outputText <- c(outputText, sprintf(
        #   "<p class='outputText'>Latitude of origin: %s<br>Standard parallel 1: %s<br>Standard parallel 2: %s</p>",
        #   latOr, latS1, latS2
        # ))
        crs_suggestions <- data.frame(prj="lcc", x0=NA_real_, lat0=center$lat, lat1=latmin + interval, lat2=latmax - interval, lon0=center$lng, k0=NA_real_,
                                    description = "Lambert conformal conic", notes = "Conformal projection for regional maps with an east-west extent")
      } else {
        # If the cone opens at the pole, switch to stereographic
#        previewMapProjection <- activeProjection <- "Stereographic"
        scaleNote <- TRUE
        if (center$lat > 0) {
          # previewMapLat0 <- 90
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
          #   activeProjection, stringLinks("stere", NA, 90.0, NA, NA, center$lng, NA)
          # ))
          crs_suggestions <- data.frame(prj="stere", x0=NA_real_, lat0=90, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Polar stereographic", notes = "Conformal projection for regional maps with an east-west extent")
        } else {
          # previewMapLat0 <- -90
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
          #   activeProjection, stringLinks("stere", NA, -90.0, NA, NA, center$lng, NA)
          # ))
          crs_suggestions <- data.frame(prj="stere", x0=NA_real_, lat0=-90, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Polar stereographic", notes = "Conformal projection for regional maps with an east-west extent")
        }
      }
    } else if (property == "Equalarea") {
      previewMapProjection <- activeProjection <- "Albers equal area conic"


      # TODO the checkConicOK function is not working yet
      #conicTest <- checkConicOK(center$lat, center$lng, previewMapProjection)
      conicTest <- TRUE # DEBUG this is currently set to TRUE without checking



      if (conicTest > 0) {
        # outputText <- c(outputText, sprintf(
        #   "<p class='outputText'><span data-proj-name='%s'>Albers equal-area conic</span>%s</p>",
        #   activeProjection, stringLinks("aea", NA, center$lat, latmin + interval, latmax - interval, center$lng, NA)
        # ))
        # outputText <- c(outputText, sprintf(
        #   "<p class='outputText'>Latitude of origin: %s<br>Standard parallel 1: %s<br>Standard parallel 2: %s</p>",
        #   latOr, latS1, latS2
        # ))
        crs_suggestions <- data.frame(prj="aea", x0=NA_real_, lat0=center$lat,  lat1=latmin + interval, lat2=latmax - interval, lon0=center$lng, k0=NA_real_,
                                    description = "Albers equal-area conic", notes = "Equal-area projection for regional maps with an east-west extent")
      } else {
        #previewMapProjection <- activeProjection <- "Lambert azimuthal equal area"
        if (conicTest == 0) {
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'><span data-proj-name='%s'>Oblique Lambert azimuthal equal-area</span>%s</p>",
          #   activeProjection, stringLinks("laea", NA, center$lat, NA, NA, center$lng, NA)
          # ))
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'>Latitude of origin: %s</p>",
          #   outputLAT(center$lat, FALSE)
          # ))
          crs_suggestions <- data.frame(prj="laea", x0=NA_real_, lat0=center$lat, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Oblique Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps with an east-west extent")
        } else if (center$lat > 0) {
          # previewMapLat0 <- 90
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
          #   activeProjection, stringLinks("laea", NA, 90.0, NA, NA, center$lng, NA)
          # ))
          crs_suggestions <- data.frame(prj="laea", x0=NA_real_, lat0=90, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Polar Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps with an east-west extent")
        } else {
          # previewMapLat0 <- -90
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
          #   activeProjection, stringLinks("laea", NA, -90.0, NA, NA, center$lng, NA)
          # ))
          crs_suggestions <- data.frame(prj="laea", x0=NA_real_, lat0=-90, lat1=NA_real_, lat2=NA_real_, lon0=center$lng, k0=NA_real_,
                                    description = "Polar Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps with an east-west extent")
        }
      }
    }
  }

  # Add central meridian information
#  outputText <- c(outputText, sprintf("<p class='outputText'>Central meridian: %s</p>", lng))

  # Include the scale factor note if necessary
  if (scaleNote) {
    #outputText <- c(outputText, printScaleFactorNote(property))
    printScaleFactorNote(property)
  }

  # Return the complete output
  #return(paste(outputText, collapse = "\n"))
  return(crs_suggestions)
}

################################################################################
# Checking if the fan of the selected extent exposes a cone opening at a pole
checkConicOK <- function(lat0, lon0, projectionString) {
  # Define projection function
  projection <- pickProjection(lat0, lon0, projectionString)

  # Initialize min and max values for y
  ymin <- Inf
  ymax <- -Inf
  res <- 1

  # Define test points
  test_pts <- list(
    c(lon0, -90),
    c(lon0, 90),
    c(normalizeLON(lonmin, lon0), latmin),
    c(normalizeLON(lonmax, lon0), latmax)
  )

  # Projecting sample points
  for (i in seq_along(test_pts)) {
    test_pts[[i]] <- projection(test_pts[[i]])
    ymin <- min(ymin, test_pts[[i]][2])
    ymax <- max(ymax, test_pts[[i]][2])
  }

  # Check if the fan of the selected extent exposes a cone opening at a pole
  if (((ymax - test_pts[[1]][2]) > 1e-6) ||
      ((ymin - test_pts[[2]][2]) < -1e-6)) {
    if (projectionString == "Lambert conformal conic") {
      res <- -1
    }
    # Case of Albers when the fan of the selected extent spans less than 180deg around a pole
    else if (test_pts[[3]][2] > test_pts[[4]][2]) {
      res <- 0
    } else {
      res <- -1
    }
  }

  return(res)
}

################################################################################
# Normalizing longitude values
normalizeLON <- function(lon, lon0) {
  while (lon < (lon0 - 180.0)) {
    lon <- lon + 360.0
  }
  while (lon > (lon0 + 180.0)) {
    lon <- lon - 360.0
  }
  return(lon)
}

################################################################################
# Function to round values for world maps
worldValues <- function(value, scale, roundCM) {
  val <- NULL

  if (roundCM || scale < 1.15) {
    val <- round(value)
  } else if (scale < 1.32) {
    val <- round(value * 2) / 2
  } else {
    val <- round(value * 10) / 10
  }

  return(val)
}



# Set default point values for equidistant world map projections
# move it to where needed
pole_eq <- -90
lngP_eq <- -180
latC_eq <- -39
lngC_eq <- 145
lat1_eq <- 34
lng1_eq <- -117
lat2_eq <- 46
lng2_eq <- 16
