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
  center$lng <- normalise_lon(center$lng, 0)

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
# regional map with a north-south extent
crs_ns_extent <- function(property, center) {
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
crs_ew_extent <- function(property, center, scale,
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
      # TODO the crs_check_conic function is not working yet
      #conic_check <- crs_check_conic(center$lat, center$lng, previewMapProjection)
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


      # TODO the crs_check_conic function is not working yet
      #conicTest <- crs_check_conic(center$lat, center$lng, previewMapProjection)
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
crs_check_conic <- function(lat0, lon0, projectionString) {
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
    c(normalise_lon(lonmin, lon0), latmin),
    c(normalise_lon(lonmax, lon0), latmax)
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
