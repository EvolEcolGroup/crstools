#' @title East-West extent
#' @description Checking the East-West extent of a regional map
#' @param distortion character string (e.g., "equal_area", "equidistant",
#'   "compromise").
#' @param center The center of the map projection.
#' @param scale numeric value, scale of the map.
#' @param lonmin The minimum longitude of the map. Default is -180.
#' @param lonmax The maximum longitude of the map. Default is 180.
#' @param latmin numeric value, minimum latitude of the map.
#' @param latmax numeric value, maximum latitude of the map.
#' @param quiet logical, whether to suppress messages.
#' @return data.frame with the suggested projection.
#' @keywords internal


################################################################################
crs_ew_extent <- function(distortion, center, scale,
                          lonmin, lonmax, latmin, latmax,
                          quiet = FALSE) {
  # Flag to determine if scale note should be included
  scaleNote <- FALSE

  # Add the initial output based on the distortion
  # if (distortion == "conformal" && !quiet) {
  #   message("Conformal projection for regional maps with an east-west extent")
  # } else if (distortion == "equal_area"  && !quiet) {
  #   message("Equal-area projection for regional maps with an east-west extent")
  # }


  # Case: Close to poles
  if (center$lat > 70) {
    #   previewMapLat0 <- 90
    if (distortion == "conformal") {
      #    previewMapProjection <- activeProjection <- "Stereographic"
      scaleNote <- TRUE
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
      #   activeProjection, stringLinks("stere", NA, 90.0, NA, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(
        prj = "stere", x0 = NA_real_, lat0 = 90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar stereographic", notes = "Conformal projection for regional maps with an east-west extent"
      )
    } else if (distortion == "equal_area") {
      # previewMapProjection <- activeProjection <- "Lambert azimuthal equal area"
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
      #   activeProjection, stringLinks("laea", NA, 90.0, NA, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(
        prj = "laea", x0 = NA_real_, lat0 = 90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps with an east-west extent"
      )
    }
  } else if (center$lat < -70) {
    previewMapLat0 <- -90
    if (distortion == "conformal") {
      #      previewMapProjection <- activeProjection <- "Stereographic"
      scaleNote <- TRUE
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
      #   activeProjection, stringLinks("stere", NA, -90.0, NA, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(
        prj = "stere", x0 = NA_real_, lat0 = -90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar stereographic", notes = "Conformal projection for regional maps with an east-west extent"
      )
    } else if (distortion == "equal_area") {
      # previewMapProjection <- activeProjection <- "Lambert azimuthal equal area"
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
      #   activeProjection, stringLinks("laea", NA, -90.0, NA, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(
        prj = "laea", x0 = NA_real_, lat0 = -90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps with an east-west extent"
      )
    }
  } else if (abs(center$lat) < 15) { # Case: Close to the equator
    #    previewMapLat0 <- 0

    # Determine the standard parallel
    if ((latmax * latmin) <= 0) {
      latS <- max(abs(latmax), abs(latmin)) / 2
    } else {
      latS <- center$lat
    }

    if (distortion == "conformal") {
      # previewMapProjection <- activeProjection <- "Mercator"
      scaleNote <- TRUE
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Mercator</span>%s</p>",
      #   activeProjection, stringLinks("merc", NA, NA, latS, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(
        prj = "merc", x0 = NA_real_, lat0 = NA_real_, lat1 = latS, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Mercator", notes = "Conformal projection for regional maps with an east-west extent"
      )
    } else if (distortion == "equal_area") {
      # previewMapProjection <- activeProjection <- "Cylindrical equal area"
      # outputText <- c(outputText, sprintf(
      #   "<p class='outputText'><span data-proj-name='%s'>Cylindrical equal-area</span>%s</p>",
      #   activeProjection, stringLinks("cea", NA, NA, latS, NA, center$lng, NA)
      # ))
      crs_suggestions <- data.frame(
        prj = "cea", x0 = NA_real_, lat0 = latS, lat1 = latS, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Cylindrical equal-area", notes = "Equal-area projection for regional maps with an east-west extent"
      )
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

    if (distortion == "conformal") {
      previewMapProjection <- activeProjection <- "Lambert conformal conic"
      
      
      # Create the CRS of the conic projection that we want to test
      conic_crs_to_test <- data.frame(
        prj = "lcc", x0 = NA_real_, lat0 = center$lat, lat1 = latmin + interval, lat2 = latmax - interval, lon0 = center$lng, k0 = NA_real_,
        description = "Lambert conformal conic", notes = "Conformal projection for regional maps with an east-west extent"
      )
      # extract the proj4 string from the conic_crs_to_test data frame
      conic_crs_to_test <- crs_string_row(conic_crs_to_test[1, ], "WGS84", "m")$proj4
      # Check if the cone opens at a pole
      conic_check <- crs_check_conic(center$lat, center$lng, conic_crs_to_test, lonmin, lonmax, latmin, latmax)
      

      # Check if the cone opens at a pole
      # TODO the crs_check_conic function is not working yet
      #conic_check <- crs_check_conic(center$lat, center$lng, previewMapProjection)
      #conic_check <- 1 # DEBUG this is currently set to TRUE without checking
      if (conic_check > 0) {
        # outputText <- c(outputText, sprintf(
        #   "<p class='outputText'><span data-proj-name='%s'>Lambert conformal conic</span>%s</p>",
        #   activeProjection, stringLinks("lcc", NA, center$lat, latmin + interval, latmax - interval, center$lng, NA)
        # ))
        # outputText <- c(outputText, sprintf(
        #   "<p class='outputText'>Latitude of origin: %s<br>Standard parallel 1: %s<br>Standard parallel 2: %s</p>",
        #   latOr, latS1, latS2
        # ))
        crs_suggestions <- data.frame(
          prj = "lcc", x0 = NA_real_, lat0 = center$lat, lat1 = latmin + interval, lat2 = latmax - interval, lon0 = center$lng, k0 = NA_real_,
          description = "Lambert conformal conic", notes = "Conformal projection for regional maps with an east-west extent"
        )
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
          crs_suggestions <- data.frame(
            prj = "stere", x0 = NA_real_, lat0 = 90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
            description = "Polar stereographic", notes = "Conformal projection for regional maps with an east-west extent"
          )
        } else {
          # previewMapLat0 <- -90
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'><span data-proj-name='%s'>Polar stereographic</span>%s</p>",
          #   activeProjection, stringLinks("stere", NA, -90.0, NA, NA, center$lng, NA)
          # ))
          crs_suggestions <- data.frame(
            prj = "stere", x0 = NA_real_, lat0 = -90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
            description = "Polar stereographic", notes = "Conformal projection for regional maps with an east-west extent"
          )
        }
      }
    } else if (distortion == "equal_area") {
      previewMapProjection <- activeProjection <- "Albers equal area conic"

      # Create the CRS of the conic projection that we want to test
      conic_crs_to_test <- data.frame(
        prj = "aea", x0 = NA_real_, lat0 = center$lat, lat1 = latmin + interval, lat2 = latmax - interval, lon0 = center$lng, k0 = NA_real_,
        description = "Albers equal-area conic",
        notes = "Equal-area projection for regional maps with an east-west extent"
      )
      # extract the proj4 string from the conic_crs_to_test data frame
      conic_crs_to_test <- crs_string_row(conic_crs_to_test[1, ], "WGS84", "m")$proj4
      # Check if the cone opens at a pole
      conicTest <- crs_check_conic(center$lat, center$lng, conic_crs_to_test, lonmin, lonmax, latmin, latmax)



      if (conicTest > 0) {
        # outputText <- c(outputText, sprintf(
        #   "<p class='outputText'><span data-proj-name='%s'>Albers equal-area conic</span>%s</p>",
        #   activeProjection, stringLinks("aea", NA, center$lat, latmin + interval, latmax - interval, center$lng, NA)
        # ))
        # outputText <- c(outputText, sprintf(
        #   "<p class='outputText'>Latitude of origin: %s<br>Standard parallel 1: %s<br>Standard parallel 2: %s</p>",
        #   latOr, latS1, latS2
        # ))
        crs_suggestions <- data.frame(
          prj = "aea", x0 = NA_real_, lat0 = center$lat, lat1 = latmin + interval, lat2 = latmax - interval, lon0 = center$lng, k0 = NA_real_,
          description = "Albers equal-area conic",
          notes = "Equal-area projection for regional maps with an east-west extent"
        )
      } else {
        # previewMapProjection <- activeProjection <- "Lambert azimuthal equal area"
        if (conicTest == 0) {
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'><span data-proj-name='%s'>Oblique Lambert azimuthal equal-area</span>%s</p>",
          #   activeProjection, stringLinks("laea", NA, center$lat, NA, NA, center$lng, NA)
          # ))
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'>Latitude of origin: %s</p>",
          #   outputLAT(center$lat, FALSE)
          # ))
          crs_suggestions <- data.frame(
            prj = "laea", x0 = NA_real_, lat0 = center$lat, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
            description = "Oblique Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps with an east-west extent"
          )
        } else if (center$lat > 0) {
          # previewMapLat0 <- 90
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
          #   activeProjection, stringLinks("laea", NA, 90.0, NA, NA, center$lng, NA)
          # ))
          crs_suggestions <- data.frame(
            prj = "laea", x0 = NA_real_, lat0 = 90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
            description = "Polar Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps with an east-west extent"
          )
        } else {
          # previewMapLat0 <- -90
          # outputText <- c(outputText, sprintf(
          #   "<p class='outputText'><span data-proj-name='%s'>Polar Lambert azimuthal equal-area</span>%s</p>",
          #   activeProjection, stringLinks("laea", NA, -90.0, NA, NA, center$lng, NA)
          # ))
          crs_suggestions <- data.frame(
            prj = "laea", x0 = NA_real_, lat0 = -90, lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
            description = "Polar Lambert azimuthal equal-area", notes = "Equal-area projection for regional maps with an east-west extent"
          )
        }
      }
    }
  }

  # Add central meridian information
  #  outputText <- c(outputText, sprintf("<p class='outputText'>Central meridian: %s</p>", lng))

  # Include the scale factor note if necessary
  if (scaleNote) {
    # outputText <- c(outputText, printScaleFactorNote(distortion))
    # printScaleFactorNote(distortion)
  }

  # Return the complete output
  # return(paste(outputText, collapse = "\n"))
  return(crs_suggestions)
}



################################################################################
# Checking if the fan of the selected extent exposes a cone opening at a pole
crs_check_conic <- function(lat0, lon0, proj4_string, lonmin, lonmax, latmin, latmax) {
  # Define projection function
#  projection <- pickProjection(lat0, lon0, proj4_string)

  # Initialize min and max values for y
  ymin <- Inf
  ymax <- -Inf
  res <- 1

  # Define test points as an sf object
  test_pts <- data.frame(
    lon = c(lon0, lon0, normalise_lon(lonmin, lon0), normalise_lon(lonmax, lon0)),
    lat = c(-89.9, 89.9, latmin, latmax))
  # cast to sf object
  test_pts <- sf::st_as_sf(test_pts, coords = c("lon", "lat"), crs = 4326)
  #project to the proj4 string
  test_pts <- sf::st_transform(test_pts, crs = proj4_string)
  # get y min and y max
  test_pts <- sf::st_coordinates(test_pts)
  
  # 
  
  
  
  # flip coordinates to match the screen coordiantes (for which the rules below have been designed)
  test_pts[,2] = - test_pts[,2]
  ymin <- min(test_pts[,2])
  ymax <- max(test_pts[,2])

  # # hack to avoid NaN values
  # if (is.nan(test_pts[1,2])||is.nan(test_pts[2,2])){
  #   return (1)
  # }
  
  
  # Check if the fan of the selected extent exposes a cone opening at a pole
  # Note: up is negative and down is positive in graphics
  if (((ymax - test_pts[1,2]) > 1e-6) ||
    ((ymin - test_pts[2,2]) < -1e-6)) {

    if (substr(proj4_string,1,9) == "+proj=lcc") { # Check that this is an lcc projection
      res <- -1
    }
    # Case of Albers when the fan of the selected extent spans less than 180deg around a pole
    # Note: up is negative and down is positive in graphics
    else if (test_pts[3,2] > test_pts[4,2]) {
      res <- 0
    } else {
      res <- -1
    }
  }

  return(res)
}
