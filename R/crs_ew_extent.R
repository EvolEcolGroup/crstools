#' @title East-West extent
#' @description Checking the East-West extent of a regional map
#' @param distortion character string (e.g., "equal_area", "conformal").
#' @param center numeric value, center of the map projection.
#' @param lonmin numeric value, minimum longitude of the map.
#' @param lonmax numeric value, maximum longitude of the map.
#' @param latmin numeric value, minimum latitude of the map.
#' @param latmax numeric value, maximum latitude of the map.
#' @param quiet logical, whether to suppress messages.
#' @return data.frame with the suggested projection.
#' @keywords internal


################################################################################
crs_ew_extent <- function(distortion, center,
                          lonmin, lonmax, latmin, latmax,
                          quiet = FALSE) {
  # Flag to determine if scale note should be included
  scale_note <- FALSE
  # Case: Close to poles
  if (center$lat > 70) {
    if (distortion == "conformal") {
      scale_note <- TRUE

      crs_suggestions <- data.frame(
        prj = "stere", x0 = NA_real_, lat0 = 90, lat1 = NA_real_,
        lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar stereographic",
        notes = paste0(
          "Conformal projection ",
          "for regional maps with an east-west extent"
        )
      )
    } else if (distortion == "equal_area") {
      crs_suggestions <- data.frame(
        prj = "laea", x0 = NA_real_, lat0 = 90, lat1 = NA_real_,
        lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar Lambert azimuthal equal-area",
        notes = paste0(
          "Equal-area projection ",
          "for regional maps with an east-west extent"
        )
      )
    }
  } else if (center$lat < -70) {
    if (distortion == "conformal") {
      scale_note <- TRUE

      crs_suggestions <- data.frame(
        prj = "stere", x0 = NA_real_, lat0 = -90, lat1 = NA_real_,
        lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar stereographic",
        notes = paste0(
          "Conformal projection ",
          "for regional maps with an east-west extent"
        )
      )
    } else if (distortion == "equal_area") {
      crs_suggestions <- data.frame(
        prj = "laea", x0 = NA_real_, lat0 = -90, lat1 = NA_real_,
        lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Polar Lambert azimuthal equal-area",
        notes = paste0(
          "Equal-area projection ",
          "for regional maps with an east-west extent"
        )
      )
    }
    # Case: Close to the equator
  } else if (abs(center$lat) < 15) {
    # Determine the standard parallel
    if ((latmax * latmin) <= 0) {
      lat_s <- max(abs(latmax), abs(latmin)) / 2
    } else {
      lat_s <- center$lat
    }

    if (distortion == "conformal") {
      scale_note <- TRUE

      crs_suggestions <- data.frame(
        prj = "merc", x0 = NA_real_, lat0 = NA_real_, lat1 = lat_s,
        lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Mercator",
        notes = paste0(
          "Conformal projection ",
          "for regional maps with an east-west extent"
        )
      )
    } else if (distortion == "equal_area") {
      crs_suggestions <- data.frame(
        prj = "cea", x0 = NA_real_, lat0 = lat_s, lat1 = lat_s,
        lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
        description = "Cylindrical equal-area",
        notes = paste0(
          "Equal-area projection ",
          "for regional maps with an east-west extent"
        )
      )
    }
  } else {
    # Case: Between pole and equator
    interval <- (latmax - latmin) / 6

    if (distortion == "conformal") {
      previewMapProjection <- activeProjection <- "Lambert conformal conic"

      # Create the CRS of the conic projection that we want to test
      conic_crs_to_test <- data.frame(
        prj = "lcc", x0 = NA_real_, lat0 = center$lat, lat1 = latmin + interval,
        lat2 = latmax - interval, lon0 = center$lng, k0 = NA_real_,
        description = "Lambert conformal conic",
        notes = paste0(
          "Conformal projection ",
          "for regional maps with an east-west extent"
        )
      )
      # extract the proj4 string from the conic_crs_to_test data frame
      conic_crs_to_test <- crs_string_row(
        conic_crs_to_test[1, ],
        "WGS84", "m"
      )$proj4
      # Check if the cone opens at a pole
      conic_check <- crs_check_conic(
        center$lng, conic_crs_to_test,
        lonmin, lonmax, latmin, latmax
      )

      # Check if the cone opens at a pole
      if (conic_check > 0) {
        crs_suggestions <- data.frame(
          prj = "lcc", x0 = NA_real_, lat0 = center$lat,
          lat1 = latmin + interval, lat2 = latmax - interval,
          lon0 = center$lng, k0 = NA_real_,
          description = "Lambert conformal conic",
          notes = paste0(
            "Conformal projection ",
            "for regional maps with an east-west extent"
          )
        )
      } else {
        # If the cone opens at the pole, switch to stereographic
        scale_note <- TRUE
        if (center$lat > 0) {
          crs_suggestions <- data.frame(
            prj = "stere", x0 = NA_real_, lat0 = 90,
            lat1 = NA_real_, lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
            description = "Polar stereographic",
            notes = paste0(
              "Conformal projection ",
              "for regional maps with an east-west extent"
            )
          )
        } else {
          crs_suggestions <- data.frame(
            prj = "stere", x0 = NA_real_, lat0 = -90, lat1 = NA_real_,
            lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
            description = "Polar stereographic",
            notes = paste0(
              "Conformal projection ",
              "for regional maps with an east-west extent"
            )
          )
        }
      }
    } else if (distortion == "equal_area") {
      # Create the CRS of the conic projection that we want to test
      conic_crs_to_test <- data.frame(
        prj = "aea", x0 = NA_real_, lat0 = center$lat, lat1 = latmin + interval,
        lat2 = latmax - interval, lon0 = center$lng, k0 = NA_real_,
        description = "Albers equal-area conic",
        notes = paste0(
          "Equal-area projection ",
          "for regional maps with an east-west extent"
        )
      )
      # extract the proj4 string from the conic_crs_to_test data frame
      conic_crs_to_test <- crs_string_row(
        conic_crs_to_test[1, ],
        "WGS84", "m"
      )$proj4
      # Check if the cone opens at a pole
      conicTest <- crs_check_conic(
        center$lng, conic_crs_to_test,
        lonmin, lonmax, latmin, latmax
      )

      if (conicTest > 0) {
        crs_suggestions <- data.frame(
          prj = "aea", x0 = NA_real_, lat0 = center$lat,
          lat1 = latmin + interval, lat2 = latmax - interval,
          lon0 = center$lng, k0 = NA_real_,
          description = "Albers equal-area conic",
          notes = paste0(
            "Equal-area projection ",
            "for regional maps with an east-west extent"
          )
        )
      } else {
        if (conicTest == 0) {
          crs_suggestions <- data.frame(
            prj = "laea", x0 = NA_real_, lat0 = center$lat, lat1 = NA_real_,
            lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
            description = "Oblique Lambert azimuthal equal-area",
            notes = paste0(
              "Equal-area projection ",
              "for regional maps with an east-west extent"
            )
          )
        } else if (center$lat > 0) {
          crs_suggestions <- data.frame(
            prj = "laea", x0 = NA_real_, lat0 = 90, lat1 = NA_real_,
            lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
            description = "Polar Lambert azimuthal equal-area",
            notes = paste0(
              "Equal-area projection ",
              "for regional maps with an east-west extent"
            )
          )
        } else {
          crs_suggestions <- data.frame(
            prj = "laea", x0 = NA_real_, lat0 = -90, lat1 = NA_real_,
            lat2 = NA_real_, lon0 = center$lng, k0 = NA_real_,
            description = "Polar Lambert azimuthal equal-area",
            notes = paste0(
              "Equal-area projection ",
              "for regional maps with an east-west extent"
            )
          )
        }
      }
    }
  }

  # Add central meridian information

  # Include the scale factor note if necessary
  if (scale_note) {
    message(paste0(
      "To reduce overall area distortion on the map, one can also apply ",
      "a scale factor k. Various values for k can be applied ",
      "and the area distortion patterns along the center and at the border of ",
      "the map are compared to select most appropriate value."
    ))
  }

  # Return the complete output
  return(crs_suggestions)
}



################################################################################
# Checking if the fan of the selected extent exposes a cone opening at a pole
crs_check_conic <- function(lon0, proj4_string, lonmin, lonmax,
                            latmin, latmax) {
  # Define projection function

  # Initialize min and max values for y
  ymin <- Inf
  ymax <- -Inf
  res <- 1

  # Define test points as an sf object
  test_pts <- data.frame(
    lon = c(
      lon0, lon0, normalise_lon(lonmin, lon0),
      normalise_lon(lonmax, lon0)
    ),
    lat = c(-89.9, 89.9, latmin, latmax)
  )
  # cast to sf object
  test_pts <- sf::st_as_sf(test_pts, coords = c("lon", "lat"), crs = 4326)
  # project to the proj4 string
  test_pts <- sf::st_transform(test_pts, crs = proj4_string)
  # get y min and y max
  test_pts <- sf::st_coordinates(test_pts)

  # flip coordinates to match the screen coordinates
  # (for which the rules below have been designed)
  test_pts[, 2] <- -test_pts[, 2]
  ymin <- min(test_pts[, 2])
  ymax <- max(test_pts[, 2])

  # Check if the fan of the selected extent exposes a cone opening at a pole
  # Note: up is negative and down is positive in graphics
  if (((ymax - test_pts[1, 2]) > 1e-6) ||
    ((ymin - test_pts[2, 2]) < -1e-6)) {
    if (substr(proj4_string, 1, 9) == "+proj=lcc") { # Check is lcc projection
      res <- -1
    } else if (test_pts[3, 2] > test_pts[4, 2]) {
      # Case of Albers when the fan of the selected extent spans
      # less than 180deg around a pole
      # Note: up is negative and down is positive in graphics
      res <- 0
    } else {
      res <- -1
    }
  }

  return(res) # nolint
}
