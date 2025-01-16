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
