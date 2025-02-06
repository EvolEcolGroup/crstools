#' Suggestions for map projections depending on the extent
#'
#' This function provides suggestions for map projections depending on the extent
#' of the map. For SDMs, an equal area projection is generally favoured, but
#' it is also possible to get suggestions for conformal, equidistant or
#' compromise projections. The algorithm is a reimplementation in R of
#' Projection Wizard (Bojan et al., 2016), version 2.1.
#'
#' TODO give some additional explanation about distortions, trade-offs
#'  (especially in the case of whole world maps)
#'
#' @references Bojan, S, Bernhard, J, & Helen, J (2016) Projection Wizard - An
#' Online Map Projection Selection Tool. The Cartographic Journal 53(2):177-185
#' DOI: 10.1080/00087041.2015.1131938
#' @param distortion The type of distortion to be minimized. Options are
#' "equal_area", "conformal", "equidistant" and "compromise". Default is
#' "equal_area".
#' @param lonmin The minimum longitude of the map. Default is -180.
#' @param lonmax The maximum longitude of the map. Default is 180.
#' @param latmin The minimum latitude of the map. Default is -90.
#' @param latmax The maximum latitude of the map. Default is 90.
#' @param roundCM Logical. If TRUE, the central meridian is rounded to the
#' nearest degree. Default is FALSE.
#' @param return_best Logical. If TRUE, only the best projection is returned, otherwise,
#' if there are multiple options, a list will be returned
#' @return Either a list of two strings (proj4 and WKT) for a single projection
#' (if either only one projection is available or return_best is TRUE),
#'  or a list of lists, each containing two strings (proj4 and WKT) for a
#'  single projection (if multiple projections are available and return_best is FALSE).
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

crs_wizard <- function(distortion = c("equal_area","conformal","equidistant","compromise"),
                       lon_min = -180, lon_max = 180, lat_min = -90, lat_max = 90,
                       roundCM = FALSE, return_best = TRUE, datum = c("WGS84", "ETRS89", "NAD83"),
                       unit= c("m","ft")
                       ) {
  # check if the input is correct
  distortion <- match.arg(distortion)

  # Computing the scale of the map
  scale <- 720 / (lon_max - lon_min) / (sin(lat_max * pi / 180) - sin(lat_min * pi / 180))

  # Getting the center of the map
  center <- data.frame(lat = (lat_max + lat_min) / 2, lng = (lon_max + lon_min) / 2)

  # Normalizing central meridian value
  center$lng <- normalise_lon(center$lng, 0)

  # Rounding central meridian
  if (roundCM) {
    center$lng <- round(center$lng)
  }

  # Choose the appropriate type of map
  if (scale < 1.5) {  # World (small-scale) map
    if (distortion=="conformal") {
      stop("conformal is not available for maps covering the whole world; try equal_area instead")
    }
    crs_df <- crs_world(distortion, center, scale, roundCM)

  } else if (scale < 6) { # Hemisphere (medium-scale) map
    # if the map is NOT focussing on the tropics
    if (!(abs(lat_max) < 23.43665 && abs(lat_min) < 23.43665)) {
      if (distortion=="conformal") {
        stop("conformal is not available for maps covering a whole hemisphere; try equal_area instead")
      }
    }
    if (distortion=="compromise") {
      stop("compromise is not available for maps covering a whole hemisphere; try equal_area instead")
    }
    crs_df <- crs_hemisphere(distortion, center, scale,  latmin = lat_min, latmax =lat_max)

  } else { # Continent or a smaller area (large-scale) map
    if (distortion=="compromise") {
      stop("compromise is not available for maps focussing on a single continent or small area; try equal_area instead")
    }
    crs_df <- crs_small_area(distortion, center, scale,
                   lonmin = lon_min, lonmax = lon_max, latmin = lat_min, latmax =lat_max)
  }
  
  if ((nrow(crs_df) > 1) && return_best) {
    crs_df <- crs_df[1,]
  }
  if (nrow(crs_df) == 1) {
    crs_string_row(crs_df[1,], datum, unit)
  }
  browser()
  
}




# Set default point values for equidistant world map projections
# # move it to where needed
# pole_eq <- -90
# lngP_eq <- -180
# latC_eq <- -39
# lngC_eq <- 145
# lat1_eq <- 34
# lng1_eq <- -117
# lat2_eq <- 46
# lng2_eq <- 16
