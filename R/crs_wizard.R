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
#' @param lat_check Logical. If TRUE, the function will check if lat values are within range (-90,90). 
#' Default is TRUE.
#' @return Either a list of two strings (proj4 and WKT) for a single projection
#' (if either only one projection is available or return_best is TRUE),
#'  or a list of lists, each containing two strings (proj4 and WKT) for a
#'  single projection (if multiple projections are available and return_best is FALSE).
#' @examples
#' # Whole map
#' crs_wizard(c(-180,180,-90, 90))
#' # Northen Hemisphere
#' crs_wizard(lonmin = -180, lonmax = 180, latmin = 21, latmax = 70)
#' # Hemisphere showing the tropics
#' crs_wizard(lonmin = -180, lonmax = 180, latmin = -7, latmax = 21)
#' # Regional map for EW extent
#' crs_wizard(lonmin = -60, lonmax = 20, latmin = 40, latmax = 70)
#' @export
#'

crs_wizard <- function(x, distortion = c("equal_area","conformal","equidistant","compromise"),
                       roundCM = FALSE, return_best = TRUE, datum = c("WGS84", "ETRS89", "NAD83"),
                       unit= c("m","ft"), lat_check =TRUE
                       ) {
  if (inherits(x, "SpatExtent")) {
    x_ext <- as.vector(x)
  } else if(inherits(x, "SpatRaster")){
    x_ext <- as.vector(terra::ext(x))
  } else if (inherits(x, "numeric") && length(x) == 4){
    x_ext <- x
  } else {
    stop("x must be a vector, a SpatExtent object or a SpatRaster object")
  }
  
    lon_min <- x_ext[1]
    lon_max <- x_ext[2]
    lat_min <- x_ext[3]
    lat_max <- x_ext[4]

  # check that the extent is valid
  # first check that latitudes are within the range
  # this ensures that lon and lat are fed in the correct order (at least in some cases)
  if ((lat_min < -90 || lat_max > 90) & lat_check == TRUE) {
    stop("Latitude values must be between -90 and 90")
  }
  # check that lat_min is smaller than lat_max
  if (lat_min > lat_max) {
     stop("lat_min must be smaller than lat_max")
  # check that lon_min is smaller than lon_max
  } else if (lon_min > lon_max) {
     stop("lon_min must be smaller than lon_max")
  } 
  
  # check if the input is correct
  distortion <- match.arg(distortion)
  
  datum <- match.arg(datum)
  unit <- match.arg(unit)

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
    crs_obj <- crs_string_row(crs_df[1,], datum, unit)
    crs_obj[["description"]] <- crs_df[1, "description"]
    crs_obj[["notes"]] <- crs_df[1, "notes"]
    return(crs_obj)
  } else {
    crs_list <- list()
    for (i in seq_len(nrow(crs_df))) {
      crs_obj <- crs_string_row(crs_df[i,], datum, unit)
      crs_obj[["description"]] <- crs_df[i, "description"]
      crs_obj[["notes"]] <- crs_df[i, "notes"]
      crs_list[[i]] <- crs_obj
    }
    names(crs_list) <- crs_df$prj
    return(crs_list)
  }

  
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
