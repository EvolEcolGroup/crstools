#' Suggestions for map projections depending on the extent
#'
#' This function provides suggestions for map projections depending on the
#' extent of the map. For SDMs, an equal area projection is generally favoured,
#' but it is also possible to get suggestions for conformal, equidistant or
#' compromise projections. The algorithm is a reimplementation in R of
#' Projection Wizard (Bojan et al., 2016), version 2.1.
#'
#' TODO give some additional explanation about distortions, trade-offs
#' (especially in the case of whole world maps)
#'
#' @references Bojan, S, Bernhard, J, & Helen, J (2016) Projection Wizard - An
#'   Online Map Projection Selection Tool. The Cartographic Journal
#'   53(2):177-185 DOI: 10.1080/00087041.2015.1131938
#' @param distortion The type of distortion to be minimized. Options are
#'   "equal_area", "conformal", "equidistant" and "compromise". Default is
#'   "equal_area".
#' @param x A vector of four numeric values representing the extent of the map
#'   (xmin, xmax, ymin, ymax), or a SpatExtent object, or a SpatRaster object.
#' @param round_cm Logical. If TRUE, the central meridian is rounded to the
#'   nearest degree. Default is FALSE.
#' @param return_best Logical. If TRUE, only the best projection is returned,
#'   otherwise, if there are multiple options, a list will be returned
#' @param lat_check Logical. If TRUE, the function will check if lat values are
#'   within range (-90,90). Default is TRUE.
#' @param datum The datum to use. Options are "WGS84", "ETRS89" and "NAD83".
#'   Default is "WGS84".
#' @param unit The unit to use. Options are "m" and "ft". Default is "m".
#' @param world_equidist if `distortion`="equidistant" for a whole world
#'   projection, then this parameter should be a list with one of the following
#'   sets of elements:
#' * "Polar azimuthal equidistant": `prj` = "polar", `pole`, `lng_central``, where
#'   `pole` is either -90 or 90 for the South and North Pole, respectively, from
#'   or through which distances are correct, and `lng_central` is the central
#'   meridian. E.g. `list(prj = "polar", pole = 90, lng_central = -180)`
#' * "Oblique azimuthal equidistant": `prj` = "oblique", `lat_center`, `lng_center`,
#'   where `lat_center` and `lng_center` are the latitude and longitude of the
#'   center from or through which distances are correct. E.g. `list(prj =
#'   "oblique", lat_center = 39, lng_center = 145)`
#' * "Two-point azimuthal": `prj` = "two_points", `lat1`, `lng1`,
#'   `lat2`, `lng2`, where `lat1`, `lng1`, `lat2`, `lng2` are the latitude and
#'   longitude of two points on the map from which distances are correct. E.g.
#'   `list(prj = "two_points", lat1 = 34, lng1 = -117, lat2 = 46, lng2 = 16)`
#' @param quiet Logical. If TRUE, suppresses messages. Default is FALSE.
#' @return Either a list of two strings (proj4 and WKT) for a single projection
#'   (if either only one projection is available or return_best is TRUE), or a
#'   list of lists, each containing two strings (proj4 and WKT) for a single
#'   projection (if multiple projections are available and return_best is
#'   FALSE).
#' @examples
#' # Whole map
#' suggest_crs(c(-180, 180, -90, 90))
#' # Northen Hemisphere
#' suggest_crs(c(-180, 180, 21, 70))
#' # Hemisphere showing the tropics
#' suggest_crs(c(-180, 180, -7, 21))
#' # Regional map for EW extent
#' suggest_crs(c(-60, 20, 40, 70))
#' @export
#'

suggest_crs <- function(
    x,
    distortion = c("equal_area", "conformal", "equidistant", "compromise"),
    round_cm = FALSE,
    return_best = TRUE,
    datum = c("WGS84", "ETRS89", "NAD83"),
    unit = c("m", "ft"),
    lat_check = TRUE,
    world_equidist = NULL,
    quiet = FALSE) {
  if (inherits(x, "SpatExtent")) {
    x_ext <- as.vector(x)
  } else if (inherits(x, "SpatRaster")) {
    x_ext <- as.vector(terra::ext(x))
  } else if (inherits(x, "sf")) {
    # note that sf::st_bbox returns c(xmin, ymin, xmax, ymax)
    x_ext <- as.vector(sf::st_bbox(x))[c(1, 3, 2, 4)]
  } else if (inherits(x, "numeric") && length(x) == 4) {
    x_ext <- x
  } else {
    stop(
      "x must be a numeric vector (minlon, maxlon, minlat, maxlat), or ",
      "an object of class SpatExtent, SpatRaster, or sf"
    )
  }

  lon_min <- x_ext[1]
  lon_max <- x_ext[2]
  lat_min <- x_ext[3]
  lat_max <- x_ext[4]

  # check that the extent is valid
  # first check that latitudes are within the range
  # this ensures that lon and lat are fed in the correct order (at least in some cases)
  if ((lat_min < -90 || lat_max > 90) && lat_check == TRUE) {
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
  scale <- 720 /
    (lon_max - lon_min) /
    (sin(lat_max * pi / 180) - sin(lat_min * pi / 180))

  # Getting the center of the map
  center <- data.frame(
    lat = (lat_max + lat_min) / 2,
    lng = (lon_max + lon_min) / 2
  )

  # Normalizing central meridian value
  center$lng <- normalise_lon(center$lng, 0)

  # Rounding central meridian
  if (round_cm) {
    center$lng <- round(center$lng)
  }

  # Choose the appropriate type of map
  if (scale < 1.5) {
    # World (small-scale) map
    if (distortion == "conformal") {
      stop(
        "conformal is not available for maps covering the whole world; try equal_area instead"
      )
    }
    crs_df <- crs_world(
      distortion = distortion,
      center = center,
      scale = scale,
      round_cm = round_cm,
      world_equidist = world_equidist,
      return_best = return_best,
      quiet = quiet
    )
  } else if (scale < 6) {
    # Hemisphere (medium-scale) map
    # if the map is NOT focussing on the tropics
    if (!(abs(lat_max) < 23.43665 && abs(lat_min) < 23.43665)) {
      if (distortion == "conformal") {
        stop(
          "conformal is not available for maps covering a whole hemisphere; try equal_area instead"
        )
      }
    }
    if (distortion == "compromise") {
      stop(
        "compromise is not available for maps covering a whole hemisphere; try equal_area instead"
      )
    }
    crs_df <- crs_hemisphere(
      distortion = distortion,
      center = center,
      scale = scale,
      latmin = lat_min,
      latmax = lat_max,
      quiet = quiet
    )
  } else {
    # Continent or a smaller area (large-scale) map
    if (distortion == "compromise") {
      stop(
        "compromise is not available for maps focussing on a single continent or small area; try equal_area instead"
      )
    }
    crs_df <- crs_small_area(
      distortion,
      center,
      scale,
      lonmin = lon_min,
      lonmax = lon_max,
      latmin = lat_min,
      latmax = lat_max,
      quiet = quiet
    )
  }

  if ((nrow(crs_df) > 1) && return_best) {
    crs_df <- crs_df[1, ]
  }
  if (nrow(crs_df) == 1) {
    crs_obj <- crs_string_row(crs_df[1, ], datum, unit)
    crs_obj[["description"]] <- crs_df[1, "description"]
    crs_obj[["notes"]] <- crs_df[1, "notes"]
    return(crs_obj)
  } else {
    crs_list <- list()
    for (i in seq_len(nrow(crs_df))) {
      crs_obj <- crs_string_row(crs_df[i, ], datum, unit)
      crs_obj[["description"]] <- crs_df[i, "description"]
      crs_obj[["notes"]] <- crs_df[i, "notes"]
      crs_list[[i]] <- crs_obj
    }
    names(crs_list) <- crs_df$prj
    return(crs_list)
  }
}
