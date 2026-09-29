#' Residuals of the polynomial transformation fitted to Ground Control Points
#'
#' This function refits the polynomial transformation that GDAL uses in
#' [georeference_img()] (pixel coordinates to longitude/latitude) and returns,
#' for each Ground Control Point (GCP), the ordinary residuals and the
#' leave-one-out (LOO) residuals. Ordinary residuals measure how well the
#' transformation fits the GCPs used to estimate it. On the other hand, LOO
#' residuals are obtained by predicting each GCP from a transformation fitted to
#' all the other GCPs, and give a more "honest" measure of the error expected
#' for points of the map that were not used as GCPs. The leverage of each GCP is
#' also returned, which is a measure of how much influence each GCP has on the
#' fitted transformation. Leverage values range from 0 to 1, with higher values
#' indicating that the GCP has more influence on the fitted transformation. 
#'
#' The transformation is a polynomial in pixel coordinates, fitted by ordinary 
#' least square separately for longitude and latitude, with all terms up to 
#' the specific order (3, 6, 10 coefficients for first, second and third order
#' polynomials). This forward (pixel to lon/lat) transformation, is the same
#' applied by GDAL in [georeference_img()] and [get_pts_coords()]. 
#' LOO resisuals are computed without refittinhg as \eqn{e_i / (1 - h_{ii})},
#' where \eqn{e_i} is the ordinary residual and \eqn{h_{ii}} is the
#' leverage of the i-th GCP.
#'
#' Residuals are given in degrees (observed minus fitted) and as the
#' great-circle distance (in km, on a sphere of radius 6371.0088 km) between
#' the observed and the fitted position of each GCP. Note that, because the
#' polynomial is fitted in degrees, residuals in longitude are not comparable
#' across latitudes, whereas distances in km are.
#'
#' @param gcp A data frame containing the Ground Control Points (GCPs), with
#'   columns `id`, `x`, `y`, `longitude` and `latitude` (the same format used
#'   by [georeference_img()]).
#' @param transform_method A character string specifying the polynomial used
#'   for the transformation. Options are "poly_1" (first order polynomial),
#'   "poly_2" (second order polynomial), "poly_3" (third order polynomial) or
#'   "auto" (the default), which mimics the choice made by GDAL in
#'   [georeference_img()]: a first order polynomial if fewer than 6 GCPs are
#'   available, and a second order polynomial otherwise.
#'
#' @return A data frame with one row per GCP, containing the original columns
#'   and:
#'   - `fitted_lon`, `fitted_lat`: the position predicted by the transformation.
#'   - `res_lon`, `res_lat`: the ordinary residuals, in degrees.
#'   - `res_km`: the distance between observed and fitted position, in km.
#'   - `leverage`: the leverage of each GCP. Values close to 1 indicate GCPs
#'     that strongly determine the transformation (typically points at the
#'     edges of the map, or isolated points).
#'   - `loo_lon`, `loo_lat`: the leave-one-out residuals, in degrees.
#'   - `loo_km`: the distance between observed and leave-one-out predicted
#'     position, in km.
#'
#'   The data frame has the attributes `order` (the order of the polynomial),
#'   `rmse_km` and `loo_rmse_km` (the root mean square of `res_km` and
#'   `loo_km`).
#'
#' @export
#'
#' @examples
#' gcp_df <- readRDS(system.file(
#'   "extdata/europe_gcp_georef.RDS",
#'   package = "crstools"
#' ))
#' res <- gcp_residuals(gcp_df, transform_method = "poly_1")
#' res
#' attr(res, "rmse_km")
#' attr(res, "loo_rmse_km")
gcp_residuals <- function(gcp,
                          transform_method = c(
                            "auto", "poly_1", "poly_2", "poly_3"
                          )) {
  transform_method <- match.arg(transform_method)

  # check if gcp is a dataframe with the right columns
  # nolint start
  if ((!is.data.frame(gcp)) ||
    (!all(c("id", "x", "y", "longitude", "latitude")
    %in% colnames(gcp)))) {
    stop(
      "gcp must be a data frame with columns: id, x, y, longitude",
      ", latitude"
    )
  }
  # nolint end
  # reorder columns and only keep the ones that we need
  gcp <- as.data.frame(gcp)[, c("id", "x", "y", "longitude", "latitude")]

  # check that there are no NAs present
  if (any(is.na(gcp))) {
    stop(
      "GCP dataframe contains NA values."
    )
  }

  n_gcp <- nrow(gcp)
  # choose the order of the polynomial (for "auto", same rule as GDAL)
  if (transform_method == "poly_1") {
    poly_order <- 1L
  }
  if (transform_method == "poly_2") {
    poly_order <- 2L
  }
  if (transform_method == "poly_3") {
    poly_order <- 3L
  }
  if (transform_method == "auto") {
    if (n_gcp < 6) {
      poly_order <- 1L
    } else {
      poly_order <- 2L
    }
  }

  # polynomial terms (the same used in get_pts_coords())
  # first order polynomial
  if (poly_order == 1L) {
    poly_formula <- ~ x + y
    n_coef <- 3L
  }
  # second order polynomial
  if (poly_order == 2L) {
    poly_formula <- ~ x + y + I(x^2) + I(y^2) + I(x * y)
    n_coef <- 6L
  }
  # third order polynomial
  if (poly_order == 3L) {
    poly_formula <- ~ x + y +
      I(x^2) + I(y^2) + I(x * y) +
      I(x^3) + I(y^3) +
      I(x^2 * y) + I(x * y^2)
    n_coef <- 10L
  }
  if (n_gcp < n_coef) {
    stop(
      "A polynomial of order ", poly_order, " requires at least ", n_coef,
      " GCPs, but only ", n_gcp, " were provided."
    )
  }

  # least squares fit, separately for longitude and latitude
  lon_model <- stats::lm(
    stats::update(poly_formula, longitude ~ .),
    data = gcp
  )
  lat_model <- stats::lm(
    stats::update(poly_formula, latitude ~ .),
    data = gcp
  )
  # lm() drops terms (NA coefficients) if the GCPs do not allow to estimate
  # them
  if (anyNA(stats::coef(lon_model))) {
    stop(
      "The GCPs are not sufficiently spread to fit a polynomial of order ",
      poly_order, " (e.g. they are collinear)."
    )
  }
  
  # predicted lon/lat of each GCP from pixel position
  fitted <- cbind(stats::fitted(lon_model), stats::fitted(lat_model))
  # residuals of each GCP from pixel position as observed minnus lon/lat of each
  # GCP in degrees
  res <- cbind(stats::residuals(lon_model), stats::residuals(lat_model))

  # obtain leverage (diagonal of the hat matrix); it only depends on the pixel
  # coordinates, so it is the same for the longitude and latitude models
  leverage <- unname(stats::hatvalues(lon_model))

  # calculate leave-one-out residuals as: e_i / (1 - h_ii)
  loo <- res / (1 - leverage)
  # loo cannot be computed for GCPs with leverage = 1. 
  loo_ok <- leverage < 1 - sqrt(.Machine$double.eps)
  loo[!loo_ok, ] <- NA
  if (any(!loo_ok)) {
    warning(
      "Leave-one-out residuals could not be computed for ",
      sum(!loo_ok), " GCP(s), as the polynomial of order ", poly_order,
      " cannot be fitted without them. ",
      "A polynomial of order ", poly_order, " requires more than ", n_coef,
      " GCPs (and ideally many more) to compute leave-one-out residuals."
    )
  }

  # build output table from the original GCP table
  out <- data.frame(
    gcp,
    # position predicted
    fitted_lon = fitted[, 1],
    fitted_lat = fitted[, 2],
    # error in degrees
    res_lon = res[, 1],
    res_lat = res[, 2],
    # error in km
    res_km = gcp_great_circle_km(
      gcp$longitude, gcp$latitude,
      fitted[, 1], fitted[, 2]
    ),
    # get leverage
    leverage = leverage,
    # leave-one-out residuals in degrees
    loo_lon = loo[, 1],
    loo_lat = loo[, 2],
    # leave-one-out residuals in km
    loo_km = gcp_great_circle_km(
      gcp$longitude, gcp$latitude,
      gcp$longitude - loo[, 1], gcp$latitude - loo[, 2]
    )
  )
  # store polynomial order
  attr(out, "order") <- poly_order
  # overall error of the fit
  attr(out, "rmse_km") <- sqrt(mean(out$res_km^2))
  # overall error of the leave-one-out residuals
  attr(out, "loo_rmse_km") <- sqrt(mean(out$loo_km^2))
  return(out)
}

# helper function to compute great-circle distance between two sets of points
# via Great-circle (haversine) distance in km
# used fixed Earth radius
gcp_great_circle_km <- function(lon1, lat1, lon2, lat2,
                                radius = 6371.0088) {
  # convert degrees to radians
  to_rad <- pi / 180
  # difference in lat
  dlat <- (lat2 - lat1) * to_rad
  # difference in lon
  dlon <- (lon2 - lon1) * to_rad
  # haversine formula from o (smae points) to 1(opposite sides of Earth) scale
  a <- sin(dlat / 2)^2 +
    # cosa here accounts for longitude degrees getting shorter as you move away
    # from the equator
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  # turn into angle between point and multiply to radius to get distance in km
  # pmin used to cap to 1 to avoid rounding errors as asin(>1) = NaN
  return(2 * radius * asin(pmin(1, sqrt(a))))
}
