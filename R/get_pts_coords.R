#' Extract geographic coordinates from pixel coordinates
#'
#' Transform target point pixel coordinates into longitude/latitude coordinates
#' using a set of Ground Control Points (GCPs). This function performs direct
#' point-wise coordinate transformation without warping a raster.
#'
#' Polynomial transformations are implemented using native linear models, while
#' thin plate spline transformations are implemented using
#' \code{fields::Tps()}.
#'
#' Note that, when using `tps` as a method, you might get warnings that the
#' "Grid searches over lambda (nugget and sill variances) with minima at
#' the endpoints:" This is often benign warning and indicates that the optimal
#' smoothing parameter is at or near the boundary of the search space (matching
#' the default lamba 1e-4, and thus approaching perfect interpolation).
#'
#' @param gcp A data frame containing the Ground Control Points (GCPs). This
#'   dataframe can be produced with the \code{draw_gcp_points} function. This
#'   data frame should have the following columns:
#'   \itemize{
#'     \item \code{id}: An identifier for each GCP (numeric).
#'     \item \code{x}: The x-coordinate of the GCP (in pixel space).
#'     \item \code{y}: The y-coordinate of the GCP (in pixel space).
#'     \item \code{lon}: The longitude of the GCP (georeferenced).
#'     \item \code{lat}: The latitude of the GCP (georeferenced).
#'   }
#'
#' @param target_pts A data frame containing the target points to transform.
#'   This dataframe must contain the following columns:
#'   \itemize{
#'     \item \code{id}: An identifier for each target point.
#'     \item \code{x} or \code{cx}: The x-coordinate of the point
#'       (in pixel space).
#'     \item \code{y} or \code{cy}: The y-coordinate of the point
#'       (in pixel space).
#'   }
#'
#' @param transform_method A character string specifying the transformation
#'   method to be used for warping the image. Options are \code{"poly_1"}
#'   (first order polynomial), \code{"poly_2"} (second order polynomial),
#'   \code{"poly_3"} (third order polynomial), \code{"tps"} (thin plate
#'   spline), or \code{"auto"} (the default, automatically selecting a
#'   polynomial order based on the number of GCPs available).
#'
#' @param lambda Numeric smoothing parameter used for thin plate spline
#'   transformations (\code{transform_method = "tps"}). A value of \code{1e-4}
#'   produces near-exact interpolation through the GCPs, mimicking GDAL TPS
#'   behaviour whilst helping with algorithm convergence. Larger values
#'   introduce smoothing and may improve robustness
#'   when GCPs contain noise or digitizing errors.
#'
#' @return A data frame containing:
#'   \itemize{
#'     \item \code{id}: The original target point identifier.
#'     \item Pixel coordinate columns from the original dataframe.
#'     \item \code{lon}: Estimated longitude.
#'     \item \code{lat}: Estimated latitude.
#'   }
#'
#' @export
get_pts_coords <- function(
  gcp,
  target_pts,
  transform_method = "auto",
  lambda = 1e-4
) {
  # ---------------------------------------------------------------------------
  # Validate transform method
  # ---------------------------------------------------------------------------

  valid_methods <- c(
    "auto",
    "poly_1",
    "poly_2",
    "poly_3",
    "tps"
  )

  if (!transform_method %in% valid_methods) {
    stop(
      "Invalid 'transform_method'. Must be one of: ",
      paste(valid_methods, collapse = ", ")
    )
  }

  # ---------------------------------------------------------------------------
  # Validate lambda
  # ---------------------------------------------------------------------------

  if (!is.numeric(lambda) || length(lambda) != 1 || is.na(lambda)) {
    stop("'lambda' must be a single numeric value.")
  }

  if (lambda < 0) {
    stop("'lambda' must be >= 0.")
  }

  # ---------------------------------------------------------------------------
  # Validate required columns in GCP dataframe
  # ---------------------------------------------------------------------------

  required_gcp_cols <- c("id", "x", "y", "lon", "lat")

  missing_gcp_cols <- setdiff(required_gcp_cols, names(gcp))

  if (length(missing_gcp_cols) > 0) {
    stop(
      "Missing required columns in 'gcp': ",
      paste(missing_gcp_cols, collapse = ", ")
    )
  }

  # ---------------------------------------------------------------------------
  # Allow x/y OR cx/cy in target points
  # ---------------------------------------------------------------------------

  if (all(c("x", "y") %in% names(target_pts))) {
    target_pts$x_internal <- target_pts$x
    target_pts$y_internal <- target_pts$y
  } else if (all(c("cx", "cy") %in% names(target_pts))) {
    target_pts$x_internal <- target_pts$cx
    target_pts$y_internal <- target_pts$cy
  } else {
    stop(
      "target_pts must contain either columns ('x', 'y') ",
      "or ('cx', 'cy')."
    )
  }

  # ---------------------------------------------------------------------------
  # Validate minimum number of GCPs needed for each transformation
  # ---------------------------------------------------------------------------

  n_gcp <- nrow(gcp)

  if (transform_method == "poly_1" && n_gcp < 3) {
    stop("At least 3 GCPs are required for a first order polynomial.")
  }

  if (transform_method == "poly_2" && n_gcp < 6) {
    stop("At least 6 GCPs are required for a second order polynomial.")
  }

  if (transform_method == "poly_3" && n_gcp < 10) {
    stop("At least 10 GCPs are required for a third order polynomial.")
  }

  # ---------------------------------------------------------------------------
  # Automatically select polynomial order if requested
  # ---------------------------------------------------------------------------

  if (transform_method == "auto") {
    if (n_gcp >= 10) {
      transform_method <- "poly_3"
    } else if (n_gcp >= 6) {
      transform_method <- "poly_2"
    } else {
      transform_method <- "poly_1"
    }
  }

  # ---------------------------------------------------------------------------
  # Build prediction dataframe
  # ---------------------------------------------------------------------------

  pred_df <- data.frame(
    x = target_pts$x_internal,
    y = target_pts$y_internal
  )

  # ---------------------------------------------------------------------------
  # First-order polynomial
  # ---------------------------------------------------------------------------

  if (transform_method == "poly_1") {
    lon_model <- stats::lm(
      lon ~ x + y,
      data = gcp
    )

    lat_model <- stats::lm(
      lat ~ x + y,
      data = gcp
    )

    lon_pred <- stats::predict(lon_model, pred_df)
    lat_pred <- stats::predict(lat_model, pred_df)
  }

  # ---------------------------------------------------------------------------
  # Second-order polynomial
  # ---------------------------------------------------------------------------

  if (transform_method == "poly_2") {
    lon_model <- stats::lm(
      lon ~ x + y + I(x^2) + I(y^2) + I(x * y),
      data = gcp
    )

    lat_model <- stats::lm(
      lat ~ x + y + I(x^2) + I(y^2) + I(x * y),
      data = gcp
    )

    lon_pred <- stats::predict(lon_model, pred_df)
    lat_pred <- stats::predict(lat_model, pred_df)
  }

  # ---------------------------------------------------------------------------
  # Third-order polynomial
  # ---------------------------------------------------------------------------

  if (transform_method == "poly_3") {
    lon_model <- stats::lm(
      lon ~ x + y +
        I(x^2) + I(y^2) + I(x * y) +
        I(x^3) + I(y^3) +
        I(x^2 * y) + I(x * y^2),
      data = gcp
    )

    lat_model <- stats::lm(
      lat ~ x + y +
        I(x^2) + I(y^2) + I(x * y) +
        I(x^3) + I(y^3) +
        I(x^2 * y) + I(x * y^2),
      data = gcp
    )

    lon_pred <- stats::predict(lon_model, pred_df)
    lat_pred <- stats::predict(lat_model, pred_df)
  }

  # ---------------------------------------------------------------------------
  # Thin plate spline transformation
  # ---------------------------------------------------------------------------

  if (transform_method == "tps") {
    if (!requireNamespace("fields", quietly = TRUE)) {
      stop(
        "Package 'fields' is required for transform_method = 'tps'."
      )
    }

    gcp_xy <- as.matrix(gcp[, c("x", "y")])

    pred_xy <- as.matrix(pred_df)

    lon_model <- fields::Tps(
      x = gcp_xy,
      Y = gcp$lon,
      lambda = lambda
    )

    lat_model <- fields::Tps(
      x = gcp_xy,
      Y = gcp$lat,
      lambda = lambda
    )

    lon_pred <- stats::predict(
      lon_model,
      pred_xy
    )

    lat_pred <- stats::predict(
      lat_model,
      pred_xy
    )
  }

  # ---------------------------------------------------------------------------
  # Assemble result
  # ---------------------------------------------------------------------------

  result <- target_pts

  result$x_internal <- NULL
  result$y_internal <- NULL

  result$lon <- as.numeric(lon_pred)
  result$lat <- as.numeric(lat_pred)

  return(result)
}
