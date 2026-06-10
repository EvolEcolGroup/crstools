#' Extract geographic coordinates from pixel coordinates using GDAL GCP transforms
#'
#' Transform target point pixel coordinates into longitude/latitude coordinates
#' using a set of Ground Control Points (GCPs) and GDAL's internal
#' transformation engine via \code{gdaltransform}. This function does not warp
#' a raster; instead it performs direct point-wise coordinate transformation.
#'
#' The function creates a temporary VRT dataset containing the supplied GCPs
#' and then uses \code{gdaltransform} to estimate geographic coordinates for
#' the supplied target points.
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
#'     \item \code{x}: The x-coordinate of the point (in pixel space).
#'     \item \code{y}: The y-coordinate of the point (in pixel space).
#'   }
#'
#' @param transform_method A character string specifying the transformation
#'   method to be used for warping the image. Options are \code{"poly_1"}
#'   (first order polynomial), \code{"poly_2"} (second order polynomial),
#'   \code{"poly_3"} (third order polynomial), \code{"tps"} (thin plate
#'   spline), or \code{"auto"} (the default, allowing GDAL to choose of a
#'   polynomial of the appropriate order based on the number of available GCP.
#'   Polynomials are best for standard maps directly captured from a publication
#'   (a first or second order polynomial is often sufficient), tps allows for
#'   scanning artefacts, but it is badly affected by any incorrect GCP.
#'
#' @return A data frame containing:
#'   \itemize{
#'     \item \code{id}: The original target point identifier.
#'     \item \code{x}: Original x pixel coordinate.
#'     \item \code{y}: Original y pixel coordinate.
#'     \item \code{lon}: Estimated longitude.
#'     \item \code{lat}: Estimated latitude.
#'   }
#'
#' @details
#' This function relies on GDAL command line utilities being available on the
#' system. Specifically, it uses:
#' \itemize{
#'   \item \code{gdal_translate} to create a temporary VRT with embedded GCPs.
#'   \item \code{gdaltransform} to transform the target point coordinates.
#' }
#'
#' No raster warping is performed; only point coordinates are transformed.
#'
#' Thin plate spline (\code{"tps"}) transformations can produce excellent local
#' accuracy on distorted historical maps, but may become unstable near edges or
#' if GCPs contain errors.
#'
#' First order polynomial (\code{"poly_1"}) is equivalent to an affine
#' transformation and is typically sufficient for modern maps with limited
#' distortion.
#'
#' @examples
#' \dontrun{
#'
#' # Example GCPs
#' gcp <- data.frame(
#'   id = 1:4,
#'   x = c(100, 500, 120, 520),
#'   y = c(200, 210, 800, 790),
#'   lon = c(-3.12, -3.00, -3.11, -2.99),
#'   lat = c(55.95, 55.96, 55.80, 55.81)
#' )
#'
#' # Target points
#' pts <- data.frame(
#'   id = 1:2,
#'   x = c(300, 350),
#'   y = c(400, 700)
#' )
#'
#' # Transform points
#' coords <- get_pts_coords(
#'   gcp = gcp,
#'   target_pts = pts,
#'   transform_method = "poly_1"
#' )
#'
#' print(coords)
#' }
#'
#' @export
get_pts_coords <- function(
    gcp,
    target_pts,
    transform_method = "auto"
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
  # Validate required columns in target points dataframe
  # ---------------------------------------------------------------------------
  
  required_target_cols <- c("id", "x", "y")
  
  missing_target_cols <- setdiff(required_target_cols, names(target_pts))
  
  if (length(missing_target_cols) > 0) {
    stop(
      "Missing required columns in 'target_pts': ",
      paste(missing_target_cols, collapse = ", ")
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
  # Create temporary files
  # ---------------------------------------------------------------------------
  
  # Temporary empty raster file
  temp_tif <- tempfile(fileext = ".tif")
  
  # Temporary VRT file that will store GCP information
  temp_vrt <- tempfile(fileext = ".vrt")
  
  # Temporary input and output point files
  pts_in <- tempfile(fileext = ".txt")
  pts_out <- tempfile(fileext = ".txt")
  
  # ---------------------------------------------------------------------------
  # Create a minimal dummy raster
  #
  # GDAL requires a raster dataset as the base for attaching GCPs.
  # We therefore create a tiny blank raster.
  # ---------------------------------------------------------------------------
  
  terra::writeRaster(
    terra::rast(matrix(1, ncol=1, nrow=1)
    ),
    temp_tif,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Build GCP arguments for gdal_translate
  # ---------------------------------------------------------------------------
  
  gcp_args <- unlist(
    apply(gcp, 1, function(row) {
      c(
        "-gcp",
        as.character(row["x"]),
        as.character(row["y"]),
        as.character(row["lon"]),
        as.character(row["lat"])
      )
    })
  )
  
  # ---------------------------------------------------------------------------
  # Create the VRT containing embedded GCPs
  # ---------------------------------------------------------------------------
  
  sf::gdal_utils(
    util = "translate",
    source = temp_tif,
    destination = temp_vrt,
    options = c(
      "-of", "VRT",
      gcp_args
    )
  )
  
  # ---------------------------------------------------------------------------
  # Write target points to temporary input file.
  #
  # gdaltransform expects:
  #   x y
  # per line.
  # ---------------------------------------------------------------------------
  
  utils::write.table(
    target_pts[, c("x", "y")],
    file = pts_in,
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Construct gdaltransform command depending on transform method
  # ---------------------------------------------------------------------------
  
  transform_args <- switch(
    transform_method,
    
    "auto" = "",
    
    "poly_1" = "-order 1",
    
    "poly_2" = "-order 2",
    
    "poly_3" = "-order 3",
    
    "tps" = "-tps"
  )
  
  # ---------------------------------------------------------------------------
  # Execute gdaltransform
  # ---------------------------------------------------------------------------
  
  cmd <- paste(
    "gdaltransform",
    transform_args,
    shQuote(temp_vrt),
    "<",
    shQuote(pts_in),
    ">",
    shQuote(pts_out)
  )
  
  system(cmd)
  
  # ---------------------------------------------------------------------------
  # Read transformed coordinates
  # ---------------------------------------------------------------------------
  
  transformed <- utils::read.table(
    pts_out,
    header = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # gdaltransform returns:
  #   lon lat z
  #
  # We only retain lon/lat.
  # ---------------------------------------------------------------------------
  
  result <- target_pts
  
  result$lon <- transformed$V1
  result$lat <- transformed$V2
  
  return(result)
}

