#' Georeference an Image using Ground Control Points (GCPs)
#'
#' This function warps a given image using a set of Ground Control Points (GCPs) to create a georeferenced
#' version of the image. The function uses GDAL tools to first translate the image into a georeferenced TIFF
#' format and then applies a warp operation to reproject the image into a spatial reference system (EPSG:4326).
#' The warped image is saved as a new file with the suffix `_warp.tif`.
#'
#' @param image_obj A character string specifying the file path to the input image (JPEG format).
#' The function reads this image and applies the GCPs for georeferencing.
#'
#' @param gcp A data frame containing the Ground Control Points (GCPs). This dataframe can be produced withthe *draw_gcp_points* function. This data frame should have the following columns:
#'   - `id`: An identifier for each GCP (numeric).
#'   - `x`: The x-coordinate of the GCP (in pixel space).
#'   - `y`: The y-coordinate of the GCP (in pixel space).
#'   - `lon`: The longitude of the GCP (georeferenced).
#'   - `lat`: The latitude of the GCP (georeferenced).
#'
#' @param output_path A character string representing the file path to the input image.
#' (`_warp.tif`) will be appended to it.
#'
#' @return A character string representing the path to the newly created warped TIFF image file (`_warp.tif`).
#' This file contains the georeferenced image.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a set of GCPs in gcp_df and an image file "image.jpg"
#' warped_image <- georeference_image(image_path = "image.jpg", gcp_df = gcp_df)
#' }

georeference_img <- function(image_obj, gcp, output_path = NULL) {
  warning("this function is untested yet!")

  # check if image is a file path or an array
  if (is.character(image_obj)) {
    if (!file.exists(image_obj)) {
      stop("File does not exist: ", image_obj)
    }
    map_jpg <- image_obj
    # if image_obj is a file name, use it as the root of all filenames
    if (is.null(output_path)) {
      output_path <- tools::file_path_sans_ext(image_obj)
    }
  } else if (is.array(image_obj)) {
    # if image_obj is an array, use a default name for the output
    if (is.null(output_path)) {
      output_path <- "map"
    }
    # write a jpeg for gdal to use
    map_jpg <- paste0(output_path, ".jpg")
    # write the image to a jpeg file
    jpeg::writeJPEG(image_obj, map_jpg)
  } else {
    stop("Image must be a file path or an array.")
  }

  # Get dimensions without fully loading image
  img_height <- dim(jpeg::readJPEG(image_obj))[1]
  # Flip Y axis to match GDAL coordinate origin
  gcp$y <- img_height - gcp$y

  # Prepare output file names
  map_tif <- paste0(output_path, ".tif")
  map_warp_tif <- paste0(output_path, "_warp.tif")
  #  map_warp_tif_path <- file.path(output_path, map_warp_tif)

  # Georeference using GDAL translate with GCPs
  sf::gdal_utils(
    "translate",
    source = map_jpg,
    dest = map_tif,
    options = c(as.vector(t(cbind("-gcp", gcp[, -1]))), "-of", "GTiff")
  )

  # Warp the image into a spatial reference system (EPSG:4326)
  sf::gdal_utils(
    "warp",
    source = map_tif,
    dest = map_warp_tif,
    options = c(
      "-tps",
      "-s_srs",
      "EPSG:4326",
      "-t_srs",
      "EPSG:4326",
      "-overwrite"
    )
  )

  return(map_warp_tif)
}

# @TODO add optionto change the output CRS as a param
