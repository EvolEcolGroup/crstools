#' Function to plot the Ground Control Points (GCPs) on an raw image
#'
#' GCPs are used to georeference images by providing known coordinates for
#' specific points in the image. This function plots GCPs onto the original map
#' (i.e. using the pixel coordinates of the points on the original image before
#' georeferencing).
#' This can be helpful in thinking about here additional GCPs might be needed
#' to improve the transformation.
#' @param image_obj An array representing the image (colour images are generally
#'   imported as an array of nx x ny x 3 colour channels), or a file path to the
#'   image (currenly this can only be of type .jpg).
#' @param gcp A dataframe of GCPs, containing at least columns `id`, `x`,
#'   `y`.
#' @param col The colour of the points to be plotted on the image. Default is
#' "red".
#' @return A plot with the GCPs
#' @export
#'
#' @examplesIf rlang::is_interactive()
#' # Get the path to an example image included in the package
#' img_path <- system.file("extdata/europe_map.jpeg", package = "crstools")
#' # load some previously captured GCPs
#' gcp_europe <- readRDS(system.file(
#'   "extdata/europe_gcp_georef.RDS",
#'   package = "crstools"
#' ))
#' # plot
#' plot_gcp(img_path, gcp = gcp_europe)
plot_gcp <- function(image_obj, gcp = NULL, col = "red") {
  # check if image is a file path or an array
  if (is.character(image_obj)) {
    if (!file.exists(image_obj)) {
      stop("File does not exist: ", image_obj)
    }
    img <- jpeg::readJPEG(image_obj)
  } else if (is.array(image_obj)) {
    img <- image_obj
  } else {
    stop("Image must be a file path or an array.")
  }

  # check if image is a valid array
  if (length(dim(img)) != 3 || dim(img)[3] != 3) {
    stop("Image must be a colour image with three channels (RGB).")
  }
  # check if gcp is a dataframe
  if (!is.null(gcp) && !is.data.frame(gcp)) {
    stop("gcp must be a dataframe with columns id, x, y.")
  }

  # if gcp is not null, check if it has the required columns
  required_cols <- c("id", "x", "y")
  if (!all(required_cols %in% colnames(gcp))) {
    stop(
      "gcp dataframe must contain columns: ",
      paste(required_cols, collapse = ", ")
    )
  }
  # check that, if the image dimensions are stored as an attribute, they match
  if (!is.null(attr(gcp, "image_dims"))) {
    img_dims <- attr(gcp, "image_dims")
    if (!all(img_dims == dim(img))) {
      stop("Image dimensions do not match the dimensions stored in gcp.")
    }
  }

  # plot the image
  plot(
    0,
    0,
    xlim = c(0, dim(img)[1]),
    ylim = c(0, dim(img)[2]),
    type = "n",
    xlab = "x_pixels",
    ylab = "y_pixels"
  )
  # add the image to the plot
  graphics::rasterImage(img, 0, 0, dim(img)[1], dim(img)[2])

  # plot and add numbers for an existing set of gcp
  graphics::points(gcp$x, gcp$y, col = col, pch = 19)
  graphics::text(gcp$x, gcp$y, labels = gcp$id, col = col, pos = 2)
}
