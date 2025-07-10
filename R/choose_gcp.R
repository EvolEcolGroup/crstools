#' Function to choose the Ground Control Point (GCP) on an image
#'
#' GCPs are used to georeference images by providing known coordinates for
#' specific points in the image.
#' @param image_obj An array representing the image (colour images are generally
#'   imported as an array of nx x ny x 3 colour channels), or a file path to the
#'   image (of type .jpg or XXXX).
#' @param gcp (optional) A dataframe of GCPs, containing columns `id`, `x`,
#'   `y`,`longitude`, `latitude`. This is used if we want to add additional GCP
#'   to an existing list (usually created by running this function multiple
#'   times).
#'  @param col The colour of the points to be plotted on the image. Default is
#' @return A dataframe with the GCPs, including the image coordinates and their
#'   corresponding geographic coordinates.
#' @export

choose_gcp <- function(image_obj, gcp = NULL, col = "red"){
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
    stop("gcp must be a dataframe with columns id, x, y, longitude, latitude.")
  }
  
  # if gcp is not null, check if it has the required columns
  if (!is.null(gcp)) {
    required_cols <- c("id", "x", "y", "longitude", "latitude")
    if (!all(required_cols %in% colnames(gcp))) {
      stop("gcp dataframe must contain columns: ", paste(required_cols, collapse = ", "))
    }
    last_id <- max(gcp$id, na.rm = TRUE)
    # check that the image dimensions are stored as an attribute
    if (!is.null(attr(gcp, "image_dims"))) {
      img_dims <- attr(gcp, "image_dims")
      if (!all(img_dims == dim(img))) {
        stop("Image dimensions do not match the dimensions stored in gcp.")
      }
    } else {
      attr(gcp, "image_dims") <- dim(img)
    }
    
  } else {
    gcp <- data.frame(id = integer(0), x = numeric(0), y = numeric(0), longitude = numeric(0), latitude = numeric(0))
    last_id <- 0
  }
  
  # open a new window to plot the image
  x11()
  # plot the image
  plot(0,0, xlim=c(0,dim(img)[1]), ylim=c(0,dim(img)[2]), type="n", xlab="x_pixels", ylab="y_pixels")
  # add the image to the plot
  rasterImage(img,0,0,dim(img)[1],dim(img)[2])

  if (last_id > 0) {
    # plot and add numbers for an existing set of gcp
    points(gcp$x, gcp$y, col = col, pch = 19)
    text(gcp$x, gcp$y, labels = gcp$id, col = col,pos=2)
  }
    
  
    
  gcp_xy <- locator(n=1000, type="p") # change number of points
  
  gcp_df_new <- data.frame(
    id = seq_len(length(gcp_xy$x)) + last_id,
    x = gcp_xy$x,
    y = gcp_xy$y,
    longitude = NA_real_,
    latitude = NA_real_
  )
  
  # combine the gcp tables
  gcp <- rbind(gcp, gcp_df_new)
  # readd the dims of the image as an attribute
  attr(gcp, "image_dims") <- dim(img)
  
 text(gcp$x, gcp$y, labels = gcp$id, col = col,pos=2)
 
 # return the gcp dataframe
  return(gcp)

}
