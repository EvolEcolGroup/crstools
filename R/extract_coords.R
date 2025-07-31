#' @title Extract coordinates
#' @description Extract coordinates from a georeferenced image.
#' @param coords_df An optional dataframe containing the coordinates of points
#' previously extracted from the image. Default is NULL.
#' @param georef_image A spatraster object representing the georeferenced image.
#' @param col The colour of the points to be plotted on the image. Default is
#' "red".
#' @return A dataframe with ID and coordinates of the points extracted from the image.

extract_coords <- function(georef_image, coords_df = NULL, col = "red"){
# check that the image is a spatraster object 
  if (!inherits(georef_image, "SpatRaster")) {
    stop("georef_image must be a SpatRaster object.")
  }
  # check that the image has a CRS
  if (is.null(terra::crs(georef_image))) {
    stop("georef_image must have a coordinate reference system (CRS).")
  }
  # check if coords_df is a dataframe
  if (!is.null(coords_df) && !is.data.frame(coords_df)) {
    stop("coords_df must be a dataframe with columns id, longitude, and latitude.")
  }
  if (!is.null(coords_df)) {
    required_cols <- c("id", "longitude", "latitude")
    if (!all(required_cols %in% colnames(coords_df))) {
      stop("coords_df dataframe must contain columns: ", 
           paste(required_cols, collapse = ", "))
    }
  last_id <- max(coords_df$id, na.rm = TRUE)
  } else {
    coords_df <- data.frame(id = integer(0), 
                            longitude = numeric(0), 
                            latitude = numeric(0))
    last_id <- 0
  }
  
  
  # plot the image
  x11()
  # plot the image
  plot(0,0, xlim=c(0,dim(georef_image)[1]), ylim=c(0,dim(georef_image)[2]), 
       type="n", xlab="x_pixels", ylab="y_pixels")
  # add the image to the plot
  terra::plot(georef_image)
  
  if (last_id > 0) {
    # plot and add numbers for an existing set of gcp
    points(coords_df$longitude, coords_df$latitude, col = col, pch = 19)
    text(coords_df$longitude, coords_df$latitude, labels = coords_df$id, 
         col = col,pos=2)
  }
  
  # extract coordinates from the image
  coords <- locator(n=1000, type="p")
  
  # create id for points
  #id <- seq_len(length(coords$x) + last_id)
  id <- (last_id + 1) : (length(coords$x) + last_id)
  
  # plot the points on the image
  if (last_id == 0){
  text(coords$x, coords$y, labels = id, col = col, pos=2)
  } else {
    text(coords_df$longitude, coords_df$latitude, 
         labels = seq_len(last_id), col = col, pos=2)
    text(coords$x, coords$y, 
         labels = id, 
         col = col, pos=2)
  }
  
  if (!is.null(coords)){     
  coords_df_new <- data.frame(
      id = id,
      longitude = coords$x,
      latitude = coords$y
    )
  # combine the old and new dataframes 
  coords_df <- rbind(coords_df, coords_df_new)
  }
  
return(coords_df)

}