#' Find the coordinates (longitude and latitude) of the ground control points (GCPs) in a given image.
#' 
#' @param gcp A data frame containing the GCPs with columns `id`, `x`, `y`, `longitude`, and `latitude`.
#' @sf_obj the reference map, as an sf object (already cut to the extend and if needed projected)
#' @returns A data frame with the GCPs, including their image coordinates and corresponding geographic coordinates.
#' @export

find_gcp_coords <- function(gcp, extent, sf_obj) {
  if (!is.data.frame(gcp) || !all(c("id", "x", "y", "longitude", "latitude") %in% colnames(gcp))) {
    stop("gcp must be a data frame with columns: id, x, y, longitude, latitude.")
  }

  # check that sf_obj is an sf object
  if (!inherits(sf_obj, "sf")) {
    stop("sf_obj must be an sf object.")
  }
  # plot the reference map
  plot(sf_obj, add = TRUE)
  
  # if some gcp already have coordinates, plot them on this map (TODO)
  
  # get coordinates of additional points
  coords <- locator(n = nrow(gcp), type = "p", col = "red", pch = 19)
  
  #TODO add the coordinates to the gcp dataframe

  
  return(gcp)
}