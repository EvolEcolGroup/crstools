#' Find the coordinates (longitude and latitude) of the ground control points (GCPs) in a given image.
#' 
#' @param gcp A data frame containing the GCPs with columns `id`, `x`, `y`, `longitude`, and `latitude`.
#' @sf_obj the reference map, as an sf object (already cut to the extend and if needed projected)
#' @returns A data frame with the GCPs, including their image coordinates and corresponding geographic coordinates.
#' @export

find_gcp_coords <- function(gcp, sf_obj) {
  if (!is.data.frame(gcp) || !all(c("id", "x", "y", "longitude", "latitude") %in% colnames(gcp))) {
    stop("gcp must be a data frame with columns: id, x, y, longitude, latitude.")
  }

  # check that sf_obj is an sf object
  if (!inherits(sf_obj, "sf")) {
    stop("sf_obj must be an sf object.")
  }

  # find the first point that has no coordinates
  first_missing <- which(is.na(gcp$longitude) | is.na(gcp$latitude))[1]
  
  # return if we have coordinates for all gcp
  if (is.na(first_missing)){
    stop ("all GCPs already have coordinates")
  }
  
  # plot the reference map
  if (capabilities("X11")) {
    x11()
  } else {
    quartz()
  }
  plot(sf::st_geometry(sf_obj))

  # if some gcp already have coordinates, plot them on this map (TODO)
  if (first_missing > 1) {
    points(gcp$longitude, gcp$latitude, col = "blue", pch = 19)
    text(gcp$longitude, gcp$latitude, labels = gcp$id, col = "blue", pos = 2)
  }
  
  # get coordinates of additional points
  coords <- locator(n = nrow(gcp), type = "p", col = "red", pch = 19)
  
  #add the coordinates to the gcp dataframe
  gcp$longitude[first_missing:length(coords$x)] <- coords$x
  gcp$latitude[first_missing:length(coords$y)] <- coords$y
  
  return(gcp)
}