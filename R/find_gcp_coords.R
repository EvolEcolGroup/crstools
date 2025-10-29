#' Find the coordinates (longitude and latitude) of the ground control points
#' (GCPs) in a given image.
#'
#' @param gcp A data frame containing the GCPs with columns `id`, `x`, `y`,
#'   `longitude`, and `latitude`.
#' @param sf_obj the reference map, as an sf object (already cut to the extend
#'   and if needed projected)
#' @returns A data frame with the GCPs, including their image coordinates and
#'   corresponding geographic coordinates.
#' @export
#'
#' @examplesIf all(rlang::is_installed("rnaturalearth"),
#'   rlang::is_interactive()) # get the path to an example image included in the
#'   package and choose GCPs img_path <- system.file("extdata/europe_map.jpeg",
#'   package = "crstools") # choose some points gcp_europe <-
#'   choose_gcp(img_path) # now get some more gcp_europe <- choose_gcp(img_path,
#'   gcp = gcp_europe) # create a map of europe to use to get the coordinates
#'   world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#'   # transform it to a suitable projection world <- sf::st_transform(world,
#'   crs = 4326) # crop it to the extent of the image europe <-
#'   sf::st_crop(world, c(xmin = -25, ymin=25, xmax = 45, ymax = 70)) # get the
#'   coordinates for these points new_gcp_europe <- find_gcp_coords(gcp_europe,
#'   sf_obj = europe) # data frame with the GCPs print(new_gcp_europe)

find_gcp_coords <- function(gcp, sf_obj) {
  if (
    !is.data.frame(gcp) ||
      !all(c("id", "x", "y", "longitude", "latitude") %in% colnames(gcp))
  ) {
    stop(
      "gcp must be a data frame with columns: id, x, y, longitude, latitude."
    )
  }

  # check that sf_obj is an sf object
  if (!inherits(sf_obj, "sf")) {
    stop("sf_obj must be an sf object.")
  }

  # find the first point that has no coordinates
  first_missing <- which(is.na(gcp$longitude) | is.na(gcp$latitude))[1]

  # return if we have coordinates for all gcp
  if (is.na(first_missing)) {
    stop("all GCPs already have coordinates")
  }

  # plot the reference map
  grDevices::x11()
  plot(sf::st_geometry(sf_obj))

  # if some gcp already have coordinates, plot them on this map
  if (first_missing > 1) {
    graphics::points(gcp$longitude, gcp$latitude, col = "blue", pch = 19)
    graphics::text(
      gcp$longitude,
      gcp$latitude,
      labels = gcp$id,
      col = "blue",
      pos = 2
    )
  }
  message("Click on the map to add a new GCP. Press ESC to finish.\n")

  # get coordinates of additional points
  while (TRUE) {
    coords <- graphics::locator(n = 1, type = "p", col = "red", pch = 19)
    if (is.null(coords)) {
      break
    }

    # add this coordinates to the first missing value in gpc$longitude
    next_missing <- which(is.na(gcp$longitude) | is.na(gcp$latitude))[1]
    # add the coordinates to the gcp dataframe
    gcp$longitude[next_missing] <- coords$x
    gcp$latitude[next_missing] <- coords$y
    graphics::text(
      gcp$longitude[next_missing],
      gcp$latitude[next_missing],
      labels = gcp$id[next_missing],
      col = "blue",
      pos = 2
    )
  }

  return(gcp)
}
