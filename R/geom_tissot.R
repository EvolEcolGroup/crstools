#' Add Tissot's indicatrix to a map
#'
#' This function adds Tissot's indicatrix to a map. Tissot's indicatrix is a
#' mathematical contrivance used in cartography to characterize local
#' distortions due to map projection.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. If
#'   specified and inherit.aes = TRUE (the default), it is combined with the
#'   default mapping at the top level of the plot. You must supply mapping if
#'   there is no plot mapping.
#' @param data The data to be displayed in this layer. There are two options: A
#'   sf object or a SpatRaster object
#' @param na.rm If FALSE, the default, missing values are removed with a
#'   warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA,
#'   the default, includes if any aesthetics are mapped. FALSE never includes,
#'   and TRUE always includes. It can also be a named logical vector to finely
#'   select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than
#'   combining with them. This is most useful for helper functions that define
#'   both data and aesthetics and shouldn't inherit behaviour from the default
#'   plot specification, e.g. [ggplot2::geom_point()].
#' @param centers Either a list with elements "lng" and "lat" giving the
#'   longitudes and latitudes of the grid of centers for the tissot's
#'   indicatrix, or a vector of length 2 with the number of rows and columns to
#'   generate such a grid automatically. The latter is passed to [pretty()] to
#'   get pretty breaks, and the exact number of Tissot's circles might differ
#'   from the input numbers (see [pretty()] for details. It defaults to c(5,5).
#' @param radius The radius of the circles (see [sf::st_buffer()] for an
#'   explanation of how units are created; we reccomend that you explicitely
#'   state your using with the `units::as_units()`, e.g `units::as_units(100,
#'   "km")`
#' @param fill The fill color of the circles
#' @param alpha The transparency of the circles
#' @param ... Other arguments passed on to [ggplot2::geom_sf()]
#' @return A ggplot2 object
#' @export

geom_tissot <- function(
    mapping = ggplot2::aes(),
    data = NULL,
    na.rm = FALSE, # nolint
    show.legend = NA, # nolint
    inherit.aes = TRUE, # nolint
    centers = c(5, 5),
    radius = NULL,
    fill = "red",
    alpha = 0.7,
    ...) {
  # if data is not null or an sf
  if (!is.null(data) && !inherits(data, "sf")) {
    # we can convert it if it is a SpatRaster or SpatVector
    if (inherits(data, "SpatRaster") || inherits(data, "SpatVector")) {
      data_bbox <- sf::st_bbox(terra::ext(data))
      # get crs from the data
      sf::st_crs(data_bbox) <- terra::crs(data)
      # create an sf object
      data <- sf::st_as_sf(sf::st_as_sfc(data_bbox))
    } else {
      stop("data must be either an sf object or a SpatRaster object")
    }
  }
  c(
    ggplot2::layer_sf(
      geom = ggplot2::GeomSf,
      data = data,
      mapping = mapping,
      stat = Tissot, # nolint
      position = "identity",
      show.legend = show.legend, # nolint
      inherit.aes = inherit.aes, # nolint
      params = list(
        na.rm = na.rm, centers = centers, radius = radius, # nolint
        fill = fill, alpha = alpha,
        ...
      )
    ),
    ggplot2::coord_sf(default = TRUE)
  )
}

Tissot <- ggplot2::ggproto("Tissot", ggplot2::StatSf, # nolint
  compute_panel = function(data, scales, coord, centers, radius) {
    # create new data with the indicatrix
    data <- create_indicatrix(data, scales, coord, centers, radius)
    ggplot2::StatSf$compute_panel(data, scales, coord)
  },
  required_aes = c("geometry")
)

create_indicatrix <- function(data, scales, coord, centers, radius) {
  data_bbox <- sf::st_bbox(data[[geom_column(data)]])
  orig_crs <- sf::st_crs(data_bbox)
  # if the bbox is not in crs 4326, we should reproject it
  if (orig_crs != sf::st_crs("EPSG:4326")) {
    data_bbox <- sf::st_transform(data_bbox, sf::st_crs("EPSG:4326"))
  }

  # if centers is a vector of two elements (and NOT a list),
  # then we generate the grid of centers
  if (!inherits(centers, "list") && length(centers) == 2) {
    # Generate sequences
    lon_seq <- pretty(c(data_bbox$xmin, data_bbox$xmax),
      n = centers[1] + 1
    )
    lat_seq <- pretty(c(data_bbox$ymin, data_bbox$ymax),
      n = centers[2] + 1
    )
    # remove first and last values
    lon_seq <- lon_seq[-c(1, length(lon_seq))]
    lat_seq <- lat_seq[-c(1, length(lat_seq))]

    coord_grid <- as.matrix(expand.grid(lon_seq, lat_seq))
    # if we have a list, we use the values in the list
  } else if (inherits(centers, "list") &&
               all(c("lng", "lat") %in% names(centers))) {
    coord_grid <- as.matrix(expand.grid(centers$lng, centers$lat))
  } else {
    stop(paste0("centers must be either a list with elements ",
                "'lng' and 'lat' or a vector of length 2"))
  }

  # create an sf of coord_grid with a lonlat crs
  coord_grid_sf <- sf::st_as_sf(
    data.frame(
      lon = coord_grid[, 1],
      lat = coord_grid[, 2]
    ),
    coords = c("lon", "lat"),
    crs = sf::st_crs("EPSG:4326")
  )

  # if radius is null, estimate distance between two points
  if (is.null(radius)) {
    dist_mat <- sf::st_distance(x = coord_grid_sf)
    diag(dist_mat) <- NA
    radius <- min(dist_mat, na.rm = TRUE) / 4
  }

  # create a buffer around the points
  coord_grid_sf_buffer <- sf::st_buffer(coord_grid_sf, radius)

  coord_grid_sf_buffer <- sf::st_transform(coord_grid_sf_buffer, orig_crs)

  new_data <- data.frame(
    geometry = coord_grid_sf_buffer,
    PANEL = 1,
    group = -1
  )
  return(new_data) # nolint
}


# copy of the ggplot2 internal function to find
# the geometry column in a data.frame
geom_column <- function(data) {
  w <- which(vapply(data, inherits, TRUE, what = "sfc"))
  if (length(w) == 0) {
    "geometry"
  } else {
    if (length(w) > 1) {
      cli::cli_warn("More than one geometry column present: taking the first")
    }
    w[[1]]
  }
}
