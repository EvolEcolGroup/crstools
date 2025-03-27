# this works just fine

library(ggplot2)

geom_tissot <- function(mapping = aes(), data = NULL, stat = "Tissot",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, centers = c(5,5), radius = NULL, fill = "red", ...) {
  c(
    layer_sf(
      geom = GeomSf,
      data = data,
      mapping = mapping,
      stat = Tissot,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm, centers = centers, radius = radius,
        fill = fill,
        ...
      )
    ),
    coord_sf(default = TRUE)
  )
}


Tissot2 <- ggproto("Tissot2", StatSf,
                   setup_params = function(data, params){
                     # do stuff to params
                     browser()
                     
                     
                     return(params)
                   },
                   setup_data = function(data, params, centers radius) {
                     # do stuff to data with a PANEL and group column
                     browser()
                     return(data)
                   },
                   required_aes = c("geometry")
                   
)


Tissot <- ggproto("Tissot", Stat,
                  compute_layer = function(self, data, params, layout) {
                    # add coord to the params, so it can be forwarded to compute_group()
                    params$coord <- layout$coord
                    ggproto_parent(Stat, self)$compute_layer(data, params, layout)
                  },
                  
                  # setup_params = function(data, params){
                  #   # do stuff to params
                  #   browser()
                  #   
                  #   
                  #   return(params)
                  # },
                  # setup_data = function(data, params) {
                  #   # do stuff to data with a PANEL and group column
                  #   browser()
                  #   return(data)
                  # },
                  compute_panel = function(data, scales,coord, centers, radius) {
                    
                    browser()
                    data_bbox <- sf::st_bbox(data[[ geom_column(data) ]])
                    # if the bbox is not in crs 4326, we should reproject it
                    if (sf::st_crs(data) != sf::st_crs("EPSG:4326")) {
                      data_bbox <- sf::st_transform(data_bbox, sf::st_crs("EPSG:4326"))
                    }
                    
                    # if centers is a vector of two elements (and NOT a list), then we generate the grid of centers
                    if (!inherits(centers, "list") && length(centers) == 2) {
                      # Generate sequences
                      lon_seq <- pretty(c(data_bbox$xmin, data_bbox$xmax),
                                        n = centers[1]+1)
                      lat_seq <- pretty(c(data_bbox$ymin, data_bbox$ymax),
                                        n = centers[2]+1)
                      # remove first and last values
                      lon_seq <- lon_seq[-c(1, length(lon_seq))]
                      lat_seq <- lat_seq[-c(1, length(lat_seq))]
                      
                      coord_grid <- as.matrix(expand.grid(lon_seq, lat_seq))
                      # if we have a list, we use the values in the list
                    } else if (inherits(centers, "list") && all(c("lng", "lat") %in% names(centers))) {
                      coord_grid <- as.matrix(expand.grid(centers$lng, centers$lat))
                    } else {
                      stop("centers must be either a list with elements 'lng' and 'lat' or a vector of length 2")
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
                      dist_mat <- sf::st_distance(x= coord_grid_sf)
                      diag(dist_mat) <- NA
                      radius <-  min(dist_mat, na.rm = TRUE)/4
                    }
                    
                    # create a buffer around the points
                    coord_grid_sf_buffer <- sf::st_buffer(coord_grid_sf, radius)
                    new_data <- data.frame(
                      geometry = coord_grid_sf_buffer,
                      PANEL = 1,
                      group = -1
                    )
                    return(new_data)
                  },
                  
                  required_aes = c("geometry")
)



# stat_tissot <- function(mapping = NULL, data = NULL, 
#                         geom = "rect", position = "identity", 
#                         na.rm = FALSE, show.legend = NA, 
#                         inherit.aes = TRUE, centers = NULL,
#                         radius = NULL, ...) {
#   layer(
#     stat = Tissot, 
#     data = data, 
#     mapping = mapping, 
#     geom = geom, 
#     position = position, 
#     show.legend = show.legend, 
#     inherit.aes = inherit.aes, 
#     params = list(na.rm = na.rm, centers = centers,
#                   radius = radius, ...)
#   )
# }


geom_column <- function (data) 
{
  w <- which(vapply(data, inherits, TRUE, what = "sfc"))
  if (length(w) == 0) {
    "geometry"
  }
  else {
    if (length(w) > 1) 
      cli::cli_warn("More than one geometry column present: taking the first")
    w[[1]]
  }
}



library(sf)
library(ggplot2)
nc <-  st_read(system.file("shape/nc.shp", package="sf"))
ggplot(nc) + 
  geom_sf() +
  ggtitle('original') +
  geom_sf(stat=Tissot2, fill='red', centers = c(5,5), radius = NULL) +
  coord_sf(crs = "+proj=merc")


  geom_tissot()
  
  
  geom_tissot(centers = c(5,5), radius = NULL, fill = 'red')
  
  
  #  stat_sf2()
  # geom_sf(stat=TissotIndicatrix, color='red', size=0.5, centers = c(-80, 35), radius = 1)
  geom_tissot(centers = c(5,5), radius = 100000, fill = 'red')+
  coord_sf()
