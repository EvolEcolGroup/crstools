
  

library(sf)
library(ggplot2)
TissotIndicatrix <- ggproto("TissotIndicatrix", Stat,
                            compute_layer = function(self, data, params, layout) {
                              # add coord to the params, so it can be forwarded to compute_group()
                              params$coord <- layout$coord
                              ggproto_parent(Stat, self)$compute_layer(data, params, layout)
                            },
                            compute_panel = function(data, scales, coord) {
                              geometry_data <- data[[ geom_column(data) ]]
                              geometry_crs <- sf::st_crs(geometry_data)
                              
                              bbox <- sf::st_bbox(geometry_data)
                              
                              if (inherits(coord, "CoordSf")) {
                                # if the coord derives from CoordSf, then it
                                # needs to know about bounding boxes of geometry data
                                coord$record_bbox(
                                  xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
                                  ymin = bbox[["ymin"]], ymax = bbox[["ymax"]]
                                )
                                
                                # to represent the location of the geometry in default coordinates,
                                # we take the mid-point along each side of the bounding box and
                                # backtransform
                                bbox_trans <- sf_transform_xy(
                                  list(
                                    x = c(rep(0.5*(bbox[["xmin"]] + bbox[["xmax"]]), 2), bbox[["xmin"]], bbox[["xmax"]]),
                                    y = c(bbox[["ymin"]], bbox[["ymax"]], rep(0.5*(bbox[["ymin"]] + bbox[["ymax"]]), 2))
                                  ),
                                  coord$get_default_crs(),
                                  geometry_crs
                                )
                                
                                # record as xmin, xmax, ymin, ymax so regular scales
                                # have some indication of where shapes lie
                                data$xmin <- min(bbox_trans$x)
                                data$xmax <- max(bbox_trans$x)
                                data$ymin <- min(bbox_trans$y)
                                data$ymax <- max(bbox_trans$y)
                              } else {
                                # for all other coords, we record the full extent of the
                                # geometry object
                                data$xmin <- bbox[["xmin"]]
                                data$xmax <- bbox[["xmax"]]
                                data$ymin <- bbox[["ymin"]]
                                data$ymax <- bbox[["ymax"]]
                              }
                              
                              data
                            },
                            
                            setup_params = function(data, params){
                              # do stuff to params
                              browser()
                              
                              
                              return(params)
                            },
                            setup_data = function(data, params) {
                              # do stuff to data with a PANEL and group column
                              browser()
                              return(data)
                            }
)


stat_tissot <- function(mapping = NULL, data = NULL, 
                       geom = "rect", position = "identity", 
                       na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, centers = NULL,
                       radius = NULL, ...) {
  layer(
    stat = TissotIndicatrix, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, centers = centers,
                  radius = radius, ...)
  )
}



nc <-  st_read(system.file("shape/nc.shp", package="sf"))
ggplot(nc) + 
  geom_sf() +
  ggtitle('original') +
#  stat_sf2()
 # geom_sf(stat=TissotIndicatrix, color='red', size=0.5, centers = c(-80, 35), radius = 1)
  stat_tissot(centers = c(-80, 35), radius = 1, color = 'red', size = 0.5)
  
  


nc_2 <- ggplot(nc) +
  geom_sf() + 
  scale_y_continuous(breaks = c(34, 35, 36)) + 
  scale_x_continuous(breaks = seq(-84, -76, by = 1)) +
  coord_sf(crs="+proj=lcc +lon_0=-90 +lat_1=33 +lat_2=45")+
  ggtitle('fewer lat, more lon')



####################################
set.seed(123)
Npts <- 100
pts <- matrix(runif(2*Npts), ncol = 2) %>% 
  st_multipoint() %>% 
  st_sfc() %>% 
  st_cast("POINT") %>% 
  st_sf()

library(dplyr)

StatEnvelope <- ggproto(
  "StatEnvelope", Stat,
  required_aes = "geometry",
  compute_group = function(data, scales) {
    if(nrow(data) <= 2) return (NULL)
    data %>%
      group_by_at(vars(-geometry)) %>%
      summarise(geometry = sf::st_convex_hull(sf::st_combine(geometry))) %>%
      ungroup()
  }
)

ggplot(pts) + 
  geom_sf() +
  geom_sf(stat = StatEnvelope, 
          alpha = 0.5, color = "grey20", fill = "white", size = 0.5) +
  theme_bw()


