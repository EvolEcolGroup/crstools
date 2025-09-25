# reference image
map_jpg <- "europe_map.jpeg"

## Load image

library(jpeg)
img <- readJPEG(map_jpg)

# use locator to create points
if (FALSE){
  grDevices::x11()
  plot(0,0, xlim=c(0,dim(img)[1]), ylim=c(0,dim(img)[2]), type="n", xlab="", ylab="")
  graphics::rasterImage(img,0,0,dim(img)[1],dim(img)[2])
  gcp <- graphics::locator(n=12, type="p") # change number of points

  # add numbers to those points
  gcp_df <- data.frame(id = 1:length(gcp$x),
                          x = gcp$x, y = gcp$y)
  graphics::text(gcp_df$x, gcp_df$y, labels = gcp_df$id, col = "blue",pos=2)
  # set the longitudes and latitudes
  gcp_df$lon <- c(-20, 0, 20, 40, -20, 0, 20, -20, 0, -20, 0, 20)
  gcp_df$lat <- c(70, 70, 70, 70, 60, 60, 60, 50, 50, 40, 40, 40)
  # save the points
  write.csv(gcp_df, "europe_gcp.csv")
}

# alternatively, use a predefined set of points
gcp_df <- read.csv("europe_gcp.csv", row.names = 1)
# plot the points
grDevices::x11()
plot(0,0, xlim=c(0,dim(img)[1]), ylim=c(0,dim(img)[2]), type="n", xlab="", ylab="")
graphics::rasterImage(img,0,0,dim(img)[1],dim(img)[2])
graphics::points(gcp_df$x, gcp_df$y, pch=19)
graphics::text(gcp_df$x, gcp_df$y, labels = gcp_df$id, col = "blue",pos=2)

# flip the y axis such that the top left corner is 0,0
gcp_df$y <- abs(gcp_df$y-dim(img)[2]) # flip the y axis

# georeference the image
map_tif <- gsub(".jpeg", ".tif", map_jpg) # replace file extension to tif
library(sf)
gdal_utils("translate",
           source = map_jpg,
           dest = map_tif,
           options =
             c(as.vector(t(cbind("-gcp",gcp_df[,-1]))),
               "-of", "GTiff"
             )
)

# and warp it
map_warp_tif <- gsub(".tif", "_warp.tif", map_tif) # replace file extension to tif
sf::gdal_utils("warp",
               source = map_tif,
               dest = map_warp_tif,
               options =
                 c("-tps",
                   "-s_srs", "EPSG:4326",
#                   "-t_srs", "EPSG:3857",
                    "-t_srs", "EPSG:4326",
                    "-overwrite"))

# now check it
map_warp <- terra::rast(map_warp_tif)
library(terra)
plot(map_warp)
# add lng and lat to those points
#https://stackoverflow.com/questions/40542685/how-to-jitter-remove-overlap-for-geom-text-labels
# reshape it

# plot it as a raster together with the map as a transparency
library(tidyterra)
ggplot() +
  geom_spatraster_rgb(data = map_warp)

library("rnaturalearth")
world <- ne_countries(scale = "medium", returnclass = "sf")
world_sub <- st_crop(world, as.vector(terra::ext(map_warp)))

ggplot() +
  geom_spatraster_rgb(data = map_warp)+
  geom_sf(
    data = world_sub,
    color = "orange",
    fill = "transparent"
  ) +
  coord_sf(expand = FALSE)


#ggmap::gggraphics::locator()
