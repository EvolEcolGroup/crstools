skip() # never run this test automatically, it only works interactively
img_path <- system.file("extdata/europe_map.jpeg", package = "crswizard")
## choose some points
gcp_europe <- choose_gcp(img_path)
# now get some more
gcp_europe <- choose_gcp(img_path, gcp = gcp_europe)
# create a map of europe to use to get the coordinates
library(sf)
library(rnaturalearth)
# load the world map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
# transform it to a suitable projection
world <- st_transform(world, crs = 4326) # WGS 84
# crop it to the extent of the image
europe <- st_crop(world, c(xmin = -25, ymin=25, xmax = 45, ymax = 70)) # approximate extent of the image

# get the coordinates for these points
new_gcp_europe <- find_gcp_coords(gcp_europe, sf_obj = europe)

# TODO add in descriptive steps e.g.
# 1. Choose 4 points
# 2. Choose a further 2 points
# etc
# Add a testthat operation to check number of points total

# We can now close the X11 windows that we had opened.


# Georeference the image using the created GCPs
georef_path <- georeference_img(image_obj = img_path, gcp = new_gcp_europe, output_path = file.path(tempdir(), "europe_map_georef"))
map_warp <- terra::rast(georef_path)

# plot the image in the default Rstudio plot window
library(ggplot2)
library(tidyterra)
ggplot() +
  geom_spatraster_rgb(data = map_warp) +
  geom_sf(
    data = europe,
    color = "orange",
    fill = "transparent"
  ) +
  coord_sf(expand = FALSE)

# We now use locator to get the coordinates of the blue dots
# Write a nicer function to do this (we want markers every time we click)
# we want to be able to give it an existing dataframe, plot the current points and add to it
blue_coords <- locator(n = 6)
