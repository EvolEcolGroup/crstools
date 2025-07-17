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
