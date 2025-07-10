library("rnaturalearth")
library("sf")
library("ggplot2")

target_crs <- "+proj=moll"
world <- ne_countries(scale = "medium", returnclass = "sf")
# world <- world %>% st_transform(target_crs)
window_coord <- st_sf(st_sfc(st_point(c(-18, 32.5)),
                             st_point(c(45, 71)),
                             crs = 4326))
window_coord_sf <- st_transform(window_coord, target_crs) %>% st_coordinates()

locations <- data.frame(
  name = c("London", "Paris", "Berlin", "Madrid", "Rome", "Reykjavík"),
  lon = c(-0.1276, 2.3522, 13.4050, -3.7038, 12.4964, -21.9426),
  lat = c(51.5072, 48.8566, 52.5200, 40.4168, 41.9028, 64.1265)
) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

# plot Europe with ggplot2
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = locations, color = "blue") +
  theme_minimal() +
  coord_sf(xlim = window_coord_sf[, "X"], ylim = window_coord_sf[, "Y"], crs = target_crs)

# write a jpeg
ggsave("europe_map.jpeg", width = 10, height = 10, dpi = 300)
