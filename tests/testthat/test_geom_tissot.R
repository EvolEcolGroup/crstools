test_that("geom_tissot draws indicatrices in each facet", {
  two_areas <- sf::st_as_sf(
    data.frame(area = c("a", "b")),
    geometry = sf::st_sfc(
      sf::st_polygon(list(
        rbind(c(-80, -60), c(-30, -60), c(-30, 10), c(-80, 10), c(-80, -60))
      )),
      sf::st_polygon(list(
        rbind(c(0, 30), c(40, 30), c(40, 70), c(0, 70), c(0, 30))
      ))
    ),
    crs = 4326
  )
  p <- ggplot2::ggplot(two_areas) +
    ggplot2::geom_sf() +
    geom_tissot() +
    ggplot2::facet_wrap(~area)
  tissot_data <- ggplot2::ggplot_build(p)$data[[2]]
  expect_setequal(unique(as.character(tissot_data$PANEL)), c("1", "2"))
  # each panel has the indicatrices of its own area
  for (i in 1:2) {
    centroids <- sf::st_coordinates(suppressWarnings(sf::st_centroid(
      tissot_data$geometry[tissot_data$PANEL == i]
    )))
    bbox <- sf::st_bbox(two_areas[i, ])
    expect_true(all(centroids[, "X"] > bbox["xmin"]))
    expect_true(all(centroids[, "X"] < bbox["xmax"]))
  }
})

skip_if_not_installed("rnaturalearth")
skip_if_not_installed("rnaturalearthdata")

# styler: off

test_that("geom_tissot works with differnet inputs",{
  suppressMessages(library(rnaturalearth))
  suppressMessages(library(sf))
  suppressMessages(library(ggplot2))
  s_america_sf <- rnaturalearth::ne_countries(continent = "South America",
                                              returnclass = "sf")
  s_am_equidist <- suggest_crs(s_america_sf, distortion = "equidistant")

  # different ways to pass the sf object
  # create plot by passing sf object to each geom
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = s_america_sf) +
    geom_tissot(data = s_america_sf) +
    ggplot2::coord_sf(crs = s_am_equidist$proj4)

  # give sf object to ggplot and let it dispatch to geoms
  p2 <- ggplot2::ggplot(s_america_sf) +
    ggplot2::geom_sf() +
    geom_tissot() +
    ggplot2::coord_sf(crs = s_am_equidist$proj4)

  tmp1 <- tempfile(fileext = ".svg")
  tmp2 <- tempfile(fileext = ".svg")
  suppressMessages(ggplot2::ggsave(tmp1, p2))
  suppressMessages(ggplot2::ggsave(tmp2, p2))
  expect_true(tools::md5sum(tmp1) == tools::md5sum(tmp2))

  # check that it works with terra
library(terra)
library(tidyterra)
# get example dataset from cran
pastclim::set_data_path(on_CRAN = TRUE)
europe_r <- pastclim::region_slice(
  time_bp = 0,
  bio_variables = c("bio01"),
  dataset = "Example",
  ext = pastclim::region_extent$Europe
)
europe_r_equal_area <- suggest_crs(europe_r, distortion = "equal_area")
p3 <-ggplot2::ggplot() +
  tidyterra::geom_spatraster(data = europe_r) +
  ggplot2::scale_fill_viridis_c(na.value = "transparent") +
  geom_tissot(data = europe_r) +
  ggplot2::coord_sf(crs = europe_r_equal_area$proj4)
tmp3 <- tempfile(fileext = ".svg")
 suppressMessages(ggplot2::ggsave(tmp3, p3))
expect_true(file.exists(tmp3))
})
