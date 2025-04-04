test_that("geom_tissot works with differnet inputs",{
  library(rnaturalearth)
  library(sf)
  library(ggplot2)
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
#  suppressMessages(ggplot2::ggsave(tmp3, p3))
  # STOP something is wrong here???
  
})