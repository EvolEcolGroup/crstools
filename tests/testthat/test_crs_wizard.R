test_that("test input sanity", {
  # check if input is not an expected class
  site <- data.frame(latitude = c(11,8),
                     longitude = c(5,9))
  expect_error(suggest_crs(site, distortion = "equal_area"), 
               "x must be a numeric vector ")
  
  # check for correct coordinates
  expect_error(
    suggest_crs(c(-180, 180, 90, -90), distortion = "equal_area", return_best = FALSE),
    "lat_min must be smaller than lat_max"
  )
  expect_error(
    suggest_crs(c(180, -180, -90, 90), distortion = "equal_area", return_best = FALSE),
    "lon_min must be smaller than lon_max"
  )
  expect_error(
    suggest_crs(c(-180, 180, -91, 90), distortion = "equal_area"),
    "Latitude values must be between -90 and 90"
  )
  
})
  
  
  
  
