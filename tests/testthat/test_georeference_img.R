test_that("georeference_img works", {
  # get the path to an example image included in the package
  img_path <- system.file("extdata/europe_map.jpeg", package = "crstools")
  # load a set of GCPs generated with choose_gcp() but not yet passed through
  # find_gcp() to get coordinates
  gcp_df <- readRDS( system.file(
    "extdata/europe_gcp1.RDS", package = "crstools"))
  # Expect an error as the gcp does not have lat and long
  expect_error(georeference_img(image_obj = img_path, gcp = gcp_df),
               "gcp dataframe contains NA values.")
  # now check that we catch incorrect column headers
  gcp_df_invalid <- gcp_df
  colnames(gcp_df_invalid)[which(colnames(gcp_df_invalid) == "longitude")] <-
    "long"
  expect_error(georeference_img(image_obj = img_path, gcp = gcp_df_invalid),
               "gcp must be a data frame with columns:")
})