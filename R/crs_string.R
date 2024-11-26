#' Function to format the PROJ.4 and WKT strings
#'
#' This function takes the parameters of a custom projection and returns the PROJ.4 and WKT strings.
#'
#' @param prj Character string. The projection type. Options are "aeqd" (Azimuthal Equidistant), "laea" (Lambert Azimuthal Equal Area), "stere" (Stereographic), "aea" (Albers Equal Area), "eqdc" (Equidistant Conic), "lcc" (Lambert Conformal Conic), "cea" (Cylindrical Equal Area), "merc" (Mercator), "eqc" (Equidistant Cylindrical).
#' @param x0 Numeric. The false easting value.
#' @param lat0 Numeric. The latitude of origin.
#' @param lat1 Numeric. The first standard parallel.
#' @param lat2 Numeric. The second standard parallel.
#' @param lon0 Numeric. The central meridian.
#' @param k0 Numeric. The scale factor at the central meridian.
#' @param datum Character string. The datum. Options are "WGS84", "ETRS89", "NAD83".
#' @param unit Character string. The linear unit. Options are "m" (meters) and "ft" (feet).
#' @return A list with two elements: "proj4" and "wkt". The "proj4" element contains the PROJ.4 string, while the "wkt" element contains the WKT string.
#' @examples
#' crs_string("aeqd", 5000, 40, 50, 60, 30, NA, "WGS84", "m")
#' @keywords internal


################################################################################
# Function to format the PROJ.4 and WKT strings
crs_string <- function(prj, x0, lat0, lat1, lat2, lon0, k0, datum, unit) {

  PROJstr <- "+proj="
  WKTstr <- "PROJCS[\"ProjWiz_Custom_"

  # Formatting Geographic/Geodetic Datum
  gcs_str <- ""
  datum_str <- switch(
    datum,
    "WGS84" = {
      gcs_str <<- "GEOGCS[\"GCS_WGS_1984\",DATUM[\"D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137.0,298.257223563]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],"
      " +datum=WGS84"
    },
    "ETRS89" = {
      gcs_str <<- "GEOGCS[\"GCS_ETRS_1989\",DATUM[\"D_ETRS_1989\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],"
      " +ellps=GRS80"
    },
    "NAD83" = {
      gcs_str <<- "GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],"
      " +datum=NAD83"
    },
    return("")
  )

  if (datum_str == "") {
    stop("Datum not recognized. Please select a valid datum.")
  }

  # Formatting Projection
  PROJstr <- paste0(PROJstr, ifelse(prj == "latlong", "eqc", prj))

  WKTstr <- paste0(
    WKTstr,
    switch(
      prj,
      "aeqd" = paste0('Azimuthal_Equidistant",', gcs_str, 'PROJECTION["Azimuthal_Equidistant"],'),
      "laea" = paste0('Lambert_Azimuthal",', gcs_str, 'PROJECTION["Lambert_Azimuthal_Equal_Area"],'),
      "stere" = paste0('Stereographic",', gcs_str, 'PROJECTION["Stereographic"],'),
      "aea" = paste0('Albers",', gcs_str, 'PROJECTION["Albers"],'),
      "eqdc" = paste0('Equidistant_Conic",', gcs_str, 'PROJECTION["Equidistant_Conic"],'),
      "lcc" = paste0('Lambert_Conformal_Conic",', gcs_str, 'PROJECTION["Lambert_Conformal_Conic"],'),
      "cea" = paste0('Cylindrical_Equal_Area",', gcs_str, 'PROJECTION["Cylindrical_Equal_Area"],'),
      "merc" = paste0('Mercator",', gcs_str, 'PROJECTION["Mercator"],'),
      "eqc" = paste0('Equidistant_Cylindrical",', gcs_str, 'PROJECTION["Equidistant_Cylindrical"],'),
      # Add other projections here if necessary
      return("")
    )
  )

  # Give error if projection is not recognized
  if (WKTstr == "") {
    stop("Projection not recognized. Please select a valid projection.")
  }

  # Formatting Projection Parameters
  if (!is.na(x0)) {
    PROJstr <- paste0(PROJstr, " +x_0=", x0)
    WKTstr <- paste0(WKTstr, 'PARAMETER["False_Easting",', x0, '],PARAMETER["False_Northing",0.0],')
  } else {
    WKTstr <- paste0(WKTstr, 'PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],')
  }

  # Round numerical values
  lat0 <- round(lat0, 7)
  lat1 <- round(lat1, 7)
  lat2 <- round(lat2, 7)
  lon0 <- round(lon0, 7)

  # Other projection parameters
  switch(
    prj,
    "aeqd" = {
      PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_0=", lat0)
      WKTstr <- paste0(WKTstr, 'PARAMETER["Central_Meridian",', lon0, '],PARAMETER["Latitude_Of_Origin",', lat0, '],')
    },
    "laea" = {
      PROJstr <- paste0(PROJstr, " +lon_0=", lon0, " +lat_0=", lat0)
      WKTstr <- paste0(WKTstr, 'PARAMETER["Central_Meridian",', lon0, '],PARAMETER["Latitude_Of_Origin",', lat0, '],')
    },
    # Add other cases as needed
    NULL
  )

  # Formatting Linear Unit and Closing Strings
  PROJstr <- paste0(
    PROJstr,
    datum_str,
    switch(
      unit,
      "m" = " +units=m +no_defs",
      "ft" = " +units=ft +no_defs",
      return("")
    )
  )

  WKTstr <- paste0(
    WKTstr,
    switch(
      unit,
      "m" = 'UNIT["Meter",1.0]]',
      "ft" = 'UNIT["Foot",0.3048]]',
      return("")
    )
  )

  # Returning formatted links
  # paste0(
  #   " <a href='#' onclick='copyPROJstring(\"", PROJstr, "\")' class='linkPROJ4'>PROJ</a>",
  #   " <a href='#' onclick='copyWKTstring(\"", WKTstr, "\")' class='linkPROJ4'>WKT</a>"
  # )
  return(list(proj4 = PROJstr, wkt = WKTstr))
}

# Example usage
# crs_string("aeqd", 5000, 40, 50, 60, 30, NA, "WGS84", "m")

