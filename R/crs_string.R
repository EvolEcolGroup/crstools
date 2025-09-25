#' Function to format the PROJ.4 and WKT strings
#'
#' This function takes the parameters of a custom projection and returns the
#' PROJ.4 and WKT strings.
#'
#' Note that the WKT string contains a few special characters to define
#' indentation; these should not impact the parsing of the wkt string (eg. by
#' `sf`)
#'
#' @param prj Character string. The projection type. Options are "aeqd"
#'   (Azimuthal Equidistant), "laea" (Lambert Azimuthal Equal Area), "stere"
#'   (Stereographic), "aea" (Albers Equal Area), "eqdc" (Equidistant Conic),
#'   "lcc" (Lambert Conformal Conic), "cea" (Cylindrical Equal Area), "merc"
#'   (Mercator), "eqc" (Equidistant Cylindrical), "eqearth" (Equal Earth),
#'   "moll" (Mollweide), "hammer" (Hammer), "eck4" (Eckert IV), "wag4" (Wagner IV),
#'   "wag7" (Wagner VII), "tpeqd" (Two Point Equidistant), "robin" (Robinson),
#'   "natearth" (Natural Earth), "wintri" (Winkel Tripel), "patterson" (Patterson),
#'   "latlong" (Latitude/Longitude), "mill" (Miller Cylindrical), "tmerc" (Transverse
#'   Mercator), "cass" (Cassini), "tcea" (Transverse Cylindrical Equal Area).
#'   .
#' @param x0 Numeric. The false easting value.
#' @param lat0 Numeric. The latitude of origin.
#' @param lat1 Numeric. The first standard parallel.
#' @param lat2 Numeric. The second standard parallel.
#' @param lon0 Numeric. The central meridian.
#' @param k0 Numeric. The scale factor at the central meridian.
#' @param datum Character string. The datum. Options are "WGS84", "ETRS89",
#'   "NAD83".
#' @param unit Character string. The linear unit. Options are "m" (meters) and
#'   "ft" (feet).
#' @return A list with two elements: "proj4" and "wkt". The "proj4" element
#'   contains the PROJ.4 string, while the "wkt" element contains the WKT
#'   string.
#' @keywords internal

################################################################################
# Function to format the PROJ.4 and WKT strings
crs_string <- function(
    prj = c(
      "aeqd",
      "laea",
      "stere",
      "aea",
      "eqdc",
      "lcc",
      "cea",
      "merc",
      "eqc",
      "eqearth",
      "moll",
      "hammer",
      "eck4",
      "wag4",
      "wag7",
      "tpeqd",
      "robin",
      "natearth",
      "wintri",
      "patterson",
      "latlong",
      "mill",
      "tmerc",
      "cass",
      "tcea"
    ),
    x0,
    lat0,
    lat1,
    lat2,
    lon0,
    k0,
    datum = c("WGS84", "ETRS89", "NAD83"),
    unit = c("m", "ft")) {
  # Check if the input is correct
  prj <- match.arg(prj)
  datum <- match.arg(datum)

  proj_str <- "+proj="
  wkt_str <- "PROJCS[\"ProjWiz_Custom_"

  # Formatting Geographic/Geodetic Datum
  gcs_datum_str <- switch(datum,
    "WGS84" = {
      c(
        "GEOGCS[\"GCS_WGS_1984\",DATUM[\"D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137.0,298.257223563]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],", # nolint
        " +datum=WGS84"
      )
    },
    "ETRS89" = {
      c(
        "GEOGCS[\"GCS_ETRS_1989\",DATUM[\"D_ETRS_1989\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],", # nolint
        " +ellps=GRS80"
      )
    },
    "NAD83" = {
      c(
        "GEOGCS[\"GCS_North_American_1983\",DATUM[\"D_North_American_1983\",SPHEROID[\"GRS_1980\",6378137.0,298.257222101]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],", # nolint
        " +datum=NAD83"
      )
    },
    stop("invalid datum")
  )
  gcs_str <- gcs_datum_str[1]
  datum_str <- gcs_datum_str[2]

  # Formatting Projection
  proj_str <- paste0(proj_str, ifelse(prj == "latlong", "eqc", prj))

  wkt_str <- paste0(
    wkt_str,
    switch(prj,
      "aeqd" = paste0(
        'Azimuthal_Equidistant",',
        gcs_str,
        'PROJECTION["Azimuthal_Equidistant"],'
      ),
      "laea" = paste0(
        'Lambert_Azimuthal",',
        gcs_str,
        'PROJECTION["Lambert_Azimuthal_Equal_Area"],'
      ),
      "stere" = paste0(
        'Stereographic",',
        gcs_str,
        'PROJECTION["Stereographic"],'
      ),
      "aea" = paste0(
        'Albers",',
        gcs_str,
        'PROJECTION["Albers"],'
      ),
      "eqdc" = paste0(
        'Equidistant_Conic",',
        gcs_str,
        'PROJECTION["Equidistant_Conic"],'
      ),
      "lcc" = paste0(
        'Lambert_Conformal_Conic",',
        gcs_str,
        'PROJECTION["Lambert_Conformal_Conic"],'
      ),
      "cea" = paste0(
        'Cylindrical_Equal_Area",',
        gcs_str,
        'PROJECTION["Cylindrical_Equal_Area"],'
      ),
      "merc" = paste0(
        'Mercator",',
        gcs_str,
        'PROJECTION["Mercator"],'
      ),
      "eqc" = paste0(
        'Equidistant_Cylindrical",',
        gcs_str,
        'PROJECTION["Equidistant_Cylindrical"],'
      ),
      "tcea" = paste0(
        'Transverse_Cylindrical_Equal_Area",',
        gcs_str,
        'PROJECTION["Transverse_Cylindrical_Equal_Area"],'
      ),
      "tmerc" = paste0(
        'Transverse_Mercator",',
        gcs_str,
        'PROJECTION["Transverse_Mercator"],'
      ),
      "cass" = paste0(
        'Cassini",',
        gcs_str,
        'PROJECTION["Cassini"],'
      ),
      "moll" = paste0(
        'Mollweide",',
        gcs_str,
        'PROJECTION["Mollweide"],'
      ),
      "hammer" = paste0(
        'Hammer_Aitoff",',
        gcs_str,
        'PROJECTION["Hammer_Aitoff"],'
      ),
      "eck4" = paste0(
        'Eckert_IV",',
        gcs_str,
        'PROJECTION["Eckert_IV"],'
      ),
      "eqearth" = paste0(
        'Equal_Earth",',
        gcs_str,
        'PROJECTION["Equal_Earth"],'
      ),
      "wag4" = paste0(
        'Wagner_IV",',
        gcs_str,
        'PROJECTION["Wagner_IV"],'
      ),
      "wag7" = paste0(
        'Wagner_VII",',
        gcs_str,
        'PROJECTION["Wagner_VII"],'
      ),
      "robin" = paste0(
        'Robinson",',
        gcs_str,
        'PROJECTION["Robinson"],'
      ),
      "natearth" = paste0(
        'Natural_Earth",',
        gcs_str,
        'PROJECTION["Natural_Earth"],'
      ),
      "wintri" = paste0(
        'Winkel_Tripel",',
        gcs_str,
        'PROJECTION["Winkel_Tripel"],'
      ),
      "patterson" = paste0(
        'Patterson",',
        gcs_str,
        'PROJECTION["Patterson"],'
      ),
      "latlong" = paste0(
        'Plate_Carree",',
        gcs_str,
        'PROJECTION["Plate_Carree"],'
      ),
      "mill" = paste0(
        'Miller_Cylindrical",',
        gcs_str,
        'PROJECTION["Miller_Cylindrical"],'
      ),
      "tpeqd" = paste0(
        'Two_Point_Equidistant",',
        gcs_str,
        'PROJECTION["Two_Point_Equidistant"],'
      ),
      stop("Projection not recognized. Please select a valid projection.")
    )
  )

  # Formatting Projection Parameters
  if (!is.na(x0)) {
    proj_str <- paste0(proj_str, " +x_0=", as.integer(x0))
    wkt_str <- paste0(
      wkt_str,
      'PARAMETER["False_Easting",',
      x0,
      '],PARAMETER["False_Northing",0.0],'
    )
  } else {
    wkt_str <-
      paste0(
        wkt_str,
        'PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],'
      )
  }

  # Round numerical values
  lat0 <- round(lat0, 7)
  lat1 <- round(lat1, 7)
  lat2 <- round(lat2, 7)
  lon0 <- round(lon0, 7)

  # Other proj parameters
  switch(prj,
    # Azimuthal Equidistant or Lambert azimuthal
    "aeqd" = ,
    "laea" = {
      proj_str <- paste0(proj_str, " +lon_0=", lon0, " +lat_0=", lat0)
      wkt_str <- paste0(
        wkt_str,
        'PARAMETER["Central_Meridian",',
        lon0,
        '],PARAMETER["Latitude_Of_Origin",',
        lat0,
        "],"
      )
    },
    # Stereographic
    "stere" = {
      if (is.na(k0)) {
        proj_str <- paste0(proj_str, " +lon_0=", lon0, " +lat_0=", lat0)
        wkt_str <- paste0(
          wkt_str,
          'PARAMETER["Central_Meridian",',
          lon0,
          '],PARAMETER["Scale_Factor",1.0],PARAMETER["Latitude_Of_Origin",',
          lat0,
          "],"
        )
      } else {
        proj_str <-
          paste0(proj_str, " +lon_0=", lon0, " +lat_0=", lat0, " +k_0=", k0)
        wkt_str <- paste0(
          wkt_str,
          'PARAMETER["Central_Meridian",',
          lon0,
          '],PARAMETER["Scale_Factor",',
          k0,
          '],PARAMETER["Latitude_Of_Origin",',
          lat0,
          "],"
        )
      }
    },

    # Albers, Equidistant conic, or Lambert Conformal conic
    "aea" = ,
    "eqdc" = ,
    "lcc" = {
      proj_str <- paste0(
        proj_str,
        " +lon_0=",
        lon0,
        " +lat_1=",
        lat1,
        " +lat_2=",
        lat2,
        " +lat_0=",
        lat0
      )
      wkt_str <- paste0(
        wkt_str,
        'PARAMETER["Central_Meridian",',
        lon0,
        '],PARAMETER["Standard_Parallel_1",',
        lat1,
        '],PARAMETER["Standard_Parallel_2",',
        lat2,
        '],PARAMETER["Latitude_Of_Origin",',
        lat0,
        "],"
      )
    },

    # Cylindrical equal-area, Equidistant cylindrical, or Mercator
    "cea" = ,
    "eqc" = ,
    "merc" = {
      proj_str <- paste0(proj_str, " +lon_0=", lon0, " +lat_ts=", lat1)
      wkt_str <- paste0(
        wkt_str,
        'PARAMETER["Central_Meridian",',
        lon0,
        '],PARAMETER["Standard_Parallel_1",',
        lat1,
        "],"
      )
    },

    # Transverse cylindrical equal-area, Transverse Mercator, or Cassini
    "tcea" = ,
    "tmerc" = ,
    "cass" = {
      if (is.na(k0)) {
        proj_str <- paste0(proj_str, " +lon_0=", lon0)
        wkt_str <- paste0(
          wkt_str,
          'PARAMETER["Central_Meridian",',
          lon0,
          '],PARAMETER["Scale_Factor",1.0],PARAMETER["Latitude_Of_Origin",0.0],'
        )
      } else {
        proj_str <- paste0(proj_str, " +lon_0=", lon0, " +k_0=", k0)
        wkt_str <- paste0(
          wkt_str,
          'PARAMETER["Central_Meridian",',
          lon0,
          '],PARAMETER["Scale_Factor",',
          k0,
          '],PARAMETER["Latitude_Of_Origin",0.0],'
        )
      }
    },

    # Mollweide, Hammer, Eckert IV, Equal Earth, Wagner IV, Wagner VII,
    # Robinson, Natural Earth, Patterson, Plate Carrée, Miller cylindrical I
    "moll" = ,
    "hammer" = ,
    "eck4" = ,
    "eqearth" = ,
    "wag4" = ,
    "wag7" = ,
    "robin" = ,
    "natearth" = ,
    "patterson" = ,
    "latlong" = ,
    "mill" = {
      proj_str <- paste0(proj_str, " +lon_0=", lon0)
      wkt_str <- paste0(wkt_str, 'PARAMETER["Central_Meridian",', lon0, "],")
    },

    # Winkel Tripel
    "wintri" = {
      proj_str <- paste0(proj_str, " +lon_0=", lon0)
      wkt_str <- paste0(
        wkt_str,
        'PARAMETER["Central_Meridian",',
        lon0,
        '],PARAMETER["Standard_Parallel_1",50.467],'
      )
    },

    # Two-point azimuthal Equidistant
    "tpeqd" = {
      proj_str <- paste0(
        proj_str,
        " +lat_1=",
        lat0,
        " +lon_1=",
        lat1,
        " +lat_2=",
        lat2,
        " +lon_2=",
        lon0
      )
      wkt_str <- paste0(
        wkt_str,
        'PARAMETER["Latitude_Of_1st_Point",',
        lat0,
        '],PARAMETER["Latitude_Of_2nd_Point",',
        lat2,
        '],PARAMETER["Longitude_Of_1st_Point",',
        lat1,
        '],PARAMETER["Longitude_Of_2nd_Point",',
        lon0,
        "],"
      )
    },

    # Default
    {
      proj_str <- proj_str
      wkt_str <- wkt_str
    }
  )

  # Formatting Linear Unit and Closing Strings
  proj_str <- paste0(
    proj_str,
    datum_str,
    switch(unit,
      "m" = " +units=m +no_defs",
      "ft" = " +units=ft +no_defs",
      return("")
    )
  )

  wkt_str <- paste0(
    wkt_str,
    switch(unit,
      "m" = 'UNIT["Meter",1.0]]',
      "ft" = 'UNIT["Foot",0.3048]]',
      return("")
    )
  )
  return(list(proj4 = proj_str, wkt = wkt_str))
}

#' function to feed the row of a crs_df to crs_string
#'
#' @param x a vector from a row of a crs_df
#' @param datum Character string. The datum. Options are "WGS84", "ETRS89",
#'   "NAD83".
#' @param unit Character string. The linear unit. Options are "m" (meters) and
#'   "ft" (feet).
#' @return A list with two elements: "proj4" and "wkt". The "proj4" element
#'   contains the PROJ.4 string, while the "wkt" element contains the WKT
#'   string.

crs_string_row <- function(x, datum, unit) {
  crs_string(
    prj = x$prj[1],
    x0 = x$x0[1],
    lat0 = x$lat0[1],
    lat1 = x$lat1[1],
    lat2 = x$lat2[1],
    lon0 = x$lon0[1],
    k0 = x$k0[1],
    datum,
    unit
  )
}
