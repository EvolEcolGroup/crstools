#' Function to create the world equidistant projection
#' 
#' @param center The center of the map (named vector with lat and long)
#' @param scale The scale of the map
#' @param round_cm The round central meridian.
#' @param prj_details a list of options defining the desired projection 
#' @return The projection object

crs_world_equidistant <- function(center,
                                  scale,
                                  round_cm = FALSE,
                                  prj_details) {

  # make sure that we have a projection element in prj_details
  if (!inherits(prj_details,"list") || !"prj" %in% names(prj_details)) {
    stop("`world_equidistant` must be a list with a `prj` element and the appropriate projection details")
  }
  
  #outputTEXT <- ""
  
  # Formatting slider steps
  if (round_cm || scale < 1.15) {
    steps <- 1.0
  } else if (scale < 1.32) {
    steps <- 0.5
  } else {
    steps <- 0.1
  }
  
  # Formatting output
  if (prj_details$prj == "polar") {
    # Polar azimuthal equidistant
    if (!"pole" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `pole` element")
    }
    # pole should be either 90 or -90
    if (prj_details$pole != 90 && prj_details$pole != -90) {
      stop("`pole` must be either 90 or -90")
    }
    pole_eq <- prj_details$pole
    if (!"lng_central" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lng_central` element")
    }
    lng_central <- prj_details$lng_central
#    lngP_eq <- worldValues(center$lng, scale)
    pole_str <- ifelse(pole_eq > 0, "North Pole", "South Pole")
    
#    outputTEXT <- paste0(outputTEXT, "<p class='outputText'>Distances are correct through or from the <span id='pole_str'>", 
#                         pole_str, " - ", stringLinks("aeqd", NA, pole_eq, NA, NA, lngP_eq, NA), "</span><br></p>")
    
    
    prj_suggestions <- data.frame(
      prj = "aeqd", x0 = NA_real_, lat0 = pole_eq, lat1 = NA_real_, lat2 = NA_real_, lon0 = lng_central, k0 = NA_real_,
      description = "Polar azimuthal equidistant",
      notes = paste0("Distance correct through or from the ",pole_str ,
                     " Pole; Central meridian: ", lng_central,".")
    )
    
    
    
    # outputTEXT <- paste0(outputTEXT, "<p class='outputText'>Center latitude: <span id='pole_val'>", 
    #                      formatWorldLAT(pole_eq), "</span></p>")
    # 
    # outputTEXT <- paste0(outputTEXT, "<div class='sliderBox'><form id='pole_eq'>", 
    #                      "<input type='radio' name='pole_eq' id='North Pole' value='90'><label for='North Pole' style='font-size:11px;'>North Pole</label>", 
    #                      "<input type='radio' name='pole_eq' id='South Pole' value='-90'><label for='South Pole' style='font-size:11px;'>South Pole</label>", 
    #                      "</form></div>")
    
    # observeEvent(input$pole_eq, {
    #   pole_eq <- as.numeric(input$pole_eq)
    #   pole_str <- ifelse(pole_eq > 0, "North Pole", "South Pole")
    #   
    #   updateTextInput(session, "pole_val", value = formatWorldLAT(pole_eq))
    #   updateTextInput(session, "pole_str", value = paste0(pole_str, " - ", stringLinks("aeqd", NA, pole_eq, NA, NA, lngP_eq, NA)))
    #   
    #   addWorldMapPreview(center, activeWorldEqDistProj, TRUE)
    # })
    # 
    # updateRadioButtons(session, "pole_eq", selected = pole_str)
    # 
    # outputTEXT <- paste0(outputTEXT, "<p class='outputText'>Central meridian: <span id='lngP_val'>", 
    #                      formatWorldLON(lngP_eq), "</span></p>")
    # 
    # outputTEXT <- paste0(outputTEXT, "<div class='sliderBox'><div class='sliderTextL'>", formatWorldLON(-180.0), 
    #                      "</div><div class='sliderTextR'>", formatWorldLON(180.0), 
    #                      "</div><div id='lngP_eq' class='slider'></div></div>")
    # 
    # observeEvent(input$lngP_eq, {
    #   lngP_eq <- input$lngP_eq
    #   updateTextInput(session, "lngP_val", value = formatWorldLON(lngP_eq))
    #   updateTextInput(session, "pole_str", value = paste0(pole_str, " - ", stringLinks("aeqd", NA, pole_eq, NA, NA, lngP_eq, NA)))
    #   addWorldMapPreview(center, activeWorldEqDistProj, TRUE)
    # })
  } 
  else if (prj_details$prj == "oblique") {
    #Oblique azimuthal equidistant
    if (!"lat_center" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lat_center` element")
    }
    lat_center <- prj_details$lat_center
    if(!"lng_center" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lng_center` element")
    }
    lng_center <- prj_details$lng_center

#    outputTEXT <- paste0(outputTEXT, "<p class='outputText'>Distances are correct through or from the center - <span id='aeqd_str'>", 
#                         stringLinks("aeqd", NA, latC_eq, NA, NA, lngC_eq, NA), "</span></br></p>")
    
    prj_suggestions <- data.frame(
      prj = "aeqd", x0 = NA_real_, lat0 = latC_eq, lat1 = NA_real_, lat2 = NA_real_, lon0 = lngC_eq, k0 = NA_real_,
      description = "Oblique azimuthal equidistant",
      notes = paste0("Distance correct through or from the center (",lng_center,", ",lat_center,")")
    )
    
    # observeEvent(input$latC_eq, {
    #   latC_eq <- input$latC_eq
    #   updateTextInput(session, "latC_val", value = formatWorldLAT(latC_eq))
    #   updateTextInput(session, "aeqd_str", value = stringLinks("aeqd", NA, latC_eq, NA, NA, lngC_eq, NA))
    #   addWorldMapPreview(center, activeWorldEqDistProj, TRUE)
    # })
    # 
    # observeEvent(input$lngC_eq, {
    #   lngC_eq <- input$lngC_eq
    #   updateTextInput(session, "lngC_val", value = formatWorldLON(lngC_eq))
    #   updateTextInput(session, "aeqd_str", value = stringLinks("aeqd", NA, latC_eq, NA, NA, lngC_eq, NA))
    #   addWorldMapPreview(center, activeWorldEqDistProj, TRUE)
    # })
  } 
  else if (prj_details$prj == "two_points") {
    # Two-point azimuthal equidistant
    if (!"lat1" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lat1` element")
    }
    lat1_eq <- prj_details$lat1
    if (!"lng1" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lng1` element")
    }
    lng1_eq <- prj_details$lng1
    if (!"lat2" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lat2` element")
    }
    lat2_eq <- prj_details$lat2
    if (!"lng2" %in% names(prj_details)) {
      stop("`world_equidistant` must contain a `lng2` element")
    }
    lng2_eq <- prj_details$lng2
    
    prj_suggestions <- data.frame(
      prj = "tpeqd", x0 = NA_real_, lat0 = lat1_eq, lat1 = lng1_eq, lat2 = lat2_eq, lon0 = lng2_eq, k0 = NA_real_,
      description = "Two-point azimuthal equidistant",
      notes = paste0("Distances are correct from two points: ",lng1_eq,", ",lat1_eq," and ",lng2_eq,", ",lat2_eq)
    )
    
    
    # THE CODE BELOW IS BADLY TRANSLATED
    # lng1_eq <- worldValues(center$lng1, scale)
    # lat1_eq <- worldValues(center$lat1, scale)
    # lng2_eq <- worldValues(center$lng2, scale)
    # lat2_eq <- worldValues(center$lat2, scale)
    # 
    # outputTEXT <- paste0(outputTEXT, "<p class='outputText'>Distances are correct from two points: <span id='tp_aeqd_str'>", 
    #                      stringLinks("aeqd", NA, lat1_eq, NA, NA, lng1_eq, NA), " and ", 
    #                      stringLinks("aeqd", NA, lat2_eq, NA, NA, lng2_eq, NA), "</span></br></p>")
    # 
    # observeEvent(input$lat1_eq, {
    #   lat1_eq <- input$lat1_eq
    #   updateTextInput(session, "lat1_val", value = formatWorldLAT(lat1_eq))
    #   updateTextInput(session, "tp_aeqd_str", value = paste0(stringLinks("aeqd", NA, lat1_eq, NA, NA, lng1_eq, NA), " and ", 
    #                                                          stringLinks("aeqd", NA, lat2_eq, NA, NA, lng2_eq, NA)))
    #   addWorldMapPreview(center, activeWorldEqDistProj, TRUE)
    # })
    # 
    # observeEvent(input$lng1_eq, {
    #   lng1_eq <- input$lng1_eq
    #   updateTextInput(session, "lng1_val", value = formatWorldLON(lng1_eq))
    #   addWorldMapPreview(center, activeWorldEqDistProj, TRUE)
    # })
  } 
  else {
    stop("the `prj` element of world_equidistant` should be one of:\n",
    "'polar', 'oblique', 'two_points'")}

  return(prj_suggestions)
}



# Set default point values for equidistant world map projections
# # move it to where needed
# pole_eq <- -90
# lngP_eq <- -180
# latC_eq <- -39
# lngC_eq <- 145
# lat1_eq <- 34
# lng1_eq <- -117
# lat2_eq <- 46
# lng2_eq <- 16
