#' Function to create the world equidistant projection
#' 
#' @param center The center of the map (named vector with lat and long)
#' @param scale The scale of the map
#' @param round_cm The round central meridian.
#' @param projection The projection to use 
#' projection suggestions
#' @return The projection object


## TODO this is messy, as each projection requires additional parameters.
# Not sure how to best implement. Probably crs_wizard should give a message
# that explains the syntax for this special function, which then would allow
# to choose one of the three projections, with the appropriate parameters for
# the chosen projection.

crs_world_equidistant <- function(center,
                                  scale,
                                  round_cm = FALSE,
                                  projection) {
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
  if (activeWorldEqDistProj == "Polar azimuthal equidistant") {
    lngP_eq <- worldValues(center$lng, scale)
    pole_str <- ifelse(pole_eq > 0, "North Pole", "South Pole")
    
    outputTEXT <- paste0(outputTEXT, "<p class='outputText'>Distances are correct through or from the <span id='pole_str'>", pole_str, " - ", 
                         stringLinks("aeqd", NaN, pole_eq, NaN, NaN, lngP_eq, NaN), "</span><br></p>")
    
    outputTEXT <- paste0(outputTEXT, "<p class='outputText'>Center latitude: <span id='pole_val'>", formatWorldLAT(pole_eq), "</span></p>")
    
    outputTEXT <- paste0(outputTEXT, "<div class='sliderBox'><form id='pole_eq'>",
                         "<input type='radio' name='pole_eq' id='North Pole' value='90'><label for='North Pole' style='font-size:11px;'>North Pole</label>",
                         "<input type='radio' name='pole_eq' id='South Pole' value='-90'><label for='South Pole' style='font-size:11px;'>South Pole</label>",
                         "</form></div>")
    
    # Handle user selection change
    observeEvent(input$pole_eq, {
      pole_eq <<- input$pole_eq
      pole_str <<- ifelse(pole_eq > 0, "North Pole", "South Pole")
      outputTEXT <<- paste0(outputTEXT, "<p class='outputText'>Central meridian: <span id='lngP_val'>", formatWorldLON(lngP_eq), "</span></p>")
      
      addWorldMapPreview(center, activeWorldEqDistProj, TRUE)
    })
    
  } else if (activeWorldEqDistProj == "Oblique azimuthal equidistant") {
    lngC_eq <- worldValues(center$lng, scale)
    latC_eq <- worldValues(center$lat, scale)
    
    outputTEXT <- paste0(outputTEXT, "<p class='outputText'>Distances are correct through or from the center - <span id='aeqd_str'>", 
                         stringLinks("aeqd", NaN, latC_eq, NaN, NaN, lngC_eq, NaN), "</span></br></p>")
    
    # Center latitude slider
    observeEvent(input$latC_eq, {
      latC_eq <<- input$latC_eq
      addWorldMapPreview(center, activeWorldEqDistProj, TRUE)
    })
    
    # Center longitude slider
    observeEvent(input$lngC_eq, {
      lngC_eq <<- input$lngC_eq
      addWorldMapPreview(center, activeWorldEqDistProj, TRUE)
    })
  } else {
    outputTEXT <- paste0(outputTEXT, "<p></p><p></p><p>Equidistant world map projection not available</p><p></p><p></p>")
  }
  
  return(outputTEXT)
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
