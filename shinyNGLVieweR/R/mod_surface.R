#' surface UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_surface_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menuItem("surface",
      icon = icon("sliders-h"),
      selectInput(ns("surfaceType"), "Show", c(
        "hide", "uniform", "electrostatic", "hydrophobicity",
        "element", "bfactor", "residueindex", "random", "resname", "sstruc"
      )),
      colourpicker::colourInput(ns("surfaceColor"), label = NULL, "white", palette = "limited", closeOnClick = TRUE),
      bs_textInput(ns("surfaceSelection"), "Select", placeholder = "e.g. 20-30 OR <NG>", id_modal = "select_modal"),
      sliderInput(ns("surfaceOpacity"), "Opacity", min = 0, ticks = FALSE, max = 1, value = 1)
    )
  )
}
    
#' surface Server Function
#'
#' @noRd 
mod_surface_server <- function(input, output, session, globalSession, r){
  ns <- session$ns
  
Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)

# Update inputs 
observeEvent(r$sidebarItemExpanded, {
  if (r$surface$loaded == FALSE && (r$sidebarItemExpanded == "surface")) { # reset in mod_NGLVieweROutput
    if (!is.null(r$fileInput$surface)) {
      data <- r$fileInput$surface
      if (data$visible == FALSE) {
        type <- "hide"
      } else {
        type <- data$colorScheme
      }
      updateSelectInput(session, "surfaceType", selected = type)
      updateSelectInput(session, "surfaceColor", selected = data$colorScheme)
      colourpicker::updateColourInput(session, "surfaceColor", value = data$colorValue)
      updateTextInput(session, "surfaceSelection", value = data$selection)
      updateSliderInput(session, "surfaceOpacity", value = data$opacity)
    } 
    r$surface$loaded <- TRUE
  }
})





observe({
  
  #Start observer when surface has loaded
  req(r$surface$loaded)
  
  if (input$surfaceType == "hide") {
    visible <- FALSE
    colorScheme <- "uniform"
  } else {
    visible <- TRUE
    colorScheme <- input$surfaceType
  }

  Viewer_proxy %>% removeSelection("surface")
  
  Viewer_proxy %>% addSelection("surface",
    param = list(
      name = "surface",
      sele = selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$surfaceSelection),
      colorScheme = colorScheme,
      colorValue = input$surfaceColor,
      opacity = input$surfaceOpacity,
      visible = visible
    )
  )
  
  #df_structure
  if(input$surfaceSelection == ""){
    selection <- "*"
  } else {
    selection <- selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$surfaceSelection)
  }
  
  #Save structure settings
  r$surface$surface <- data.frame(
    colorScheme = colorScheme,
    colorValue = input$surfaceColor,
    selection = selection,
    opacity = input$surfaceOpacity,
    visible = visible
  ) 
})

observe({
  Viewer_proxy %>% updateRepresentation("surface", param = list(
    opacity = input$surfaceOpacity,
    colorValue = input$surfaceColor
  ))
})

observeEvent(input$surfaceType, {
  if (input$surfaceType == "uniform") {
    shinyjs::show("surfaceColor")
  } else {
    shinyjs::hide("surfaceColor")
  }
})
  
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
