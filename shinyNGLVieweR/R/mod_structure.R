#' structure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_structure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menuItem("structure",
      icon = icon("sliders-h"),
      selectInput(ns("structureType"), "Type", list(
        "hide", "cartoon", "ball+stick", "ribbon", "backbone", "licorice", "spacefill", "line", "contact",
        "helixorient", "hyperball", "rocket"
      ), selected = "cartoon"),
      selectInput(ns("structureColorScheme"), "Scheme", c(
        "uniform", "residueindex", "element", "hydrophobicity",
        "bfactor", "sstruc", "random", "resname", "chainname", "entityindex",
        "entitytype", "modelindex", "occupancy"
      ), selected = "residueindex"),
      colourpicker::colourInput(ns("structureColor"), label = NULL, "blue", palette = "limited", closeOnClick = TRUE),
      bs_textInput(ns("structureSelection"), "Select", placeholder = "e.g. 20-30 OR <NG>", id_modal = "select_modal")
    )
  )
}
#' structure Server Function
#'
#' @noRd 
mod_structure_server <- function(input, output, session, globalSession, r) {
  ns <- session$ns

  Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)
  
  # Update inputs 
observeEvent(r$sidebarItemExpanded, {
  if (r$structure$loaded == FALSE && (r$sidebarItemExpanded == "structure")) { # reset in mod_NGLVieweROutput
    if (!is.null(r$fileInput$structure)) {
      data <- r$fileInput$structure
      if (data$visible == FALSE) {
        type <- "hide"
      } else {
        type <- data$type
      }
      updateSelectInput(session, "structureType", selected = type)
      updateSelectInput(session, "structureColorScheme", selected = data$colorScheme)
      colourpicker::updateColourInput(session, "structureColor", value = data$colorValue)
      updateTextInput(session, "structureSelection", value = data$selection)
    } else {
      if (isolate(r$fileInput$fileExt) == "pdb" || isolate(r$fileInput$fileExt) == "ngl" || is.null(isolate(r$fileInput$fileExt))) {
        representation <- "cartoon"
      } else {
        representation <- "ball+stick"
      }
      updateSelectInput(session, "structureType", selected = representation)
      updateSelectInput(session, "structureColorScheme", selected = "residueindex")
      colourpicker::updateColourInput(session, "structureColor", value = "#0000FF")
      updateTextInput(session, "structureSelection", value = "")
    }
    r$structure$loaded <- TRUE
  }
})

  observe({
    
    #Start observer when structure has loaded
    req(r$structure$loaded)
    
    input$structureType
    input$structureColorScheme
    input$structureColor
    
    Viewer_proxy %>% removeSelection("structure")

    Viewer_proxy %>% addSelection(input$structureType,
      param = list(
        name = "structure",
        sele = selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$structureSelection),
        colorScheme = input$structureColorScheme,
        colorValue = input$structureColor
      )
    )
    
    #df_structure
    if(input$structureSelection == ""){
      selection <- "*"
    } else {
      selection <- selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$structureSelection)
    }

  if(input$structureType == "hide"){
    visible <- FALSE
    type <- "cartoon"
  } else {
    visible <- TRUE
    type <- input$structureType
  }
    
  #Save structure settings
  r$structure$structure <- data.frame(
    type = type,
    colorScheme = input$structureColorScheme,
    colorValue = input$structureColor,
    selection = selection,
    visible = visible
  ) 
  })

  observeEvent(input$structureColorScheme, {
    if (input$structureColorScheme == "uniform" || input$structureColorScheme == "element") {
      shinyjs::show("structureColor")
    } else {
      shinyjs::hide("structureColor")
    }
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
