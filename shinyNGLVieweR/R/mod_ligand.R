#' ligand UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ligand_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menuItem("ligand",
      icon = icon("sliders-h"),
      selectInput(ns("ligand"), "Ligand", c("hide", "ball+stick", "spacefill")),
      selectInput(ns("ligandColorScheme"), "Scheme", c("element", "uniform"), selected = "element"),
      colourpicker::colourInput(ns("ligandColor"), label = "Color", "#00FF00", palette = "limited", closeOnClick = TRUE),
      fluidRow(
        style = "padding: 0 15px; margin-top: -30px;",
        shinyWidgets::awesomeCheckboxGroup(inputId = ns("waterIon"), label = NULL, choices = c("Water", "Ion"), inline = TRUE, status = "primary")
      )
    )
  )
}
    
#' ligand Server Function
#'
#' @noRd 
mod_ligand_server <- function(input, output, session, globalSession, r) {
  ns <- session$ns
  Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)
  
  # Update inputs 
  observeEvent(r$sidebarItemExpanded, {
    if (r$ligand$loaded == FALSE && (r$sidebarItemExpanded == "ligand")) { # reset in mod_NGLVieweROutput
      if (!is.null(r$fileInput$ligand)) {
        data <- r$fileInput$ligand
        updateSelectInput(session, "ligand", selected = data$ligand)
        updateSelectInput(session, "ligandColorScheme", selected = data$colorScheme)
        colourpicker::updateColourInput(session, "ligandColor", value = data$colorValue)
        updateAwesomeCheckboxGroup(session, "waterIon", selected = unlist(str_split(data$waterIon, " "))) 
      } 
      r$ligand$loaded <- TRUE
    }
  })
  
  
  

  observe({
    
    #start observer when ligand has loaded 
    req(r$ligand$loaded)
    
    Viewer_proxy %>% removeSelection("ligand")

    if (input$ligand == "spacefill" || input$ligand == "ball+stick") {
      Viewer_proxy %>% addSelection(input$ligand,
        param = list(
          name = "ligand",
          sele = "( not polymer or not ( protein or nucleic ) ) and not ( water or ACE or NH2 or ion)",
          colorScheme = input$ligandColorScheme,
          colorValue = input$ligandColor
        )
      )
    }

    if (is.null(input$waterIon)) {
      waterIon <- ""
    } else {
      waterIon <- paste(input$waterIon, collapse = " ")
    }

    if (!is.null(waterIon)) {
      Viewer_proxy %>%
        removeSelection("water") %>%
        removeSelection("ion")

      if (grepl("Water", waterIon)) {
        Viewer_proxy %>% addSelection("ball+stick",
          param = list(
            name = "water",
            sele = "water"
          )
        )
      }

      if (grepl("Ion", waterIon)) {
        Viewer_proxy %>% addSelection("ball+stick",
          param = list(
            name = "ion",
            sele = "ion"
          )
        )
      }
    }

    # Write data.frame
    r$ligand$ligand <- data.frame(
      ligand = input$ligand,
      colorScheme = input$ligandColorScheme,
      colorValue = input$ligandColor,
      waterIon = waterIon
    )
  })

  observeEvent(input$ligand, {
    if (input$ligand != "hide") {
      shinyjs::show("ligandColorScheme")
      shinyjs::show("ligandColor")
    } else {
      shinyjs::hide("ligandColorScheme")
      shinyjs::hide("ligandColor")
    }
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
