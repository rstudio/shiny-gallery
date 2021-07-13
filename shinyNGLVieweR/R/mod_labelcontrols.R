#' labelcontrols UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_labelcontrols_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjqui::jqui_draggable(
      #hidden(
        div(
          id = ns("labelOptionsPlaceholder"), class='hideBox', style ="position: absolute; z-index:100; width: calc(30vw); top:calc(60vh);",
          box(
            width = 12, title = actionLink(ns("optionsTitle"), "Label options"), status = "primary", color = "black", solidHeader = TRUE, align = "left",
            id = ns("labelBox"), class = "controlBox", collapsible = TRUE, closable = TRUE,
            uiOutput(ns("labelcontrols"))
          )
        )
      )
    )
  #)
}
    
#' labelcontrols Server Function
#'
#' @noRd 
mod_labelcontrols_server <- function(input, output, session, globalSession, r) {
  ns <- session$ns

  # Unfold box on title click
  observeEvent(input$optionsTitle, {
    js$collapse(ns("labelBox"))
  })

  # Hide/Show box on Options click
  observeEvent(r$label$labelOptions, {
    updateBox("labelBox", action = "restore")
    shinyjs::toggleClass(class = "hideBox", id = "labelOptionsPlaceholder")
  })
  # Hide/Show box on close Icon
  observeEvent(input$labelBox, {
    if (isFALSE(input$labelBox$visible)) {
      shinyjs::toggleClass(class = "hideBox", id = "labelOptionsPlaceholder")
    }
  })

  # send inputs to label module
  observe({
    r$labelcontrols$textColor <- input$textColor
    r$labelcontrols$labelSize <- input$labelSize
    r$labelcontrols$fixedSize <- input$fixedSize
    r$labelcontrols$xOffset <- input$xOffset
    r$labelcontrols$yOffset <- input$yOffset
    r$labelcontrols$zOffset <- input$zOffset
    r$labelcontrols$labelBackground <- input$labelBackground
    r$labelcontrols$labelbackgroundColor <- input$labelbackgroundColor
    r$labelcontrols$labelbackgroundOpacity <- input$labelbackgroundOpacity
  })

  # update label options when component link is clicked
  observeEvent(r$label$labelLink_id, {
    uu_id <- str_replace(r$label$labelLink_id, "labelLink-", "")
    id <- sprintf("label-%s", uu_id)
    data <- r$label$labels[r$label$labels$id == id, ]

    if (data$fixedSize == "TRUE") {
      fixedSize <- "yes"
    } else {
      fixedSize <- "no"
    }
    
    if (data$labelBackground == "FALSE") {
      labelBackground <- "none"
    } else {
      labelBackground <- "color"
    }

    colourpicker::updateColourInput(session, "textColor", value = data$textColor)
    updateNumericInput(session, "labelSize", value = data$labelSize)
    updateSelectInput(session, "fixedSize", selected = fixedSize)
    updateNumericInput(session, "xOffset", value = data$xOffset)
    updateNumericInput(session, "yOffset", value = data$yOffset)
    updateNumericInput(session, "zOffset", value = data$zOffset)
    updateSelectInput(session, "labelBackground", selected = data$labelBackground)
    colourpicker::updateColourInput(session, "labelbackgroundColor", value = data$labelbackgroundColor)
    updateNumericInput(session, "labelbackgroundOpacity", value = data$labelbackgroundOpacity)
    
  })
  
  
  
  

  output$labelcontrols <- renderUI({
    div(
      fluidPage(
        style = "padding:0;",
        htmlTemplate(
          filename = app_sys("app/www/labelcontrols.html"),
          textColor = colourpicker::colourInput(ns("textColor"), label = "Text Color", "#FFFFFF", palette = "limited", closeOnClick = TRUE),
          labelSize = numericInput(ns("labelSize"), "Size", value = 3, step = 0.1),
          fixedSize = selectInput(ns("fixedSize"), "Fixed", c("yes", "no"), selected = "no"),
          xOffset = numericInput(ns("xOffset"), "xOffset", step = 0.1, value = 0),
          yOffset = numericInput(ns("yOffset"), "yOffset", step = 0.1, value = 0),
          zOffset = numericInput(ns("zOffset"), "zOffset", step = 0.1, value = 0),
          labelBackground = selectInput(ns("labelBackground"), "Background", c("none" = FALSE, "color" = TRUE)),
          labelbackgroundColor = shinyjs::disabled(colourpicker::colourInput(ns("labelbackgroundColor"), label = "Background color", palette = "limited", closeOnClick = TRUE, value = "#000000")),
          labelbackgroundOpacity = shinyjs::disabled(numericInput(ns("labelbackgroundOpacity"), "Opacity", step = 0.1, value = 1, min = 0, max = 1))
        )
      )
    )
  })

  observeEvent(input$labelBackground, {
    if (input$labelBackground) {
      shinyjs::enable("labelbackgroundColor")
      shinyjs::enable("labelbackgroundOpacity")
    } else {
      shinyjs::disable("labelbackgroundColor")
      shinyjs::disable("labelbackgroundOpacity")
    }
  })
}
## To be copied in the UI
#
    
## To be copied in the server
# 
 
