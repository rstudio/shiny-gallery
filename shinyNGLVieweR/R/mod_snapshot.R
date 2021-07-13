#' snapshot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_snapshot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menuItem("snapshot",
      icon = icon("image"),
      textInput(ns("snapshotName"), label = NULL, placeholder = "Filename"),
      materialSwitch(inputId = ns("antialias"), label = "antialias", right = TRUE, value = TRUE, status = "primary"),
      materialSwitch(inputId = ns("trim"), label = "trim", right = TRUE, value = TRUE, status = "primary"),
      materialSwitch(inputId = ns("transparent"), label = "transparent", right = TRUE, value = TRUE, status = "primary"),
      fluidRow(
        style = "padding: 15px 0 15px 0; margin: 0;",
        actionButton(ns("snapshot"), "Snapshot")
      )
    )
  )
}
    
#' snapshot Server Function
#'
#' @noRd 
mod_snapshot_server <- function(input, output, session, globalSession){
  ns <- session$ns
  
Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)

observeEvent(input$snapshot, {
  if (nchar(input$snapshotName) != 0) {
    fileName <- input$snapshotName
  } else {
    fileName <- "structure_snapshot"
  }
  Viewer_proxy %>% snapShot(
    fileName = fileName,
    param = list(
      factor = 1,
      antialias = isolate(input$antialias),
      trim = isolate(input$trim),
      transparent = isolate(input$transparent)
    )
  )
})

}
    
## To be copied in the UI
# mod_snapshot_ui("snapshot_ui_1")
    
## To be copied in the server
# callModule(mod_snapshot_server, "snapshot_ui_1")
 
