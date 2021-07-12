#' refresh UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets
#' @import pins
mod_refresh_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::actionBttn(
      "refresh",
      "Refresh Data",
      style = "unite",
      icon = icon("download"),
      size = "sm"
    )
  )
}
    
#' refresh Server Function
#'
#' @noRd 
mod_refresh_server <- function(input, output, session){
  ns <- session$ns
  
  current_data <- reactiveVal(NULL)
 
}
    
## To be copied in the UI
# mod_refresh_ui("refresh_ui_1")
    
## To be copied in the server
# callModule(mod_refresh_server, "refresh_ui_1")
 
