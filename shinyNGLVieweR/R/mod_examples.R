#' examples UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_examples_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      tags$script(
        HTML("$(document).on('click', '.example_link', function () {
                        Shiny.onInputChange('example_link_id', this.id);
                    });")
      )
    ),
    menuItem("examples",
      icon = icon("atom"),
      fluidRow(
        style = "padding: 15px 0 0 0; margin: 0;",
        icon = icon("atom"),
        actionLink("6xcn", ">6xcn",
          class = "example_link btn-link",
        ),
        actionLink("2pne", ">2pne",
                   class = "example_link btn-link",
        ),
        actionLink("7ahl", ">7ahl",
                   class = "example_link btn-link",
        ),
        actionLink("6fp7", ">6fp7",
          class = "example_link btn-link",
        ),
        actionLink("6qzy", ">6qzy",
          class = "example_link btn-link",
        ),
        actionLink("7cid", ">7cid",
                   class = "example_link btn-link",
        )
      )
    )
  )
}
    
#' examples Server Function
#'
#' @noRd 
mod_examples_server <- function(input, output, session, r) {
  ns <- session$ns

  observeEvent(r$examples$example_link_id, {
    r$rendering <- TRUE
    r$fileInput <- readFile(app_sys(sprintf("app/www/%s.ngl", r$examples$example_link_id)))
    r$fileInput$name <- r$examples$example_link_id 
    r$stage$fileColor <- r$fileInput$stage$backgroundColor #Needed to change theme
  })
}
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
