

# Module UI function
mod_create_svg_input_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    bs4Dash::accordion(
      id = ns("accordion"),
      bs4Dash::accordionItem(
        title = "1 Upload", status = "olive", collapsed = FALSE, 
        shiny::div(
          style = "max-width:1200px; width:100%; margin:0 auto;",
          mod_upload_svg_ui(ns("up")),
          shiny::fluidRow(shiny::column(width = 12, shiny::actionButton(ns("btn_1"), "Next")))
        )
      ),
      bs4Dash::accordionItem(
        title = "2 Set Inputs", status = "olive",
        shiny::div(
          style = "max-width:1200px; width:100%; margin:0 auto;",
          mod_set_inputs_ui(ns("si")),
          shiny::fluidRow(shiny::column(width = 12, shiny::actionButton(ns("btn_2"), "Next")))
        )
      ),
      bs4Dash::accordionItem(
        title = "3 Download", status = "olive",
        shiny::div(
          style = "max-width:1200px; width:100%; margin:0 auto;",
          mod_download_files_ui(ns("df"))
        )
      )
    ),
    if (Sys.getenv("BROWSE") == "TRUE") { shiny::actionButton(ns("browse"), "Browse")}
  )
}

# Module server function
mod_create_svg_input_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
      
      shiny::observeEvent(input$browse, { browser() })
      
      shiny::observeEvent(input$btn_1, { bs4Dash::updateAccordion(id = "accordion", selected = 2, session = session) })
      shiny::observeEvent(input$btn_2, { bs4Dash::updateAccordion(id = "accordion", selected = 3, session = session) })
      
      svg_base   <- mod_upload_svg_server("up")
      svg_inputs <- mod_set_inputs_server("si", svg_base)
      mod_download_files_server("df", svg_inputs)
      
      
  })    
}