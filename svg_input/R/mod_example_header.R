
mod_example_header_ui <- function(id, title = "") {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::HTML(paste0('<h2 class="hh2" style="color:#565656;text-align:center;margin-bottom:25px;"><b>&nbsp;GALLERY&nbsp;</b></h2>')),
    # shiny::h2(shiny::tags$b(" GALLERY "), class = "hh2", style = "color:#565656;text-align:center;margin-bottom:25px;"),
    shiny::fluidRow(
      style = "color:#666666;",
      shiny::column(width = 1, shiny::actionLink(ns("left"), NULL, icon = shiny::icon("chevron-left", "fa-2x"))),
      shiny::column(width = 10, shiny::h2(title, style = "margin-top:-4px;")),
      shiny::column(width = 1, shiny::actionLink(ns("right"), NULL, icon = shiny::icon("chevron-right", "fa-2x")))
    ),
    shiny::hr(style = "color:#666666")
  )
}

mod_example_header_server <- function(id, parent, prev_tab, next_tab) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$left,  { shiny::updateTabsetPanel(session = parent, inputId = "wizard", selected = prev_tab) })
    shiny::observeEvent(input$right, { shiny::updateTabsetPanel(session = parent, inputId = "wizard", selected = next_tab) })
    
  })
}

