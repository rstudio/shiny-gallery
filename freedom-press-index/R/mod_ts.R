# Module UI
  
#' @title   mod_ts_ui and mod_ts_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ts
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_ts_ui <- function(id){
  ns <- NS(id)
  pageContainer(
    h2("Countries through time"),
    br(),
    fluidRow(
      column(
        6,
        uiOutput(ns("country_select_generated"))
      ),
      column(
        6,
        shinyWidgets::radioGroupButtons(
          inputId = ns("value"),
          label = "Metric",
          choices = c("rank", "score"),
          checkIcon = list(
            yes = icon("ok",
            lib = "glyphicon")
          )
        )
      )
    ),
    echarts4r::echarts4rOutput(ns("trend"), height="50vh")
  )
}
    
# Module Server
    
#' @rdname mod_ts
#' @export
#' @keywords internal
    
mod_ts_server <- function(input, output, session){
  ns <- session$ns

  output$country_select_generated <- renderUI({
    cns <- fopi %>% 
      dplyr::arrange(country) %>% 
      dplyr::distinct(country) %>% 
      dplyr::pull(country)

    selectizeInput(
      ns("country_select"),
      "Search a country",
      choices = cns,
      selected = sample(cns, 2),
      multiple = TRUE
    )
  })

  output$trend <- echarts4r::renderEcharts4r({
    req(input$country_select)

    msg <- paste0(tools::toTitleCase(input$value), ", the lower the better")

    fopi %>% 
      dplyr::mutate(year = as.character(year)) %>% 
      dplyr::arrange(year) %>% 
      dplyr::filter(country %in% input$country_select) %>% 
      dplyr::group_by(country) %>% 
      echarts4r::e_charts(year) %>% 
      echarts4r::e_line_(input$value) %>% 
      echarts4r::e_tooltip(trigger = "axis") %>% 
      echarts4r::e_y_axis(inverse = TRUE) %>% 
      echarts4r::e_axis_labels("Years") %>% 
      echarts4r::e_title(msg) %>% 
      echarts4r::e_color(
        c("#247BA0", "#FF1654", "#70C1B3", "#2f2f2f", "#F3FFBD", "#B2DBBF")
      )
  })
}
