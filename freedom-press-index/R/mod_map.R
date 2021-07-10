#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_ui <- function(id){
  ns <- NS(id)
  pageContainer(
    class = "light",
    h2("Scores through time"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("value"),
      label = "Metric",
      choices = c("rank", "score"),
      checkIcon = list(
        yes = icon("ok",
        lib = "glyphicon")
      )
    ),
    echarts4r::echarts4rOutput(ns("map"), height = "50vh"),
    uiOutput(ns("desc"))
  )
}
    
#' map Server Function
#'
#' @noRd 
mod_map_server <- function(input, output, session){
  ns <- session$ns

  output$desc <- renderUI({
    msg <- paste0(tools::toTitleCase(input$value), ", the lower (the darker) the better")

    tags$i(msg)
  })

  output$map <- echarts4r::renderEcharts4r({
    fopi %>% 
      dplyr::mutate(
        year = gsub("^20", "", year),
        year = paste0("'", year)
      ) %>% 
      tidyr::drop_na(input$value) %>% 
      echarts4r::e_country_names(country, country, type = "country.name") %>% 
      dplyr::group_by(year) %>% 
      echarts4r::e_charts(country, timeline = TRUE) %>% 
      echarts4r::e_map_(input$value, name = "Index") %>% 
      echarts4r::e_visual_map_(
        input$value, 
        top = "middle",
        textStyle = list(color = "#fff"),
        outOfRange = list(
          color = "#2f2f2f"
        ),
        inRange = list(
          color = c("#247BA0", "#70C1B3", "#B2DBBF")
        )
      ) %>% 
      echarts4r::e_color(background = "rgba(0,0,0,0)") %>%  
      echarts4r::e_tooltip() %>% 
      echarts4r::e_timeline_opts(
        playInterval = 600, 
        currentIndex = 16,
        symbolSize = 4, 
        label = list(
          color = "#f9f7f1"
        ),
        checkpointStyle = list(
          color = "#f9f7f1"
        ),
        lineStyle = list(
          color = "#f9f7f1"
        ),
        controlStyle = list(
          color = "#f9f7f1",
          borderColor = "#f9f7f1"
        )
      )
  })
}
    
## To be copied in the UI
# mod_map_ui("map")
    
## To be copied in the server
# callModule(mod_map_server, "map")
 