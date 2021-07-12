#' trackstats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import echarts4r
#' @import dplyr
#' @import reactable
mod_trackstats_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        div(
          align = "center",
          h1("Grand Prix Statistics!")
        )
      )
    ),
    fluidRow(
      col_12(
        h4("Each grand prix in our Hotshot Racing League consists of 4 challenging tracks each coming from a unique region. Here's a breakdown of the tracks used in each Grand Prix raced as of today:")
      )
    ),
    fluidRow(
      col_12(
        reactable::reactableOutput(ns("track_table"))
      )
    ),
    fluidRow(
      col_12(
        br(),
        br()
      )
    ),
    fluidRow(
      col_12(
        box(
          title = "Margin of Victory",
          solidHeader = FALSE,
          width = 12,
          background = "info",
          maximizable = TRUE,
          tagList(
            fluidRow(
              col_12(
                h4("Let's explore the distribution of each race's margin of victory by each grand prix as well as each region.  Need a refresher on how to interpret a box plot?  Read on!"),
                tags$li("In a box plot, the lower and upper edges of the box indicate the central 50% of the data."),
                tags$li("The middle line inside tbe box denotes the median of the data"),
                tags$li("Lines extending from the box boundaries depict the additonal range of the data, specifically the interquartile range. Dots that appear outside this range are considered outliers.")
              )
            ),
            fluidRow(
              col_6(
                echarts4r::echarts4rOutput(ns("mov_by_gp"))
              ),
              col_6(
                echarts4r::echarts4rOutput(ns("mov_by_region"))
              )
            )
          )
        )
      )
    )
  )
}
    
#' trackstats Server Function
#'
#' @noRd 
mod_trackstats_server <- function(input, output, session, hotshot_stat_df){
  ns <- session$ns
  
  output$track_table <- reactable::renderReactable({
    req(hotshot_stat_df())
    create_track_table(hotshot_stat_df())
  })
  
  output$mov_by_gp <- echarts4r::renderEcharts4r({
    req(hotshot_stat_df())
    
    plot_df <- hotshot_stat_df() %>%
      gen_tidy_race_data() %>%
      filter(position == 1) %>%
      select(grand_prix_fct, track_fct, direction_fct, player_name, margin_victory) %>%
      distinct()
    
    my_chart <- plot_df %>%
      group_by(grand_prix_fct) %>%
      e_charts() %>%
      e_boxplot(margin_victory) %>%
      e_tooltip(trigger = "item") %>%
      e_title(
        text = "Average Margin of Victory (seconds) by Grand Prix",
        subtext = "Pooled over both normal and mirrod versions",
        left = "center"
      ) %>%
      e_x_axis(
        axisLabel = list(
          show = TRUE,
          textStyle = list(
            fontSize = 14
          )
        )
      ) %>%
      e_y_axis(
        axisLabel = list(
          show = TRUE,
          textStyle = list(
            fontSize = 14
          )
        )
      ) %>%
      e_theme("dark")
    
    my_chart
  })
  
  output$mov_by_region <- echarts4r::renderEcharts4r({
    req(hotshot_stat_df())
    
    region_df <- tibble::enframe(hotshot_data$tracks) %>%
      tidyr::unnest(cols = value) %>%
      rename(region = name, track_fct = value)
    
    plot_df <- hotshot_stat_df() %>%
      gen_tidy_race_data() %>%
      filter(position == 1) %>%
      select(grand_prix_fct, track_fct, direction_fct, player_name, margin_victory) %>%
      distinct() %>%
      left_join(region_df, by = "track_fct")
    
    my_chart <- plot_df %>%
      group_by(region) %>%
      e_charts() %>%
      e_boxplot(margin_victory) %>%
      e_tooltip(trigger = "item") %>%
      e_title(
        text = "Average Margin of Victory (seconds) by Region",
        subtext = "Pooled over both normal and mirrod versions",
        left = "center"
      ) %>%
      e_x_axis(
        axisLabel = list(
          show = TRUE,
          textStyle = list(
            fontSize = 14
          )
        )
      ) %>%
      e_y_axis(
        axisLabel = list(
          show = TRUE,
          textStyle = list(
            fontSize = 14
          )
        )
      ) %>%
      e_theme("dark")
    
    my_chart
  })
  
}
    
## To be copied in the UI
# mod_trackstats_ui("trackstats_ui_1")
    
## To be copied in the server
# callModule(mod_trackstats_server, "trackstats_ui_1")
 
