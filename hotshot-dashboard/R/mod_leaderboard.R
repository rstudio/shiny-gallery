#' leaderboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import reactable
#' @import dplyr
#' @import echarts4r
#' @import shinyWidgets
mod_leaderboard_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        uiOutput(ns("title"))
      )
    ),
    fluidRow(
      col_12(
        reactable::reactableOutput(ns("leaderboard_table"))
      )
    ),
    fluidRow(
      col_12(
        br(),
        br(),
        uiOutput(ns("player_card_placeholder"))
      )
    )
  )
}
    
#' leaderboard Server Function
#'
#' @noRd 
mod_leaderboard_server <- function(input, output, session, hotshot_stat_df){
  ns <- session$ns
  
  output$title <- renderUI({
    req(hotshot_stat_df())
    
    # derive number of races completed
    n_races <- hotshot_stat_df() %>%
      select(grand_prix, track, direction) %>%
      distinct() %>%
      nrow(.)
    
    tagList(
      h2(glue::glue("Leaderboard after {n_races} races!")),
      h4("Click the little arrows to see individual grand prix details for a player")
    )
  })
  
  df_position <- reactive({
    # generate position data
    req(hotshot_stat_df())
    
    df_position <- hotshot_stat_df() %>%
      gen_tidy_race_data() %>%
      gen_summary_gp_data()
    
    return(df_position)
  })
  
  player_stats_df <- reactive({
    req(df_position())
    df <- hotshot_stat_df() %>%
      gen_tidy_race_data() %>%
      gen_player_summary() %>%
      
      return(df)
  })
  
  player_car_stats_df <- reactive({
    req(df_position())
    df <- hotshot_stat_df() %>%
      gen_tidy_race_data() %>%
      gen_player_car_stats() %>%
      
      return(df)
  })
  
  player_driver_stats_df <- reactive({
    req(df_position())
    df <- hotshot_stat_df() %>%
      gen_tidy_race_data() %>%
      gen_player_driver_stats() %>%
      
      return(df)
  })
  
  df_overall <- reactive({
    # generate overall data
    req(df_position())
    req(player_stats_df())

    df_overall <- gen_summary_overall(df_position()) %>%
      arrange(desc(total_points)) %>%
      mutate(position = row_number()) %>%
      mutate(position_emo = case_when(
        position == 1 ~ "1st_place_medal",
        position == 2 ~ "2nd_place_medal",
        position == 3 ~ "3rd_place_medal",
        position == 8 ~ "8ball",
        TRUE ~ "checkered_flag"
      )) %>%
      left_join(player_data, by = "player_name") %>%
      left_join(country_data, by = "country") %>%
      left_join(
        select(player_stats_df(), player_name, avg_position, avg_margin_victory, avg_time_from_first)
      )
    
    return(df_overall)
  })
  
  output$leaderboard_table <- reactable::renderReactable({
    req(df_position())
    req(df_overall())
    
    create_leaderboard(df_position(), df_overall())
  })
  
  hotshot_row_selected <- reactive({
    req(df_overall())
    getReactableState("leaderboard_table", "selected")
  })
  
  player_selected <- reactive({
    req(hotshot_row_selected())
    req(df_overall())
    
    player_name <- df_overall() %>%
      slice(hotshot_row_selected()) %>%
      pull(player_name)
    
    return(player_name)
  })
  
  player_metadata <- reactive({
    req(player_selected())
    player_data_sub <- player_data %>%
      mutate(image_path = glue::glue("www/player_pics/{avatar}")) %>%
      filter(player_name == player_selected())
    
    return(player_data_sub)
  })
  
  output$player_card_placeholder <- renderUI({
    req(player_metadata())
    req(player_stats_df())
    req(player_car_stats_df())
    req(player_driver_stats_df())
    
    userBox(
      title = userDescription(
        title = pull(player_metadata(), full_name),
        subtitle = player_selected(),
        type = 1,
        image = pull(player_metadata(), image_path)
      ),
      status = "primary",
      width = 12,
      gradient = TRUE,
      background = "primary",
      boxToolSize = "xl",
      tagList(
        fluidRow(
          col_12(
            h4("Performance breakdown by car type")
          )
        ),
        fluidRow(
          col_6(
            echarts4r::echarts4rOutput(ns("car_finish_bars"))
          ),
          col_6(
            echarts4r::echarts4rOutput(ns("car_type_boxplots"))
          )
        ),
        fluidRow(
          col_12(
            h4("Performance breakdown by car driver"),
            echarts4r::echarts4rOutput(ns("driver_bars"))
          )
        ),

      ),
      footer = NULL
    )
  })
  
  output$car_finish_bars <- echarts4r::renderEcharts4r({
    req(hotshot_stat_df())
    req(player_selected())
    req(player_car_stats_df())
    
    plot_df <- filter(player_car_stats_df(), player_name == player_selected())
    
    my_chart <- plot_df %>%
      e_charts(type) %>%
      e_bar(avg_position) %>%
      e_axis_labels(y = "Avg Race Finish", x = "Car Type") %>%
      e_tooltip(
        trigger = "item", 
        formatter = e_tooltip_item_formatter(
          style = "decimal",
          digits = 3
        )
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
      e_legend(show = FALSE) %>%
      e_theme("dark")
      
    my_chart
  })
  
  output$car_type_boxplots <- echarts4r::renderEcharts4r({
    req(hotshot_stat_df())
    req(player_selected())
    
    # obtain player performance based on type of car
    df_cars <- gen_tidy_racers(hotshot_data) %>%
      select(driver = driver_name, car = car_name, type, driver_img_url, car_img_url)
    
    plot_df <- hotshot_stat_df() %>%
      gen_tidy_race_data() %>%
      left_join(df_cars, by = c("driver", "car")) %>%
      filter(player_name == player_selected())
    
    my_chart <- plot_df %>%
      group_by(type) %>%
      e_charts() %>%
      e_boxplot(player_margin_victory) %>%
      e_title(
        text = "Average Margin of Victory (seconds) by Car Type",
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
      e_tooltip(trigger = "item") %>%
      e_theme("dark")
    
    my_chart
  })
  
  output$driver_bars <- echarts4r::renderEcharts4r({
    req(hotshot_stat_df())
    req(player_selected())
    req(player_driver_stats_df())
    
    plot_df <- filter(player_driver_stats_df(), player_name == player_selected())
    
    my_chart <- plot_df %>%
      e_charts(driver) %>%
      e_bar(avg_position) %>%
      e_axis_labels(y = "Avg Race Finish", x = "Driver") %>%
      e_tooltip(
        trigger = "item", 
        formatter = e_tooltip_item_formatter(
          style = "decimal",
          digits = 3
        )
      ) %>% 
      e_x_axis(
        axisLabel = list(
          show = TRUE,
          textStyle = list(
            fontSize = 20
          )
        )
      ) %>%
      e_y_axis(
        axisLabel = list(
          show = TRUE,
          textStyle = list(
            fontSize = 20
          )
        )
      ) %>%
      e_legend(show = FALSE) %>%
      e_theme("dark")
    
    my_chart
  })
}
    
## To be copied in the UI
# mod_leaderboard_ui("leaderboard_ui_1")
    
## To be copied in the server
# callModule(mod_leaderboard_server, "leaderboard_ui_1")
 
