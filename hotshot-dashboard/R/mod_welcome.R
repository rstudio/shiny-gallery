#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import countup
#' @import plotly
#' @import echarts4r
#' @import waiter
mod_welcome_ui <- function(id){
  ns <- NS(id)
  
  # create the HTML for loading screen
  gif <- "https://filedn.com/lXHQDOYF1yHVL1Tsc38wxx7/assets/hotshot_clip_10fps.gif"
  loading_gif <- tagList(
    h3("Starting up Hotshots Dashboard engine..."),
    img(src = gif, height = "520px")
  )
  
  tagList(
    waiter_show_on_load(html = loading_gif),
    waiter_hide_on_render(ns("mov")),
    fluidRow(
      col_12(
        jumbotron(
          title = "Hotshot Racing Dashboard!",
          lead = "Welcome to the Official Wimpy's World of Linux Gaming Most Official Unofficial Hotshot Racing League Spring Championship (WWOLGMOUHRLSC) Dashboard!  We've been working hard to track the results of each race and other geeky stats with a multitude of over-engineered apps and utilities. Would you expect anything less from a bunch of Linux and open-source enthusiasts?  I think not!",
          status = "primary",
          btnName = "Watch the replays on YouTube!",
          href = "https://www.youtube.com/channel/UC6D0aBP5pnWTGhQAvEmhUNw"
        )
      )
    ),
    fluidRow(
      col_12(
        box(
          title = "High-level Statistics!",
          solidHeader = FALSE,
          width = 12,
          background = "info",
          tagList(
            fluidRow(
              col_4(
                div(
                  align = "center",
                  h4("Average Margin of Victory"),
                  echarts4rOutput(ns("mov"))
                )
              ),
              col_4(
                div(
                  align = "center",
                  h4("Season Progress"),
                  echarts4rOutput(ns("n_laps"))
                )
              ),
              col_4(
                div(
                  align = "center",
                  h4("Avg Top 3 Separation"),
                  echarts4rOutput(ns("avg_top3"))
                )
              )
            ),
            fluidRow(
              col_4(
                div(
                  align = "center",
                  h4("Unique Winners"),
                  countup::odometerOutput(ns("n_first"), height = "100px"),
                )
              ),
              col_4(
                div(
                  align = "center",
                  h4("Did Not Finishers (DNF)"),
                  countup::odometerOutput(ns("n_dnf"), height = "100px")
                )
              ),
              col_4(
                div(
                  align = "center",
                  h4("Total Racing Hours"),
                  countup::odometerOutput(ns("total_time"), height = "100px")
                )
              )
            )
          )
        )
      )
    ),
    fluidRow(
      col_12(
        box(
          title = "Racing Progress in Stats!",
          solidHeader = FALSE,
          width = 12,
          background = "info",
          maximizable = TRUE,
          fluidRow(
            col_12(
              fluidRow(
                col_3(
                  selectInput(
                    ns("plot_stat"),
                    "Select Plot Statistic",
                    choices = c("Total Race Points" = "points_running", 
                                "Total Top 3 Finishes" = "top3_running",
                                "Total DNF Finishes" = "dnf_running"),
                    selected = "points_running",
                    multiple = FALSE
                  )
                ),
                col_3(
                  sliderInput(
                    ns("plot_interval"),
                    "Animation transition time (seconds)",
                    min = 0.1,
                    max = 1,
                    value = 0.5,
                    step = 0.1
                  )
                )
              ),
              fluidRow(
                col_12(
                  plotly::plotlyOutput(ns("points_plot"), height = "600px")
                )
              )
            )
          )
        )
      )
    )
  )
}
    
#' welcome Server Function
#'
#' @noRd 
mod_welcome_server <- function(input, output, session, hotshot_stat_df){
  ns <- session$ns
  
  # reactive for processed data
  df_ovstats <- reactive({
    req(hotshot_stat_df())
    res <- gen_tidy_race_data(hotshot_stat_df()) %>%
      gen_overall_summary()
    
    return(res)
  })
  
  output$n_first <- countup::renderOdometer({
    req(df_ovstats())
    countup::odometer(
      count = df_ovstats()$n_firstplace_winners,
      format = "d",
      theme = "car",
      width = "100%",
      height = "100%"
    )
  })
  
  output$n_dnf <- countup::renderOdometer({
    req(df_ovstats())
    countup::odometer(
      count = df_ovstats()$n_dnf_losers,
      format = "d",
      theme = "car",
      width = "100%",
      height = "100%"
    )
  })
  
  output$total_time <- countup::renderOdometer({
    req(df_ovstats())
    countup::odometer(
      count = df_ovstats()$total_time_hours,
      format = "dd.ddd",
      theme = "car",
      width = "100%",
      height = "100%"
    )
  })
  
  output$mov <- renderEcharts4r({
    req(df_ovstats())

    e_charts() %>%
      e_gauge(
        round(df_ovstats()$avg_margin_victory, 2), 
        name = "Seconds",
        min = 0,
        max = 2,
        axisTick = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        splitLine = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        pointer = list(
          itemStyle = list(
            color = "#fefd03"
          )
        ),
        axisLabel = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        title = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        detail = list(
          color = "#fff",
          fontSize = 30,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        animationDuration = 2000)
  })
  
  output$avg_top3 <- renderEcharts4r({
    req(df_ovstats())
    
    e_charts() %>%
      e_gauge(
        round(df_ovstats()$avg_top_3_sep, 2), 
        name = "Seconds",
        min = 0,
        max = 2,
        axisTick = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        splitLine = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        pointer = list(
          itemStyle = list(
            color = "#fefd03"
          )
        ),
        axisLabel = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        title = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        detail = list(
          color = "#fff",
          fontSize = 30,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        animationDuration = 2000)
  })
  
  output$n_laps <- renderEcharts4r({
    req(df_ovstats())
    
    e_charts() %>%
      e_gauge(
        round(df_ovstats()$n_laps_complete, 2), 
        name = "Laps",
        min = 0,
        max = 40,
        axisTick = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        splitLine = list(
          lineStyle = list(
            color = "#fff"
          )
        ),
        pointer = list(
          itemStyle = list(
            color = "#fefd03"
          )
        ),
        axisLabel = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        title = list(
          color = "#fff",
          fontSize = 20,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        detail = list(
          color = "#fff",
          fontSize = 30,
          fontFamily = "TTSupermolotNeue-Bold"
        ),
        animationDuration = 2000)
  })
  
  # reactive for plot data
  df_chart <- reactive({
    req(hotshot_stat_df())
    req(input$plot_stat)
    
    df2 <- gen_tidy_race_data(hotshot_stat_df())
    df_chart <- gen_plot_data(df2, all_drivers = TRUE)
    
    df_chart <- dplyr::mutate(df_chart, plot_var = .data[[input$plot_stat]])
    df_chart <- distinct(df_chart)
    
    # join additional player metadata
    df_chart <- left_join(
      df_chart,
      select(player_data, player_name, plot_emoji),
      by = "player_name"
    )
    
    df_chart <- mutate(df_chart, plot_emoji2 = purrr::map_chr(plot_emoji, ~emo::ji(.x)))
    return(df_chart)
  })
  
  n_players <- reactive({
    req(df_chart())
    length(unique(df_chart()$player_name))
  })
  
  xaxis_label <- reactive({
    req(input$plot_stat)
    
    if (input$plot_stat == "points_running") {
      xaxis_label <- "Total Points"
    } else if (input$plot_stat == "top3_running") {
      xaxis_label <- "Total Top 3 Finishes"
    } else if (input$plot_stat == "dnf_running") {
      xaxis_label <- "Total DNF"
    }
    
    return(xaxis_label)
  })
  
  plot_main_obj <- reactive({
    req(df_chart())
    req(n_players())
    req(xaxis_label())
    
    plot_ly(df_chart(), source = "running_plot") %>%
      add_text(
        x = ~plot_var,
        y = ~player_name,
        text = ~plot_emoji2,
        color = ~player_name,
        colors = RColorBrewer::brewer.pal(n_players(), "Set3"),
        frame = ~frame_label_fct,
        size = I(50)
      ) %>%
      layout(
        showlegend = FALSE,
        paper_bgcolor = '#5875D5',
        plot_bgcolor = '#5875D5',
        yaxis = list(title = "", color = '#ffffff', tickangle = 0, side = 'right',
                     gridwidth = 10),
        xaxis = list(title = xaxis_label(), color = '#ffffff'),
        font = list(
          color = '#ffffff',
          family = "TTSupermolotNeue-Bold",
          size = 16
        )
      )
  })
  
  plot_final_obj <- reactive({
    req(plot_main_obj())
    req(input$plot_interval)
    
    # obtain desired animation interval time (convert to ms)
    animation_interval <- 1000 * input$plot_interval

    new_plot <- plot_main_obj() %>%
      animation_opts(
        frame = animation_interval,
        easing = 'linear',
        redraw = FALSE
      ) %>%
      animation_button(
        x = 1.1, xanchor = 'right'
      ) %>%
      animation_slider(
        currentvalue = list(
          prefix = "Race Track: "
        ),
        font = list(color = '#5875D5'),
        step = list(visible = FALSE)
      )
    
    return(new_plot)
  })
  
  output$points_plot <- renderPlotly({
    req(plot_final_obj())
    plot_final_obj()
  })
  
}
    
## To be copied in the UI
# mod_welcome_ui("welcome_ui_1")
    
## To be copied in the server
# callModule(mod_welcome_server, "welcome_ui_1")
 
