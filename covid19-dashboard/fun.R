funUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    material_row(
      material_card(
        title = "Toilet Paper",
        tagList(
          div(style="text-align:center;margin:auto;",
            tags$img(src="./img/toilet_paper.jpg", style="max-width:100%")
          ),
          tags$p("Did you ever wonder why you have to fight for toilet paper in your country and others find it weird? Maybe
                 you basically live in a Skandinavian country, Switzerland or the US."),
          tags$p("This is where people seem to use this stuff the most. Please stop hording, there will be enough for everybody:"),
          tags$p("Data is taken from ", tags$a(href="https://www.statista.com/outlook/80010000/109/toilet-paper/united-states", "statista"))
          )
      )
    ),
    material_row(
      plotlyOutput(ns("toilet_paper"), height = "600px")
    ),
    material_row(
      material_card(
        title = "Pasta",
        tagList(
          div(style="text-align:center;margin:auto;",
              tags$img(src="./img/pasta.jpg", style="max-width:100%")
          ),
          tags$p("Corona and Pasta seem to be highly correlated. Look at who is leader in world-wide pasta consumption:"),
          tags$p("Data is taken from ", tags$a(href="https://www.statista.com/outlook/40060100/100/pasta-noodles/worldwide", "statista"))
        )
      )
    ),
    material_row(
      plotlyOutput(ns("pasta"), height = "600px")
    )
  )
}

fun <- function(input, output, session, fun_data) {
  colpal <- viridisLite::viridis(3)
  
  output$toilet_paper <- renderPlotly({
    
    fun_data()$toilet_paper %>% 
    dplyr::arrange(per.capita) %>%
      mutate(per.capita = round(per.capita/0.55*4)) %>%
      mutate(X = factor(X, levels = X)) %>%
    plot_ly(y = ~ X,  
            x = ~ per.capita, 
            legendgroup = ~X, 
            orientation = 'h',
            type = "bar", 
            marker = list(color = colpal[2])) %>%
    layout(title = "Number of rolls per household / year",
           yaxis = list(title = "Country", tickvals = 1:nrow(fun_data()$toilet_paper), tickmode = "array"),
           xaxis = list(title = "Usage of Rolls per Household")
          )
  })
  output$pasta <- renderPlotly({
    
    fun_data()$pasta %>% 
    dplyr::arrange(per.capita) %>%
      mutate(per.capita = per.capita/0.55*4) %>%
      mutate(X = factor(X, levels = X)) %>%
    plot_ly(y = ~ X,  
            x = ~ per.capita, 
            orientation = 'h',
            type = "bar", 
            marker = list(color = colpal[2])) %>%
    layout(title = "Spending on Pasta / Year",
           yaxis = list(title = "Country", tickvals = 1:nrow(fun_data()$pasta), tickmode = "array"),
           xaxis = list(title = "USD")
          )
  })
}