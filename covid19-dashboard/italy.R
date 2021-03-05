italyUI <- function(id){
  ns <- NS(id)
  tagList(
    material_row(
      material_card(
        title = "Italy",
        tagList(
          tags$p("The charts on this site were enabled by the",
                 tags$a(href="https://github.com/RamiKrispin/covid19Italy", "covid19italy"),
                 "package created by ", tags$a(href="https://twitter.com/Rami_Krispin", "Rami Krispin"))
        )
      )
    ),
    material_row(
      material_card(title = "Confirmed Cases:",
                    tagList(
                      p("The graph below shows the aggregated number of confirmed cases for Italy.")
                    )
      )
    ),
    material_row(
      tagList(
        plotlyOutput(ns("all_cases"))
      )
    ),
    material_row(
      material_card(title = "Active Cases:",
                    tagList(
                      p("Distribution of the active cases in Italy.")
                    )
      )
    ),
    material_row(
      plotlyOutput(ns("active"))
    ),
    material_row(
      material_card(title = "Cases by region:"
      )
    ),
    material_row(
      plotlyOutput(ns("region"))
    )
  )
}

italy <- function(input, output, session, italy_data) {
  colpal <- viridisLite::viridis(3)
  
  output$active <- renderPlotly({
    plot_ly(data = italy_data()$total,
            x = ~ date,
            y = ~home_confinement, 
            name = 'Home Confinement', 
            line = list(color = colpal[3]),
            type = 'scatter',
            mode = 'lines', 
            stackgroup = 'one') %>%
      add_trace( y = ~ hospitalized_with_symptoms, 
                 name = "Hospitalized with Symptoms",
                 line = list(color = colpal[2])) %>%
      add_trace(y = ~intensive_care, 
                name = 'Intensive Care', 
                line = list(color = colpal[1])) %>%
      layout(title = "Italy - Distribution of Active Covid19 Cases",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Number of Cases"),
             xaxis = list(title = "Source: Italy Department of Civil Protection"))
  })
  
  output$all_cases <- renderPlotly({
    plot_ly(data = italy_data()$total,
            x = ~ date,
            y = ~cumulative_positive_cases, 
            name = 'Active', 
            line = list(color = colpal[3]),
            type = 'scatter',
            mode = 'lines', 
            stackgroup = 'one') %>%
      add_trace( y = ~ death, 
                 name = "Death",
                 line = list(color = colpal[2])) %>%
      add_trace(y = ~recovered, 
                name = 'Recovered', 
                line = list(color = colpal[1])) %>%
      layout(title = "Italy - Distribution of Covid19 Cases",
             legend = list(x = 0.1, y = 0.9),
             yaxis = list(title = "Number of Cases"),
             xaxis = list(title = "Source: Italy Department of Civil Protection"))
  })
  
  output$region <- renderPlotly({
    outplot <- italy_data()$region %>% 
      mutate(date = as.Date(date)) %>%
      filter(date == max(date)) %>% 
      select(region_name, cumulative_positive_cases, recovered, death) %>%
      arrange(-cumulative_positive_cases) %>%
      mutate(region = factor(region_name, levels = region_name)) %>%
      plot_ly(y = ~ region, 
              x = ~ cumulative_positive_cases, 
              orientation = 'h',
              text =  ~ cumulative_positive_cases,
              textposition = 'auto',
              type = "bar", 
              name = "Active",
              marker = list(color = colpal[3])) %>%
      add_trace(x = ~ death, 
                text =  ~ death,
                textposition = 'auto',
                name = "Death",
                marker = list(color = colpal[2])) %>%
      add_trace(x = ~ recovered,
                text =  ~ recovered,
                textposition = 'auto',
                name = "Recovered",
                marker = list(color = colpal[1])) %>%
      layout(title = "Cases Distribution by Region",
             barmode = 'stack',
             yaxis = list(title = "Region"),
             xaxis = list(title = "Number of Cases"),
             hovermode = "compare",
             legend = list(x = 0.65, y = 0.9),
             margin =  list(
               l = 20,
               r = 10,
               b = 10,
               t = 30,
               pad = 2
             ))
    wait_hide(session)
    outplot
  })
}