running_chartsUI <- function(id) {
  
  ns <- NS(id)
  tagList(
    material_row(
      material_column(
        width = 12,
        uiOutput(ns("selector"))
      )
    ),
    material_row(
      material_card(title = "Running day:",
                    tagList(
                      p("After having 200 patients identified with COVID-19 a country starts to appear in these charts.
                        by this information you can compare how fast a country delt with the appearing pressure on hospitals
                        and started to fight against the virus")
                    )
      )
    ),
    # ----- active_per_inhabitant ----
    material_row(
      material_card(title = "Active/100.000 inhabitants per running day:",
                    tagList(
                      p("Patients identified with the disease that did not die or recover and still have it normalized on 100.000 inhabitants.")
                    )
      )
    ),
    material_row(
      material_column(
        width = 12,
        plotlyOutput(outputId = ns("active_per_inhabitant"), width = "100%")
      )
    ),
    # ----- confirmed ----
    material_row(
      material_card(title = "Confirmed per running day:",
                    tagList(
                      p("Patients identified with the disease.")
                    )
      )
    ),
    material_row(
      material_column(
        width = 12,
        plotlyOutput(outputId = ns("confirmed"), width = "100%")
      )
    ),
    # ----- active ----
    material_row(
      material_card(title = "Active per running day:",
                    tagList(
                      p("Patients identified with the disease that did not die or recover and still have it.")
                    )
      )
    ),
    material_row(
      material_column(
        width = 12,
        plotlyOutput(outputId = ns("active"), width = "100%")
      )
    ),
    material_row(
      material_card(title = "New cases:",
                    tagList(
                      p("These are the new cases per country popping up on the running day.")
                    )
      )
    ),
    material_row(
      material_column(
        width = 12,
        plotlyOutput(outputId = ns("new_cases"), width = "100%")
      )
    ),
    # ----- Barchart Doubling ----
    material_row(
      material_card(title = "Doubling days:",
                    tagList(
                      p("For each day the number of days it would take to double the number of confirmed cases is shown."),
                      p("This means, the higher the number, the slower people get infected."),
                      p("The doubling time is calculated by estimating exponential growth over at least the last three days.")
                    )
      )
    ),
    material_row(
      material_column(
        width = 12,
        plotlyOutput(outputId = ns("DoublingDays"), width = "100%")
      )
    ),
    # ----- Mortality ----
    material_row(
      material_card(title = "Mortality:",
                    tagList(
                      p("This shows the percentage of lethal cases per country over time."),
                      p(tags$em("Hovering shows total death under the braces."))
                    )
      )
    ),
    material_row(
      material_column(
        width = 12,
        plotlyOutput(outputId = ns("mortality"), width = "100%")
      )
    )
  )
}

running_charts <- function(input, output, session, all_data = all_data, map_data = map_data, population_data = NULL) {
  
  default_countries <- c("New York City", "Korea, South", "Italy", "China (only Hubei)", "US")
  
 
  #----- Timeline Data -----
  
  plot_data_intern <- reactive({
    if (!is.null(input$countries)) {
      selected_countries <-  input$countries
    } else {
      selected_countries <-  default_countries
    }
    
    return(
      all_data() %>%
        filter(country %in% selected_countries)
    )
  })
  output$selector = renderUI({
    tagList(
      selectInput(inputId = session$ns('countries'),
                  'Select Countries you want to add:', sort(unique(all_data()$country)),
                  selected = default_countries, multiple = TRUE),
      div(style="clear:both;height:20px;")
    )
  })
  
  # ---- data running ----
  running_day_data <- reactive({
    
    plotting_data <- plot_data_intern()  %>%
      mutate(
        date_greater_200 = case_when(
          as.numeric(value) > 200 ~ 1,
          TRUE ~ 0
        )
      ) %>%
      filter(date_greater_200 == 1) %>%
      group_by(country) %>%
      mutate(running_day = row_number()) %>%
      ungroup()
    population_data <- population_data %>% rename(country = Country)
    left_join(plotting_data, population_data, by = "country") %>% 
      mutate(active_per_inhabitant = active / Year_2016 * 100000)
  })
  
  
  # ---- Barchart Doubling ----
  output$DoublingDays <- renderPlotly({
    
    plot_data_intern2 <- running_day_data()
    # generate bins based on input$bins from ui.R
    
    plot_ly(
      data = plot_data_intern2,
      hoverinfo = "",
      type = "scatter",
      transforms = list(
        list(
          type = 'groupby',
          groups = plot_data_intern2$country,
          styles = lapply(seq_along(unique(plot_data_intern2$country)), function(x){
            palette_col <- viridisLite::viridis(n = length(unique(plot_data_intern2$country)))
            list(target = unique(plot_data_intern2$country)[x], value = list(line = list(color = palette_col[x]), 
                                                                             marker = list(color = alpha(palette_col[x], 0.6))))
          })
        )
      )
    ) %>%
      add_trace(x = ~running_day,
                y = ~doubling_days,
                type = 'bar',
                name = 'Days to double') %>%
      layout(
        xaxis = list(
          title = "Running Day"
        ),
        yaxis = list(
          type = "log",
          title = "Doubling Days",
          range = c(0, max(as.numeric(running_day_data()$doubling_days), na.rm = T) + 0.5)
        )
      )
    
  })
  
  running_plot <- function(running_day_data, value, label = "Active (Total cases)", type = "lines"){
    plot_data_intern2 <- running_day_data()
    data_for_country <- spread(plot_data_intern2 %>%
                                 select(one_of("country", "running_day", value)),
                               key = "country",
                               value = value)
    palette_col <- viridisLite::viridis(n = length(unique(plot_data_intern2$country)))
    plot_first <- if (type == "lines") {
      plot_ly(
        hoverinfo = "",
        type = "scatter",
        mode = "lines"
      ) 
    } else {
      plot_ly(
        hoverinfo = ""
      )
    }
    for (country_name in unique(plot_data_intern2$country)) {
      simple_data <- data_for_country[, c("running_day", country_name)]
      
      simple_data <- simple_data[!is.na(simple_data[, country_name]), ]
      
      names(simple_data)[which(names(simple_data) == country_name)] <- "active"
      
      plot_first <- plot_first %>% add_trace(
        data = simple_data,
        x = ~as.numeric(running_day),
        y = ~as.numeric(active),
        name = country_name,
        text="",
        type = if(type == "lines") NULL else type,
        line = list(color = palette_col[which(unique(plot_data_intern2$country) == country_name)]),
        marker = if(type != "lines") {
          list(color = palette_col[which(unique(plot_data_intern2$country) == country_name)])
        } else {
            list()
          }
      )
      
    }
    
    plot_first %>%
      layout(
        xaxis = list(
          title = "Running Day"
        ),
        yaxis = list(
          title = label,
          range = c(0, max(as.numeric(plot_data_intern2[[value]]), na.rm=TRUE) + 0.3)
        )
      )
  }
  
  # ---- active_per_inhabitant ----
  output$active_per_inhabitant <- renderPlotly({
    out_plot <- running_plot(running_day_data = running_day_data, value = "active_per_inhabitant")
    out_plot
  })
  # ---- confirmed ----
  output$confirmed <- renderPlotly({
    out_plot <- running_plot(running_day_data = running_day_data, value = "value")
    out_plot
  })
  # ---- active ----
  output$active <- renderPlotly({
    out_plot <- running_plot(running_day_data = running_day_data, value = "active")
    out_plot
  })
  # ---- new_cases ----
  output$new_cases <- renderPlotly({
    out_plot <- running_plot(running_day_data = running_day_data, value = "change", type = "bar")
    out_plot
  })
  # ---- mortality ----
  output$mortality <- renderPlotly({
    out_plot <- running_plot(running_day_data = running_day_data, value = "mortality", label = "Mortality in %")
    wait_hide(session)
    out_plot
  })
}