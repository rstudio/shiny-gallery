dt_tableUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$script('$("#DataTables_Table_0").show()'),
    material_row(
      material_card(
        title = "Overview table",
        p("This table lists all countries that showed exponential growth (doubling of infections within 6.5 days or less)
              for at least one day. The columns show the following:"),
        material_button(input_id = ns("showExbutton"), "Show Explanation of columns"),
        uiOutput(ns("explanation"))
        
      )
    ),
    material_row(
      material_column(width=12,
                      div(style="",
                          
                      DT::dataTableOutput(ns("MaxDoublingTime"))
                          )
      )
    )
  )
}

dt_table <- function(input, output, session, all_data = NULL, population_data_short = NULL) {
  
  session$userData$showEx <- reactiveVal(TRUE)
  
  observeEvent(input$showExbutton, {
    value <- !session$userData$showEx()
    session$userData$showEx(value)
  })
  
  
  output$explanation <- renderUI({
    if (session$userData$showEx()) {
      tags$ul(
        tags$li(tags$b("Country:"), "name of the country"),
        tags$li(tags$b("Maximum time of exponential growth in a row:"), "The number of days a country showed exponential growth
                (doubling of infections in short time) in a row. This means there was no phase of slow growth or decrease in between."),
        tags$li(tags$b("Days to double infections:"), "This gives the time it took until today to double the number of infections. A
                higher number is better, because it takes longer to infect more people"),
        tags$li(tags$b("Exponential growth today:"), "Whether the countries number of infections is still exponentially growing"),
        tags$li(tags$b("Confirmed cases:"), "Confirmed cases today due to the Johns Hopkins CSSE data set"),
        tags$li(tags$b("Deaths:"), "Summed up deaths until today due to the Johns Hopkins CSSE data set"),
        tags$li(tags$b("Population:"), "Number of people living inside the country"),
        tags$li(tags$b("Confirmed cases on 100,000 inhabitants:"), "How many people have been infected if you would randomely choose
                100,000 people from this country."),
        tags$li(tags$b("mortality Rate:"), "Percentage of deaths per confirmed case")
        )
    } else {
      p()
    }
  })
  output$MaxDoublingTime <- DT::renderDataTable({
    
    df <- key_factors(all_data(), population_data_short)
    
    brks_clrs_doubling_days <- breaks_colors(df$doubling_days, reverse = TRUE)
    brks_clrs_max_exponential_time <- breaks_colors(df$max_exponential_time)
    dt_out <- datatable(df,
                        rownames= FALSE,
                        # extensions = c("FixedHeader"),
                        colnames = c(
                          "Country",
                          "Maximum time of exponential growth in a row (since Jan 1st)",
                          "Days to double infections (from today)",
                          "Exponential growth today?",
                          "Confirmed Cases (Johns Hopkins CSSE)",
                          "Deaths (Johns Hopkins CSSE)",
                          "Recovered",
                          "Population (in Mio)",
                          "Confirmed Cases on 100,000 inhabitants",
                          "mortality Rate (%)",
                          "Still active"
                        ),
                        options = list(
                          pageLength = 200,
                          scrollCollapse = TRUE,
                          scrollY = "400px",
                          scrollX = "300px"
                          # fixedHeader = TRUE
                        )
    ) %>%
      formatStyle("doubling_days",
                  backgroundColor = styleInterval(brks_clrs_doubling_days$brks, brks_clrs_doubling_days$clrs),
                  color = "white"
                  # color = styleInterval(brks_clrs_doubling_days$brks,
                  #                       c(
                  #                         rep("white", floor(1 * length(brks_clrs_doubling_days$clrs) / 3)),
                  #                         rep("black", ceiling(2 * length(brks_clrs_doubling_days$clrs) / 3))
                  #                       )
                  # )
      ) %>%
      formatStyle("still_exponential",
                  backgroundColor = styleEqual(c("yes", "no"), c("rgb(22, 176, 247)", "rgb(5,25,35)")),
                  color = styleEqual(c("no", "yes"), c("rgb(255,255,255)", "rgb(255,255,255)"))
      ) %>%
      formatStyle("max_exponential_time",
                  backgroundColor = styleInterval(brks_clrs_max_exponential_time$brks, brks_clrs_max_exponential_time$clrs),
                  color = "white"
                  # color = styleInterval(brks_clrs_max_exponential_time$brks,
                  #                       c(
                  #                         rep("black", floor(1 * length(brks_clrs_max_exponential_time$clrs) / 3)),
                  #                         rep("white", ceiling(2 * length(brks_clrs_max_exponential_time$clrs) / 3))
                  #                       )
                  # )
      )
    wait_hide(session)
    return(dt_out)
  })
}