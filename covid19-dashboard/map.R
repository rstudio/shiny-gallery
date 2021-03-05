mapUI <- function(id){
  ns <- NS(id)
  tagList(
    material_row(
      material_card(
        title = "Overview map - Active cases",
        tags$p("Please note: On 2020-03-23 the level of details changed for the USA. The explosion in cases you will see is due to changes
               in geographical details in ", tags$a(href = "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data", "CSSE Date", target="_new"), " not to a real explosion of cases."),
        
        uiOutput(ns("slider"))
      )
    ),
    material_row(
      material_column(width=12,
                        leafletOutput(outputId = ns("outmap"), height = 540)
      )
    )
  )
}

map <- function(input, output, session, all_dates = NULL, map_data = NULL) {
  
  session$userData$showEx <- reactiveVal(TRUE)
  
  output$slider <- shiny::renderUI({
    
    div(style="width:85%;padding-left:7%;padding-right:7%",
      shiny::sliderInput(inputId = session$ns("datum"),
                         min = as.POSIXct("2020-02-01"),
                         max = max(all_dates()),
                         value = max(all_dates()),
                         step = 86400,
                         label = "Date", timeFormat="%Y-%m-%d", width = "100%",
                         animate = animationOptions(interval = 200))
    )
  })
  
  curr_date <- reactiveVal( as.character(Sys.Date() + 1))
  
  full_data <- reactive({
    get_dat <- map_data()
    
    return(get_dat)
    })
  
  output$outmap <- renderLeaflet({
    
    mapout <- leaflet_static <- leaflet() %>% addProviderTiles(providers$ CartoDB.Positron) %>%
      setView(0,0, 2)
    
    return(mapout)
  })
  
  observeEvent(input$datum, {
     
     date_to_choose <- if (is.null(input$datum)) {
       as.character(Sys.Date() - 1)
     } else {
       as.character(input$datum)
     }
     
     only_numeric <- sort(as.numeric(unique(full_data()$active)))
     
     col_pal <- colorNumeric(
       rev(viridisLite::magma(99)[1:78]),
       domain = c(min(only_numeric, na.rm = TRUE), max(only_numeric, na.rm = T))
     )
     
     max_val <- max(only_numeric, na.rm = T)
     
     data_for_display <- full_data() %>%
       filter(date == as.character(date_to_choose)) %>%
       select(Lat, Long, active, date, Combined_Key) %>%
       filter(active > 0) %>%
       filter(!is.na(Long) & !is.na(Lat)) %>%
       mutate(active_scaled = case_when(
         grepl(pattern = "\\,\\s{0,1}US", x = Combined_Key) &
           as.Date(date_to_choose, origin = "1970-01-01") > as.Date("2020-03-21", origin = "1970-01-01") ~ scales::rescale(
             x = active, from = c(0, max_val), to = c(12000, 650000)
           ),
         TRUE ~ scales::rescale(x = active, from = c(0, max_val), to = c(60000, 450000))
       ),
       text = paste0(as.character(Combined_Key), "\n", active),
       color = col_pal(active)
       ) %>%
       arrange(active)
     material_spinner_hide(session, session$ns("wait"))
     leafletProxy(mapId = "outmap") %>%
       clearGroup(curr_date()) %>%
       addCircles(data = data_for_display, lng = ~Long, lat = ~Lat,
                  radius = ~active_scaled, popup = ~text, fillColor = ~color, stroke = FALSE, fillOpacity = 0.5,
                  group = stringr::str_match(date_to_choose, "\\d{4}\\-\\d{2}\\-\\d{2}")[1,1]
     )
     curr_date(stringr::str_match(date_to_choose, "\\d{4}\\-\\d{2}\\-\\d{2}")[1,1])
   })
}