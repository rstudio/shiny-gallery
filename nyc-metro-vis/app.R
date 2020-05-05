library(leaflet)
library(tidyverse)
library(shiny)
library(lubridate)
library(promises)
library(future)

plan(multiprocess)

shinyOptions(cache = diskCache("./clock-cache"))

ui <- fluidPage(
  
  sidebarPanel(
    
    uiOutput(outputId = "slider"),
    h4(textOutput("time")),
    plotOutput("clock",width = 250,height = 250),
    actionButton("refresh",label = "Refresh Data",icon = icon("fa-refresh") )),
  mainPanel(
    leafletOutput("mymap1",width = "100%",height = "800") )
)


server <- function(input, output) {
  
  
  
  # loading data after etl --------------------------------------------------
  
  all_clean <- reactiveVal(read_rds("data/cache_all_data.rds"))
  
  
  # utilizing promises to handle new data download and manipulation --------
  
  observeEvent(input$refresh, {
    future({  
    data_station <- read_rds("data/cache_coordinate.rds")
  
    data3 <- read_csv("http://web.mta.info/developers/data/nyct/turnstile/turnstile_190302.txt")
    
    all <- inner_join(data_station %>% 
                        mutate(`Station Name`= toupper(`Station Name`)),data3,by=c("Station Name"="STATION","Division"="DIVISION")) %>% 
      filter(DESC == 'REGULAR',
             Entry =='YES') 
    
    all_clean <- all %>% 
      mutate(TIME_C = paste(DATE,TIME) %>% mdy_hms(),
             ENTRIES = as.numeric(ENTRIES),
             EXITS = as.numeric(EXITS)) %>% 
      distinct(`Station Latitude`,`Station Longitude`,TIME_C,Line,ENTRIES,SCP
               ,`C/A`) %>% 
      filter(SCP == '00-00-00') %>% 
      group_by(`Station Latitude`,`Station Longitude`,`C/A`) %>% 
      arrange(TIME_C) %>% 
      mutate(ENTRIES_new = ENTRIES - lag(ENTRIES)) %>% 
      mutate(ENTRIES_new = case_when(
        is.na(ENTRIES_new) ~ 0,
        TRUE ~ ENTRIES_new
      )) %>% ungroup()
    
    saveRDS(all_clean, "data/cache_all_data.rds")
    })
    
  })  
  
  
  
  # render ui for slider ----------------------------------------------------
  
  output$slider <- renderUI({
    sliderInput("year","Time",min = min(all_clean()$TIME_C), max = max(all_clean()$TIME_C), value = min(all_clean()$TIME_C),step = 3600, timezone = "+0000", animate = T)
  })
  
  
  # adding the text for date display ----------------------------------------
  
  
  output$time <- renderText({
    
    req(input$year)
    paste(input$year %>% date(),ifelse( hour(input$year) >= 12, "PM","AM"))
    
  })
  
  
  # adding time animation with renderCachePlot ------------------------------
  
  output$clock <- renderCachedPlot({
    
    time <- data.frame(list(x=c(ifelse(hour(input$year) >= 12, hour(input$year)-12 + (minute(input$year) )*0.2/12 , hour(input$year)+( minute(input$year) )*0.2 ), (minute(input$year) )*0.2 ), y=c(.9, 1.3)))
    
    ggplot(time, aes(xmin=x, xmax=x+0.1, ymin=0, ymax=y))+
      geom_rect(aes(alpha=0.5))+
      scale_x_continuous(limits=c(0,11.98333), breaks=0:11, 
                         labels=c(12, 1:11))+
      scale_y_continuous(limits=c(0,1.3))+scale_alpha()+theme_bw()+
      coord_polar()+
      theme(axis.text.y=element_blank(), axis.ticks=element_blank(), 
            panel.grid.major=element_blank(), 
            strip.background = element_rect(colour = 'white'),
            legend.title = element_blank(),
            legend.position = "none")  
    
  },
  cacheKeyExpr = { input$year })
  
  # map part ----------------------------------------------------------------
  
  output$mymap1 <- renderLeaflet({
    
    pal <- colorQuantile(c("navy", "pink","orange","red"),domain = quantile(all_clean()$ENTRIES_new))     
    
    all_clean() %>% 
      filter(TIME_C == min(all_clean()$TIME_C)) %>% 
      leaflet() %>% 
      
      addProviderTiles(providers$CartoDB.DarkMatter) %>%   
      addCircleMarkers(lng = ~`Station Longitude`,
                       lat = ~`Station Latitude`,
                       radius = ~ENTRIES_new/100,
                       fillOpacity =0.6,
                       fill = F,
                       color = ~pal(ENTRIES_new)
      ) 
    
    
  })
  
  
  observeEvent(input$year,{ 
    pal <- colorQuantile(c("navy", "red","pink","orange"),domain = quantile(all_clean()$ENTRIES_new))
    
    
    # adding day and night section --------------------------------------------
    
    if (hour(input$year) >= 7 & hour(input$year) < 17 ) {
      
      leafletProxy("mymap1", data= all_clean() %>% 
                     filter(TIME_C < input$year) %>% 
                     filter(TIME_C == max(TIME_C)) ) %>% 
        clearMarkers() %>% 
        addTiles() %>% 
        addCircleMarkers(~`Station Longitude`,
                         ~`Station Latitude`
                         ,radius = ~ENTRIES_new/100,fillOpacity =0.6,fill = F,  color = ~pal(ENTRIES_new)
        )
    } else {
      leafletProxy("mymap1", data= all_clean() %>% 
                     filter(TIME_C < input$year) %>% 
                     filter(TIME_C == max(TIME_C)) ) %>% 
        clearMarkers() %>% 
        addProviderTiles(providers$CartoDB.DarkMatter) %>% 
        addCircleMarkers(~`Station Longitude`,
                         ~`Station Latitude`
                         ,radius = ~ENTRIES_new/100,fillOpacity =0.6,fill = F,  color = ~pal(ENTRIES_new)
        )}
  })
  
}

shinyApp(ui, server)
