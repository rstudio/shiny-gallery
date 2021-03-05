library(dplyr)
library(stringr)
library(shiny)
library(shinymaterial)
library(plotly)
library(viridisLite)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(RColorBrewer)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(scales)

source("data_gen.R")
source("about.R")
source("timeline_charts.R")
source("running_charts.R")
source("dt_table.R")
source("italy.R")
source("fun.R")
source("map.R")

#---- population data ----
population_data <- read.csv("./population-figures-by-country-csv_csv.csv")
population_data_short <- rbind(
  population_data %>% select(Country, Country_Code, Year_2016) %>%
    mutate(Country = str_replace(Country, "Korea, Rep.", "Korea, South")) %>%
    mutate(Country = str_replace(Country, "Czech Republic", "Czechia")) %>%
    mutate(Country = str_replace(Country, "Russian Federation", "Russia")) %>%
    mutate(Country = str_replace(Country, "United States", "US")) %>%
    mutate(Country = str_replace(Country, "Iran, Islamic Rep.", "Iran")),
  data.frame(Country = c("Taiwan", "China (only Hubei)"), Country_Code = c("TAI", "XXX"), Year_2016 = c(23780000, 58500000))
)



#---- !! UI !! ----
# Define UI for application that draws a histogram
ui <- material_page(
  nav_bar_color = "blue",
  
  title = "COVID-19",
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/bootstrap.min.css"),
    tags$script(type = "text/javascript", src = "jquery_browser.js")
  ),
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  tags$head(includeHTML("google-analytics.html")),
  div(id = "wait", style = "    position: fixed;
    top: 0rem;
      margin: auto;
      height: 100px;
      width: 100px;
margin: 20% auto; /* Will not center vertically and won't work in IE6/7. */
left: 0;
      right: 0;
      /* display: none; */
      opacity: 1;"),
  #---- material_side_nav ----
  material_side_nav(
    fixed = TRUE, 
    background_color = "white",
    image_source = "img/corona-4938929_1920.jpg",
    tags$a(class="sidenav-close", "data-target"="slide-out", href="#", "x"),
    # Place side-nav tabs within side-nav
    material_side_nav_tabs(
      side_nav_tabs = c(
        "WorldMap" = "map",
        "All Countries Table" = "all_countries",
        "Timeline Charts" = "housing_prices",
        "Compare countries charts" = "running_charts",
        "Italy Charts" = "italy",
        "Fun facts" = "fun",
        "About" = "about"
      ),
      icons = c("map", "format_list_bulleted", "insert_chart", "exposure", "local_pizza", "toys", "info_outline")
    ),
    div(style="height:50px"),
    material_card(
      
          htmlOutput("last_modified")
    )
  ),
  #---- UI objects ----
  material_side_nav_tab_content(
    side_nav_tab_id = "map",
    mapUI("map_module")
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "all_countries",
    dt_tableUI("dt_table_module")
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "housing_prices",
    timeline_chartsUI("timeline_charts_module")
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "running_charts",
    running_chartsUI("running_charts_module")
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "italy",
    italyUI("italy_module")
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "fun",
    funUI("fun_module")
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "about",
    aboutUI("about_module")
  )
 
)

#---- !! server !! ----
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #---- git pull ----
  git_pull <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "Updating CSSE data", value = 0.5)
    wait_show(session)
    if (dir.exists("COVID-19")) {
      setwd("COVID-19")
      system("git pull")
      setwd("..")
    } else {
      
      system("git clone https://github.com/CSSEGISandData/COVID-19.git", timeout = 1000)
    }
    wait_hide(session)
  })
  
  git_pull_italy <- reactive({
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Updating Italy data", value = 0.5)
    wait_show(session)
    if (dir.exists("covid19Italy")) {
      setwd("covid19Italy")
      system("git pull")
      setwd("..")
    } else {
      
      system("git clone https://github.com/RamiKrispin/covid19Italy.git", timeout = 1000)
    }
    wait_hide(session)
  })
  #---- Italy data ----
  italy_data <- reactive({
    git_pull_italy()
    wait_show(session)
    list(
      total = read.csv("covid19Italy/csv/italy_total.csv"),
      region = read.csv("covid19Italy/csv/italy_region.csv"),
      province = read.csv("covid19Italy/csv/italy_province.csv")
    )
  })
  #---- Fun data ----
  fun_data <- reactive({
    list(
      toilet_paper = read.csv("toilet_paper.csv"),
      pasta = read.csv("pasta.csv")
    )
  })
  #---- CSSE data ----
  # data_confirmed <- reactive({
  #   git_pull()
  #   wait_show(session)
  #   per_country_data(read.csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
  # })
  # 
  # data_death <- reactive({
  #   git_pull()
  #   per_country_data(read.csv("./COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
  # })
  # data_recovered <- reactive({
  #   git_pull()
  #   
  # })
  
  all_data <- reactive({
    
    git_pull()
    wait_show(session)
    generate_from_daily("./COVID-19/csse_covid_19_data/csse_covid_19_daily_reports") %>%
      rename(value = confirmed) %>%
      new_data_gen() %>%
      add_mortality()
  })
  #---- Map data ----
  
  map_data <- reactive({
    git_pull()
    wait_show(session)
    map_data <- generate_all_from_daily("./COVID-19/csse_covid_19_data/csse_covid_19_daily_reports")
    wait_hide(session)
    return(map_data)
  })
  
  all_dates <- reactive({
    map_data()$date %>% unique() %>% as.POSIXct(origin = "1970-01-01")
  })
  
  #---- last mod ----
  output$last_modified <- renderUI({
    git_pull()
    div(style = "font-size: 0.5em; text-align:center",
        tags$p("Data last modified:"),
        tags$p(file.info("COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")["mtime"][1,1]),
        tags$br(),
        HTML(
          "<p>&copy; <a target='_new' href='https://mail-wolf.de/?page_id=1292'>zappingseb</a></p>"
        )
    )
  })
  callModule(about, "about_module")
  callModule(timeline_charts, "timeline_charts_module",
             all_data = all_data,
             map_data = map_data
             )
  callModule(running_charts, "running_charts_module",
             all_data = all_data,
             map_data = map_data,
             population_data = population_data_short
             )
  callModule(dt_table, "dt_table_module", all_data = all_data, population_data_short = population_data_short)
  callModule(italy, "italy_module", italy_data = italy_data)
  callModule(fun, "fun_module", fun_data = fun_data)
  callModule(map, "map_module", map_data = map_data, all_dates = all_dates)
}

# Run the application 
shinyApp(ui = ui, server = server)
