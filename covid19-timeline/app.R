library(plyr)
library(data.table)
library(dotenv)
library(dplyr)
library(english)
library(forecast)
library(fresh)
library(highcharter)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(leaflet.mapboxgl)
library(plotly)
library(quantmod)
library(readr)
library(sass)
library(sf)
library(shiny)
library(shinyanimate)
library(shinybusy)
library(shinyjs)
library(shinyWidgets)
library(stringr)
library(waiter)
# load the data
options(scipen = 15)
# load the api key for mapbox and the seed
try(load_dot_env("key.env"), silent = TRUE)
key <- Sys.getenv("MAPBOX_KEY")
options(mapbox.accessToken = key)
css <- sass(sass_file("styles.scss"))
# load the interface
source(file.path("server", "load_data.R"), local = TRUE)
source(file.path("server", "create_story.R"), local = TRUE)
ui <- source(file.path("ui", "ui.R"), local = TRUE)$value
# load the server
server <- function(input, output, session) {
  source(file.path("server", "leaflet_maps.R"), local = TRUE)$value
  source(file.path("server", "event_observer.R"), local = TRUE)$value
  source(file.path("server", "reactive_events.R"), local = TRUE)$value
  source(file.path("server", "text_outputs.R"), local = TRUE)$value
  source(file.path("server", "ui_outputs.R"), local = TRUE)$value
  source(file.path("server", "plotly_outputs.R"), local = TRUE)$value
  source(file.path("server", "highcharter_outputs.R"), local = TRUE)$value
  source(file.path("server", "observer.R"), local = TRUE)$value
  source(file.path("server", "modal.R"), local = TRUE)$value
  waiter_hide()
}
shinyApp(ui, server)
