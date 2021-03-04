library(shiny)
library(shiny.grid)
library(shiny.blank)
library(htmltools)
library(tidyr)
library(leaflet)
library(R6)
library(googlesheets)
library(glue)
library(utils)
library(dplyr)
library(jsonlite)
library(modules)
library(sass)
library(readxl)

# Remember to install github packages if you dont have them already.
# install.packages('devtools')
# library(devtools)
# devtools::install_github('pedrocoutinhosilva/shiny.grid')
# devtools::install_github('pedrocoutinhosilva/shiny.blank')

# Process and minify styles
sass(
  sass::sass_file("styles/main.scss"),
  #cache_options = sass_cache_options(FALSE),
  options = sass_options(output_style = "compressed"),
  output = "www/styles/sass.min.css"
)

# Generic gameManager to initialize ui elements
gameManager <- use("logic/gameManager.R")$gameManager$new()
