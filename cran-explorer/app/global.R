################################################################################
# Entry point of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2019-01-22 09:42:17
################################################################################


# import libraries
library(shiny)
library(dplyr)
library(glue)
library(highcharter)
library(sparkline)
library(lubridate)
library(purrr)
library(visNetwork)
library(shinyWidgets)
library(shinycssloaders)
library(httr)

library(future)
library(promises)
plan(multiprocess)



# Global constants --------------------------------------------------------

S3_DATA_PACKAGE <- "https://s3.amazonaws.com/cran-explorer/data.zip"

S3_PKG_DEPENDENCIES <- "https://s3.amazonaws.com/cran-explorer/pkg_dependencies.csv"
S3_PKG_RELEASES <- "https://s3.amazonaws.com/cran-explorer/pkg_releases.csv"
S3_TILE_SUMMARIES <- "https://s3.amazonaws.com/cran-explorer/tile_summary.rds"



# Load modules and utils --------------------------------------------------

# load utilities
source("utils/ui-utils.R")

# load modules
source("modules/mod_pretty_value_box.R")
source("modules/mod_graph_network.R")
source("modules/mod_package_chart.R")
source("modules/mod_featured_packages.R")
source("modules/mod_icon_box.R")
