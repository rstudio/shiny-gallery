################################################################################
# Server logic of the app
#
# Author: Stefan Schliebs
# Created: 2019-01-22 09:44:28
################################################################################


server <- function(input, output, session) {

  # load namespace from session
  ns <- session$ns
  

  # Data reactives ----------------------------------------------------------

  # download data package from AWS and unpack zip file into app directory,  
  # all other data reactives load their data from the contents of this zip file 
  data_package <- reactive({
    cat("Downloading data package from", S3_DATA_PACKAGE, "\n")
    future({
      download.file(S3_DATA_PACKAGE, "data.zip")
      unzip("data.zip")
    })
  })

  d_pkg_dependencies <- reactive({
    req(value(data_package()))
    readr::read_csv("data/pkg_dependencies.csv")
  })
  
  d_pkg_releases <- reactive({
    req(value(data_package()))
    readr::read_csv("data/pkg_releases.csv")
  })
  
  d_pkg_details <- reactive({
    req(value(data_package()))
    readRDS("data/pkg_details.rds")
  })
  
  l_tile_summary <- reactive({
    req(value(data_package()))
    readRDS("data/tile_summary.rds")
  })
  

  # Summary tiles -----------------------------------------------------------

  # render UI of the summary tiles
  # NOTE: the reason for this cumbersome way is to get the withSpinner() into the UI
  output$header_ui <- renderUI({
    tags$p(
      icon_box_ui(ns("n_cran_packages"), title = "packages", icon = "glyphicon-gift"),
      icon_box_ui(ns("last_update"), title = "data updated", icon = "glyphicon-time")
    )
  })
  
  m_cran_packages <- callModule(icon_box, id = "n_cran_packages", "loading...")
  m_last_update <- callModule(icon_box, id = "last_update", "loading...")
  
  observeEvent(d_pkg_releases(), {
    m_cran_packages$set_value(n_distinct(d_pkg_releases()$package) %>% format(big.mark = ","))
    m_last_update$set_value(max(d_pkg_releases()$published, na.rm = TRUE) %>% strftime("%d %b %Y"))
  })

  

  # CRAN stats info boxes ---------------------------------------------------
  
  # define the info boxes as instances of the pretty_value_box module
  m_pkgs_new_month <- callModule(pretty_value_box, id = "packages-new-month")
  m_pkgs_updated_month <- callModule(pretty_value_box, id = "packages-updated-month")
  m_pkgs_new_year <- callModule(pretty_value_box, id = "packages-new-year")
  m_pkgs_updated_year <- callModule(pretty_value_box, id = "packages-updated-year")
  
  # populate the info boxes when new tile data become available
  observeEvent(l_tile_summary(), {
    map2(
      list(m_pkgs_new_month, m_pkgs_updated_month, m_pkgs_new_year, m_pkgs_updated_year),
      l_tile_summary(),
      function(mod, values) {
        do.call(mod$set_values, values)
      }
    )
  })
  
  
  # Dependency network ------------------------------------------------------

  callModule(graph_network, "dependency_network", d_pkg_dependencies, d_pkg_details, d_pkg_releases)  
  

  # Package chart -----------------------------------------------------------

  callModule(package_chart, "package_chart", d_pkg_releases)
  

  # Featured packages -------------------------------------------------------

  callModule(featured_packages, "featured_packages", d_pkg_releases, d_pkg_dependencies, d_pkg_details)
  
}