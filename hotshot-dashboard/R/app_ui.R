#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import fresh
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  # create tribble for box global config
  box_config <- tibble::tribble(
    ~background, ~labelStatus,
    "danger", "warning",
    "purple", "success",
    "success", "primary",
    "warning", "danger",
    "fuchsia", "info"
  )
  
  # box factory function
  box_factory <- function(background, labelStatus) {
    box(
      title = "Cyberpunk Box",
      collapsible = TRUE,
      background = background,
      height = "200px",
      label = boxLabel(1, labelStatus)
    )
  }
  
  # pmap magic
  boxes <- purrr::pmap(box_config, box_factory)
  
  my_theme <- hotshot_theme()
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      freshTheme = my_theme,
      header = dashboardHeader(
        skin = "light"
      ),
      sidebar = dashboardSidebar(
        skin = "light",
        sidebarMenu(
          id = "sidebarMenu",
          menuItem(
            text = "Overview",
            tabName = "tab1"
          ),
          menuItem(
            text = "Leaderboard",
            tabName = "tab2"
          ),
          menuItem(
            text = "GP & Track Stats",
            tabName = "tab3"
          )
        )
      ),
      body = dashboardBody(
        #boxes
        tabItems(
          tabItem(
            tabName = "tab1",
            mod_welcome_ui("welcome_ui_1")
          ),
          tabItem(
            tabName = "tab2",
            mod_leaderboard_ui("leaderboard_ui_1")
          ),
          tabItem(
            tabName = "tab3",
            mod_trackstats_ui("trackstats_ui_1")
          )
        )
      ),
      controlbar = dashboardControlbar(),
      title = "Fresh theming",
      dark = FALSE,
      fullscreen = TRUE,
      scrollToTop = TRUE
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'hotshots.dashboard'
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/css/TTSupermolotNeue.css"),
    tags$style(HTML("body {font-family: 'TTSupermolotNeue-Bold', sans-serif;}" )),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    waiter::use_waiter()
  )
}