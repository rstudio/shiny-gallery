#' Launch
#' 
#' Launche the Freedom of Press Shiny Application
#'
#' @export
#' @import fopi
#' @import fullPage
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_fopi <- function() {
  utils::data("fopi", package = "fopi")
  
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list()
  )
}
