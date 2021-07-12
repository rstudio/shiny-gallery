#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import pins
#' @noRd
app_server <- function( input, output, session ) {
  # set default options for echarts
  echarts4r::e_common(font_family = "TTSupermolotNeue-Bold")
  
  # load race results from internal data frame
  hotshot_stat_df <- reactiveVal(gp_results)
  
  # List the first level callModules here
  callModule(mod_welcome_server, "welcome_ui_1", hotshot_stat_df)
  callModule(mod_leaderboard_server, "leaderboard_ui_1", hotshot_stat_df)
  callModule(mod_trackstats_server, "trackstats_ui_1",  hotshot_stat_df)
}
