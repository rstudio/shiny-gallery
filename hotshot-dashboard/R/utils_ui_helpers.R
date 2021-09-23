#' @import bs4Dash
#' @import fresh
#' @noRd
hotshot_theme <- function(base_font = 'TTSupermolotNeue-Bold') {
  my_theme <- create_theme(
    bs4dash_vars(
      navbar_light_color = "#081c5e",
      navbar_dark_color = "#081c5e",
      navbar_light_active_color = "#FFF",
      navbar_light_hover_color = "#FFF"
    ),
    bs4dash_yiq(
      contrasted_threshold = 10,
      text_dark = "#FFF", 
      text_light = "#272c30"
    ),
    bs4dash_layout(
      main_bg = "#0676d3"
    ),
    bs4dash_sidebar_light(
      bg = "#272c30", 
      color = "#bec5cb",
      hover_color = "#FFF",
      submenu_bg = "#272c30", 
      submenu_color = "#FFF", 
      submenu_hover_color = "#FFF"
    ),
    bs4dash_status(
      primary = "#5E81AC", 
      danger = "#BF616A", 
      light = "#081c5e"
    ),
    bs4dash_color(
      gray_900 = "#FFF"
    ),
    bs4dash_font(
      family_base = base_font
    )
  )
  
  
  return(my_theme)
}
