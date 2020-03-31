################################################################################
# Module package_chart
#
# Author: Stefan Schliebs
# Created: 2019-05-25 10:51:53
################################################################################


# Module constants --------------------------------------------------------

F_ICON_BOX_TEMPLATE <- "www/modules/icon_box/index.html"


# Module UI ---------------------------------------------------------------

icon_box_ui <- function(id, title, icon = "glyphicon-time") {
  ns <- NS(id)

  htmlTemplate(
    filename = F_ICON_BOX_TEMPLATE,
    value = textOutput(ns("value"), inline = TRUE),
    title = title, 
    icon = icon
  )
}



# Module logic ------------------------------------------------------------

icon_box <- function(input, output, session, value) {
  
  box_values <- reactiveValues(
    value = value
  )

  output$value <- renderText(box_values$value)
  
  list(
    set_values = function(value) {
      box_values$value <- value
    }
  )
}

