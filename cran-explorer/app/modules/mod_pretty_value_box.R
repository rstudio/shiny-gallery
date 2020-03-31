################################################################################
# Module pretty_value_box
#
# Author: Stefan Schliebs
# Created: 2019-01-31 08:07:42
################################################################################


# Module constants --------------------------------------------------------

F_PRETTY_VALUE_BOX_TEMPLATE <- "www/modules/pretty_value_box/index.html"


# Module UI ---------------------------------------------------------------

pretty_value_box_ui <- function(id, background_color = "#e2a267", icon_name = "question-circle") {
  # define namespace function
  ns <- NS(id)
  
  htmlTemplate(
    filename = F_PRETTY_VALUE_BOX_TEMPLATE,
    background_color = background_color,
    icon = icon(icon_name),
    title = textOutput(ns("title"), inline = TRUE),
    value = textOutput(ns("value"), inline = TRUE),
    last_days = textOutput(ns("last_days"), inline = TRUE),
    date_range = textOutput(ns("date_range"), inline = TRUE),
    inline_bar = sparklineOutput(ns("inline_bar"), width = "300px") %>% 
      withSpinner(proxy.height = "50px", size = 0.5, type = 1, color = "#EEEEEE"),
    trend_icon = uiOutput(ns("trend_icon"), inline = TRUE),
    trend = textOutput(ns("trend"), inline = TRUE),
    trend_label = textOutput(ns("trend_label"), inline = TRUE)
  )
}  



# Module server logic -----------------------------------------------------

pretty_value_box <- function(input, output, session, 
                             title = "-", value = 0, last_days = "-", date_range = "-", inline_bar = 0,
                             trend = 0, trend_icon = "", trend_label = "-") {
  
  ns <- session$ns
  
  # initialise data as reactiveValues with defaults from the module parameters
  box_values <- reactiveValues(
    title = title,
    value = value,
    last_days = last_days,
    date_range = date_range,
    inline_bar = inline_bar,
    trend = trend,
    trend_icon = trend_icon,
    trend_label = trend_label
  )


  # Render data -------------------------------------------------------------
  
  output$value <- renderText(box_values$value)
  
  output$title <- renderText(box_values$title)
  
  output$last_days <- renderText(box_values$last_days)
  
  output$date_range <- renderText(box_values$date_range)
  
  output$inline_bar <- sparkline::renderSparkline({
    sparkline(
      box_values$inline_bar, 
      type = "bar", 
      height = "36px", 
      barSpacing = 10, 
      negBarColor = "#8A3E0B",
      barColor = "#424956",
      barWidth = 18,
      width = 150
    )
  })
  
  output$trend <- renderText(paste0(box_values$trend, "%"))
  
  output$trend_label <- renderText(box_values$trend_label)
  
  output$trend_icon <- renderUI({
    req(box_values$trend)
    
    if (as.numeric(box_values$trend) < 0) {
      icon("arrow-circle-down down fa-inverse")
    } else {
      icon("arrow-circle-up up fa-inverse")
    }
  })
  

  # return interface to this module
  list(
    values = box_values,
    set_values = function(title = NULL, value = NULL, last_days = NULL, date_range = NULL, inline_bar = NULL, 
                          trend_icon = NULL, trend_label = NULL, trend = NULL) {
      if (!is.null(title)) box_values$title = title
      if (!is.null(value)) box_values$value = value
      if (!is.null(last_days)) box_values$last_days = last_days
      if (!is.null(date_range)) box_values$date_range = date_range
      if (!is.null(inline_bar)) box_values$inline_bar = inline_bar
      if (!is.null(trend_icon)) box_values$trend_icon = trend_icon
      if (!is.null(trend_label)) box_values$trend_label = trend_label
      if (!is.null(trend)) box_values$trend = trend
    }
  )
}
