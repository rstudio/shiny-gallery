################################################################################
# Module package_chart
#
# Author: Stefan Schliebs
# Created: 2019-03-04 09:28:48
################################################################################



# Module UI ---------------------------------------------------------------

package_chart_ui <- function(id) {
  ns <- NS(id)
  
  htmlTemplate(
    filename = "www/modules/package_chart/index.html",
    switch_new_updated = switchInput(
      ns("switch_new_updated"), 
      label = "Packages",
      onLabel = "New", offLabel = "Updated",
      onStatus = "success", offStatus = "danger",
      value = TRUE, size = "mini"
    ),
    chart_packages = highchartOutput(ns("chart_packages"), width = "100%", height = "300px") %>% 
      withSpinner(size = 0.5, proxy.height = "300px", type = 6, color = "#FFFFFF")
  )
}



# Server logic ------------------------------------------------------------

package_chart <- function(input, output, session, d_pkg_releases) {
  ns <- session$ns
  
  # Chart new/updated packages ----------------------------------------------
  
  # determine new packages
  d_pkg_new <- reactive({
    d_pkg_releases() %>% 
      filter(published >= "2000-01-01") %>% 
      group_by(package) %>% 
      filter(published == min(published)) %>%
      ungroup()
  })
  
  # determine updated packages
  d_pkg_updated <- reactive({
    d_pkg_releases() %>%
      filter(published >= "2000-01-01") %>% 
      group_by(package) %>% 
      filter(n() > 1) %>% 
      filter(version != "archived", published == max(published)) %>%
      ungroup()
  })
  
  
  output$chart_packages <- renderHighchart({
    req(d_pkg_releases())
    
    # select data to plot based on input switch
    if (input$switch_new_updated) {
      d <- d_pkg_new() 
      chart_title <- "Number of monthly new packages"
      chart_color <- "#77AB43"
    } else {
      d <- d_pkg_updated()
      chart_title <- "Number of monthly updated packages"
      chart_color <- "#FF2700"
    }
    
    d %>% 
      mutate(year_month = strftime(published, "%Y-%m-01") %>% as.Date()) %>% 
      count(year_month) %>% 
      hchart("spline", hcaes(x = year_month, y = n), color = chart_color) %>% 
      hc_title(text = chart_title) %>% 
      hc_xAxis(title = list(text = "Time")) %>% 
      hc_yAxis(title = list(text = "")) %>% 
      hc_add_theme(hc_theme_538())
  })
}