################################################################################
# Module featured_packages
#
# Author: Stefan Schliebs
# Created: 2019-03-06 08:25:53
################################################################################



# Module UI ---------------------------------------------------------------

featured_packages_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("module_ui")) %>% withSpinner()
}



# Module server logic -----------------------------------------------------

featured_packages <- function(input, output, session, d_pkg_releases, d_pkg_dependencies, d_pkg_details) {


  # Featured packages -------------------------------------------------------

  d_latest_release <- reactive({
    d_pkg_releases() %>% 
      group_by(package) %>% 
      filter(n() > 1) %>% 
      filter(version != "archived") %>% 
      summarise(published = max(published)) %>%
      ungroup()
  })
  
  d_featured_packages <- reactive({
    d_pkg_dependencies() %>% 
      filter(dependency != "R") %>%
      count(dependency, sort = TRUE) %>% 
      left_join(d_latest_release(), by = c(dependency = "package")) %>%
      arrange(desc(published)) %>% 
      head(n = 100) %>% 
      arrange(desc(n)) %>% 
      head(n = 6) %>% 
      left_join(d_pkg_details(), by = c(dependency = "package")) %>% 
      mutate(
        slide = seq(1, n()),
        first = 1 == slide
      )
  })
  

  # Package UI --------------------------------------------------------------
  
  output$module_ui <- renderUI({
    req(d_featured_packages())
    
    l_featured_packages <- split(d_featured_packages(), seq(nrow(d_featured_packages())))
    
    htmlTemplate(
      filename = "www/modules/featured_packages/index.html",
      items = package_items_ui(l_featured_packages),
      captions = package_captions_ui(l_featured_packages)
    )
  })
  
  package_items_ui <- function(l_featured_packages) {
    tagList(
      lapply(l_featured_packages, function(x) {
        htmlTemplate(
          filename = "www/modules/featured_packages/item.html", 
          package = x$dependency, 
          title = x$title,
          published = strftime(x$published, "%Y-%m-%d"),
          description = x$description,
          version = x$version,
          author = x$author,
          active_class = ifelse(x$first, "active", "")
        )
      }),
      tags$script(src = "js/main.js")
    )
  }
  
  package_captions_ui <- function(l_featured_packages) {
    tagList(
      lapply(l_featured_packages, function(x) {
        htmlTemplate(
          filename = "www/modules/featured_packages/caption.html", 
          package = x$dependency,
          slide = x$slide - 1,
          published = strftime(x$published, "%Y-%m-%d"),
          active_class = ifelse(x$first, 'class="active"', "")
        )
      })
    )
  }
}
