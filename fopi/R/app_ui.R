#' @import shiny
app_ui <- function() {

  options <- list()

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    pagePiling(
      sections.color = c('#2f2f2f', '#2f2f2f', '#f9f7f1', '#2f2f2f'),
      opts = options,
      menu = c(
        "Home" = "home",
        "Map" = "map",
        "Series" = "ts",
        "About" = "about"
      ),
      pageSectionImage(
        center = TRUE,
        img = "www/img/reading.jpg",
        menu = "home",
        h1(typed::typedOutput("title"), class = "header shadow-dark"),
        h3(
          class = "light footer",
          "by", tags$a("news-r", href = "https://news-r.org", class = "link"), "with", emo::ji("coffee")
        )
      ),
      pageSection(
        center = TRUE,
        menu = "map",
        mod_map_ui("map")
      ),
      pageSection(
        center = TRUE,
        menu = "ts",
        mod_ts_ui("ts")
      ),
      pageSection(
        center = TRUE,
        menu = "about",
        h1("About", class = "header shadow-dark"),
        h2(
          class = "shadow-light",
          tags$a("The code", href = "https://github.com/news-r/fopi.app", target = "_blank", class = "link"),
          "|",
          tags$a("The API", href = "https://github.com/news-r/fopi", target = "_blank", class = "link")
        ),
        h3(
          class = "light footer",
          "by", tags$a("news-r", href = "https://news-r.org", class = "link"), "with", emo::ji("coffee")
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'fopi.app')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(
      rel = "stylesheet", href = shinythemes::shinytheme("sandstone")
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/css/style.css"),
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=UA-74544116-1"),
    tags$script(
      "window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-74544116-1');"
    )
  )
}
