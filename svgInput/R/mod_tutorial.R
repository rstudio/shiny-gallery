

# Module UI function
mod_tutorial_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      bs4Dash::box(
        title = "Select Iris Sepal or Petal", 
        elevation = 4,
        width = 6,
        solidHeader = FALSE, 
        status = "olive",
        collapsible = TRUE,
        shiny::fluidRow(
          shiny::column(width = 2, svg_iris(ns("iris_input"))),
          shiny::column(width = 10, plotly::plotlyOutput(ns("iris_plot")))
        )
      ),
      bs4Dash::box(
        title = "Bar and Pie chart", 
        elevation = 4,
        width = 6,
        solidHeader = FALSE, 
        status = "olive",
        collapsible = TRUE,
        shiny::fluidRow(
          shiny::column(width = 2, svg_bar_pie(ns("bar_pie"))),
          shiny::column(width = 10, plotly::plotlyOutput(ns("bar_pie_plot")))
        )
      )
    ),
    if (Sys.getenv("BROWSE") == "TRUE") { shiny::actionButton(ns("browse"), "Browse")}
  )
}

# Module server function
mod_tutorial_server <- function(id, stringsAsFactors) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$browse, { browser() })
    
    output$iris_plot <- plotly::renderPlotly({
      shiny::req(input$iris_input)
      
      iris_ <- iris
      if (input$iris_input == "sepal") {
        iris_$x <- iris_$Sepal.Width
        iris_$y <- iris_$Sepal.Length
      } else if (input$iris_input == "petal") {
        iris_$x <- iris_$Petal.Width
        iris_$y <- iris_$Petal.Length
      }
      
      cls <- RColorBrewer::brewer.pal(3, "Dark2")
      
      plotly::plot_ly(iris_) %>% 
        plotly::add_markers(data = dplyr::filter(iris_, Species == "setosa"),
                            x = ~x, y = ~y, 
                            marker = list(color = cls[1]))  %>% 
        plotly::add_markers(data = dplyr::filter(iris_, Species == "versicolor"),
                            x = ~x, y = ~y, 
                            marker = list(color = cls[2]))  %>% 
        plotly::add_markers(data = dplyr::filter(iris_, Species == "virginica"),
                            x = ~x, y = ~y, 
                            marker = list(color = cls[3])) %>% 
        plotly::layout(
          title = paste0("Iris <b>", input$iris_input, "s</b> dimensions for ",
          '<span style = "color:', cls[1], '">Setosa</span>, ',
          '<span style = "color:', cls[2], '">Versicolor</span> and ',
          '<span style = "color:', cls[3], '">Virginica</span> species'),
          xaxis = list(title = "Width"), yaxis = list(title = "Length"),
          showlegend = FALSE)
    })
    
    shiny::observeEvent(input$iris_input, {
      
      if (input$iris_input == "sepal") {
        shinyjs::removeClass(selector = "[data-shinyclick = 'petal']", class = "opaque80")
        shinyjs::addClass(selector = "[data-shinyclick = 'sepal']", class = "opaque80")
      } else if (input$iris_input == "petal") {
        shinyjs::removeClass(selector = "[data-shinyclick = 'sepal']", class = "opaque80")
        shinyjs::addClass(selector = "[data-shinyclick = 'petal']", class = "opaque80")
      }
      
    })
    
    output$bar_pie_plot <- plotly::renderPlotly({
      
      input$bar_pie
      plotly::plot_ly(x = 1:10, y = rnorm(10))
    })
    
    shiny::observeEvent(input$bar_pie, {
      
      if (input$bar_pie == "bar") {
        shinyjs::removeClass(selector = "[data-shinyclick = 'bar']", class = "opaque40")
        shinyjs::addClass(selector = "[data-shinyclick = 'pie']", class = "opaque40")
      } else if (input$bar_pie == "pie") {
        shinyjs::removeClass(selector = "[data-shinyclick = 'pie']", class = "opaque40")
        shinyjs::addClass(selector = "[data-shinyclick = 'bar']", class = "opaque40")
      }
      
    })
    
  })  
}