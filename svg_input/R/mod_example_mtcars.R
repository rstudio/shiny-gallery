
mod_example_mtcars_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 2, style = "margin-top:100px;",
                    shiny::div(style = "width:100%:", svg_mtcars_02(ns("mtcars_var")))),
      shiny::column(width = 10, plotly::plotlyOutput(ns("mtcars_plot"), height = "600px"))
    ),
    if (Sys.getenv("BROWSE") == "TRUE") { shiny::actionButton(ns("browse"), "Browse")}
  )
}

mod_example_mtcars_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$browse, { browser() })
    
    output$mtcars_plot <- plotly::renderPlotly({
      shiny::req(input$mtcars_var)
      
      if (input$mtcars_var == "wt") {
        p <- plotly::plot_ly(mtcars, x = ~wt, color = ~wt, y = paste0(rownames(mtcars), " "), type = "bar")
        title_ <- "Weight in 1000lbs"
      }
      if (input$mtcars_var == "mpg") {
        p <- plotly::plot_ly(mtcars, x = ~mpg, color = ~mpg, y = paste0(rownames(mtcars), " "), type = "bar")
        title_ <- "Miles per gallon"
      }
      if (input$mtcars_var == "cyl") {
        p <- plotly::plot_ly(mtcars, x = ~cyl, color = ~cyl, y = paste0(rownames(mtcars), " "), type = "bar")
        title_ <- "Number of cylinders"
      }
      if (input$mtcars_var == "gear") {
        p <- plotly::plot_ly(mtcars, x = ~gear, color = ~gear, y = paste0(rownames(mtcars), " "), type = "bar")
        title_ <- "Number of forward gears"
      }
      if (input$mtcars_var == "hp") {
        p <- plotly::plot_ly(mtcars, x = ~hp, color = ~hp, y = paste0(rownames(mtcars), " "), type = "bar")
        title_ <- "Gross horsepower"
      }
      p %>% 
        plotly::layout(title = title_,
                       xaxis = list(title = "", zeroline = FALSE), yaxis = list(showline = FALSE)) %>% 
        plotly::hide_colorbar() 
    })
    
    shiny::observeEvent(input$mtcars_var, {
      shiny::req(input$mtcars_var)
      shinyjs::addClass(selector = "[data-shinyclick = 'wt']", class = "opaque40")
      shinyjs::addClass(selector = "[data-shinyclick = 'mpg']", class = "opaque40")
      shinyjs::addClass(selector = "[data-shinyclick = 'cyl']", class = "opaque40")
      shinyjs::addClass(selector = "[data-shinyclick = 'gear']", class = "opaque40")
      shinyjs::addClass(selector = "[data-shinyclick = 'hp']", class = "opaque40")
      
      if (input$mtcars_var == "wt") {
        shinyjs::addClass(selector = ".svg_mtcars_rot", class = "svg_rot_0")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_72")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_144")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_216")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_288")
        shinyjs::removeClass(selector = "[data-shinyclick = 'wt']", class = "opaque40")
      }
      if (input$mtcars_var == "mpg") {
        shinyjs::addClass(selector = ".svg_mtcars_rot", class = "svg_rot_72")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_0")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_144")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_216")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_288")
        shinyjs::removeClass(selector = "[data-shinyclick = 'mpg']", class = "opaque40")
      }
      if (input$mtcars_var == "cyl") {
        shinyjs::addClass(selector = ".svg_mtcars_rot", class = "svg_rot_144")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_0")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_72")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_216")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_288")
        shinyjs::removeClass(selector = "[data-shinyclick = 'cyl']", class = "opaque40")
      }
      if (input$mtcars_var == "gear") {
        shinyjs::addClass(selector = ".svg_mtcars_rot", class = "svg_rot_216")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_0")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_72")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_144")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_288")
        shinyjs::removeClass(selector = "[data-shinyclick = 'gear']", class = "opaque40")
      }
      if (input$mtcars_var == "hp") {
        shinyjs::addClass(selector = ".svg_mtcars_rot", class = "svg_rot_288")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_0")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_72")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_144")
        shinyjs::removeClass(selector = ".svg_mtcars_rot", class = "svg_rot_216")
        shinyjs::removeClass(selector = "[data-shinyclick = 'hp']", class = "opaque40")
      }
    })
    
  })
}

