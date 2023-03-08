
mod_example_titanic_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::div(
      style = "max-width:400px; width:100%; margin: 0 auto;",
      shiny::div(class = "titanic_div", style = "margin-bottom:-5px;", shiny::HTML(readLines("svg_samples/titanic01_smog.svg"))),
      svg_titanic(ns("titanic"))),
    shiny::uiOutput(ns("titanic_plot_title")),
    shiny::plotOutput(ns("titanic_plot"), height = "200px"),
    if (Sys.getenv("BROWSE") == "TRUE") { shiny::actionButton(ns("browse"), "Browse")}
  )
}

mod_example_titanic_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$browse, { browser() })
    
    rv_invalidate <- shiny::reactiveVal(TRUE)
    
    shiny::observe({
      shiny::req(input$titanic)
      # Use reactivevalue and invalidatelater instead of Sys.sleep to free up the R session
      if (isolate(rv_invalidate())) {
        shinyjs::addClass(selector = ".titanic_div", class = "svg_titanic_go")
        shiny::invalidateLater(800, session)
        isolate(rv_invalidate(FALSE))
      } else {
        shinyjs::removeClass(selector = ".titanic_div", class = "svg_titanic_go")
        isolate(rv_invalidate(TRUE))
      }
      
      
    })
    
    class_titanic <- shiny::reactive({
      shiny::req(input$titanic)
      names(titanic_class[titanic_class == input$titanic])
    })
    
    output$titanic_plot_title <- shiny::renderUI({
      shiny::req(class_titanic())
      shiny::HTML(paste0(
        "<h4>Survival chart of <span style='color:#4682b4;font-weight:bold;'>", class_titanic(),
        "</span> versus <span style='color:#898989;font-weight:bold;'>other</span> classes</h4>"))
    })
    
    output$titanic_plot <- shiny::renderPlot({
      shiny::req(class_titanic())
      
      class_ <- class_titanic()
      
      fills <- c("#898989", "#4682b4")
      names(fills) <- c("other", class_)
      
      titanic %>% 
        dplyr::mutate(`Highlighted class` = dplyr::if_else(class_input == input$titanic, Class, "other")) %>% 
        ggplot2::ggplot(ggplot2::aes(x = Survived, y = n, fill = `Highlighted class`)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::facet_grid(. ~ Age+Sex) + 
        ggplot2::theme_minimal() + 
        ggplot2::labs(y = "Number of people") + 
        ggplot2::scale_fill_manual(values = fills, guide = FALSE) 
    })
    
  })
}

