

# Module UI function
mod_upload_svg_ui <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      # style = "text-align:center;",
      column(
        width = 6, 
        shiny::h4("Upload SVG file"),
        shiny::fileInput(ns("file"), NULL, accept = ".svg"),
        shiny::h4("Or select sample SVG"),
        shiny::selectizeInput(
          ns("file_select"), "", 
          choices = c("none", "mtcars", "iris", "titanic", "rocket"),
          options = list(placeholder = 'select an SVG')),
        shiny::actionButton(ns("btn_1"), "Next")),
      shiny::column(
        width = 6,
        # shiny::div(
        #   style = "border: 1px dashed #898989;",
        uiOutput(ns("svg_ui"))
        # )
      )
    ),
    if (Sys.getenv("BROWSE") == "TRUE") { shiny::actionButton(ns("browse"), "Browse")}
  )
}

# Module server function
mod_upload_svg_server <- function(id, parent, tab) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$browse, { browser() })
    
    rv_svg_file <- shiny::reactiveValues(filename = NULL, name = NULL)
    
    shiny::observeEvent(input$btn_1, { 
      shiny::updateTabsetPanel(parent, inputId = "wizard", selected = tab) 
    })
    
    observeEvent(input$file, { 
      rv_svg_file$filename <- input$file$datapath 
      rv_svg_file$name     <- paste0("svg_", stringr::str_sub(input$file$name, 1, -5))
    })
    
    observeEvent(input$file_select, { 
      if (input$file_select == "mtcars") { 
        rv_svg_file$filename <- "svg_samples/mtcars_02.svg" 
        rv_svg_file$name     <- "mtcars"
      }
      if (input$file_select == "iris") { 
        rv_svg_file$filename <- "svg_samples/iris01.svg" 
        rv_svg_file$name     <- "iris"
      }
      if (input$file_select == "titanic") { 
        rv_svg_file$filename <- "svg_samples/titanic01.svg" 
        rv_svg_file$name     <- "titanic"
      }
      if (input$file_select == "rocket") { 
        rv_svg_file$filename <- "svg_samples/rocket.svg" 
        rv_svg_file$name     <- "rocket"
      }
    })
    
    svg_lines <- shiny::reactive({ 
      shiny::req(rv_svg_file$filename)
      readLines(rv_svg_file$filename) 
    })
    
    svg_base  <- shiny::reactive({ 
      shiny::req(svg_lines())
      svg_lines_to_tibble(svg_lines())
    })
    
    output$svg_ui <- shiny::renderUI({
      shiny::req(svg_base)
      shiny::HTML(svg_base()$line)
    })
    
    # return(svg_base)
    shiny::reactive({ 
      if (is.null(rv_svg_file$filename)) {
        FALSE
      } else { 
        list(svg_base = svg_base(),
             name     = rv_svg_file$name)
      }
    })
  })    
}