

# Module UI function
mod_download_files_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::uiOutput(ns("ui")),
    if (Sys.getenv("BROWSE") == "TRUE") { shiny::actionButton(ns("browse"), "Browse")}
    # svg_input_name <- "svginput"
    # svg_input_id   <- "shinyclick"
    # class_name     <- svg_input_name
  )
}

# Module server function
mod_download_files_server <- function(id, create_upload, svg_inputs) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$browse, { browser() })
    
    output$ui <- shiny::renderUI({
      ns <- session$ns
      
      if (!is.list(create_upload())) { return("Please upload or select an SVG first") }
      
      initial_name <- paste0("svg_", create_upload()$name)
      # initial_name <- paste(sample(as.character(unique(iris$Species)), 1),
      #                      sample(rownames(mtcars), 1)) %>% 
      #   stringr::str_replace_all(" ", "_") %>% 
      #   tolower()
      
      shiny::tagList(
        shiny::fluidRow(
          # style = "text-align:center;",
          column(
            width = 4, 
            shiny::textInput(ns("name"), "Input name (preferably unique)", value = initial_name),
            shiny::textInput(ns("data_element"), "Element data_<name> to collect with JS", value = "shinyclick"),
            shiny::hr(),
            shiny::p(style = "margin:10px 0 0;", "Script containing the input function"),
            shiny::downloadButton(ns("dwnl_r_function"), "R function"),
            shiny::p(style = "margin:10px 0 0;", "JS script to add to your apps 'www' directory"),
            shiny::downloadButton(ns("dwnl_js"), "JS"),
            shiny::p(style = "margin:10px 0 0;", "Simple Shiny app displaying only the SVG input and textOutput (optional)"),
            shiny::downloadButton(ns("dwnl_r_sample_app"), "Sample Shiny app")
          ),
          shiny::column(
            width = 8,
            shiny::tabsetPanel(
              id = "file_tabs",
              shiny::tabPanel(title = "R function", value = "create_r_function", shiny::uiOutput(ns("tab1"))),
              shiny::tabPanel(title = "JS", value = "create_js", shiny::uiOutput(ns("tab2"))),
              shiny::tabPanel(title = "R sample app", value = "create_r_sample_app", shiny::uiOutput(ns("tab3")))
            )
          )
        )
      )
    })
    
    
    temp_dir <- paste0("temp_", paste(letters[ceiling(26 * runif(10))], collapse = ""))
    dir.create(temp_dir)
    temp_dir_www <- file.path(temp_dir, "www")
    dir.create(temp_dir_www)
    shiny::onStop(function() { unlink(temp_dir, recursive = TRUE) })
    
    svg_final <- shiny::reactive({
      shiny::req(svg_inputs())
      
      svg_inputs() %>% 
        dplyr::mutate(line_mod = purrr::pmap_chr(list(line, shiny_value), function(line_, sv_) {
          if (!is.na(sv_)) { line_ <- stringr::str_replace(line_, " ", paste0(" data-shinyclick='", sv_, "' ")) }
          line_
        }))
    })
    
    rv_filenames <- shiny::reactiveValues(r_function_file   = "",
                                          js_file           = "",
                                          r_sample_app_file = "")
    
    # R function
    
    r_function_file <- shiny::reactive({
      shiny::req(svg_final())
      shiny::req(input$name)
      
      isolate({ if (rv_filenames$r_function_file != "") { unlink(rv_filenames$r_function_file) } })
      
      filename <- write_svg_input_r_lines(svg_lines      = svg_final()$line_mod, 
                                          svg_input_name = input$name,
                                          class_name     = input$name,
                                          include_css    = FALSE) %>% 
        write_svg_input_r_file(lines = ., svg_input_name = input$name, path = temp_dir)
      
      isolate({ rv_filenames$r_function_file <- filename })
      
      filename
    })
    
    output$tab1 <- shiny::renderUI({ shiny::pre(shiny::includeText(r_function_file())) })
    shiny::observe({ 
      shiny::req(r_function_file())
      r_function_file() 
    }) # In case the tab isn't opened
    
    output$dwnl_r_function <- shiny::downloadHandler(
      filename = function() { paste0(input$name, ".R") }, 
      content = function(file) { file.copy(r_function_file(), file) })
    
    # JS
    
    js_file <- shiny::reactive({
      shiny::req(input$name)
      
      isolate({ if (rv_filenames$js_file != "") { unlink(rv_filenames$js_file) } })
      
      filename <- write_js_binding_lines(svg_input_id = input$data_element, class_name = input$name) %>% 
        write_js_binding_file(svg_input_name = input$name, path = temp_dir_www)
      
      isolate({ rv_filenames$js_file <- filename })
      
      filename
    })
    
    output$tab2 <- shiny::renderUI({ shiny::pre(shiny::includeText(js_file())) })
    shiny::observeEvent(js_file(), { js_file() }) # In case the tab isn't opened
    
    output$dwnl_js <- shiny::downloadHandler(
      filename = function() { paste0(input$name, ".js") }, 
      content = function(file) { file.copy(js_file(), file) })
    
    # R sample app
    
    r_sample_app_file <- shiny::reactive({
      shiny::req(input$name)
      
      isolate({ if (rv_filenames$r_sample_app_file != "") { unlink(rv_filenames$r_sample_app_file) } })
      
      filename <- write_svg_input_shiny_lines(svg_input_name = input$name) %>% 
        write_svg_input_shiny_file(lines = ., svg_input_name = input$name, path = temp_dir)
      
      isolate({ rv_filenames$r_sample_app_file <- filename })
      
      filename
    })
    
    output$tab3 <- shiny::renderUI({ shiny::pre(shiny::includeText(r_sample_app_file())) })
    shiny::observeEvent(r_sample_app_file(), { r_sample_app_file() }) # In case the tab isn't opened
    
    output$dwnl_r_sample_app <- shiny::downloadHandler(
      filename = function() { paste0("app_", input$name, ".R") }, 
      content = function(file) { file.copy(r_sample_app_file(), file) })
    
  })
}