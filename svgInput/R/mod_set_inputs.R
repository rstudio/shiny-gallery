

# Module UI function
mod_set_inputs_ui <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      # style = "text-align:center;",
      column(width = 6, 
             shiny::textOutput(ns("upload_fail_msg")),
             shiny::uiOutput(ns("controls")),
             shiny::actionButton(ns("btn_1"), "Next")
      ),
      shiny::column(
        width = 6,
        # shiny::div(
        #   style = "border: 1px dashed #898989;",
        shiny::uiOutput(ns("svg_highlight_ui")))
        # )
    ),
    if (Sys.getenv("BROWSE") == "TRUE") { shiny::actionButton(ns("browse"), "Browse")}
  )
}

# Module server function
mod_set_inputs_server <- function(id, create_upload, parent, tab) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$browse, { browser() })
    
    shiny::observeEvent(input$btn_1, { 
      shiny::updateTabsetPanel(parent, inputId = "wizard", selected = tab) 
    })
    
    svg_base <- shiny::reactive({ 
      if (is.list(create_upload())) {
        create_upload()$svg_base
      } else {
        NULL
      }
    })
    
    output$upload_fail_msg <- shiny::renderText({
      if (is.null(svg_base())) { 
        "Please upload or select an SVG first"
      } else {
        NULL
      }
    })
    
    rv_svg     <- reactiveValues(tibble = NULL)
    rv_svg_mod <- reactiveValues(tibble = NULL)
    
    shiny::observeEvent(svg_base(), {
      shiny::req(svg_base())
      
      ns <- session$ns
      
      svg_base_plus <- svg_base() %>% 
        dplyr::mutate(shiny_highlighted = FALSE) %>% 
        dplyr::mutate(shiny_highlight_input_id = paste0(shiny_class, "_hl")) %>% 
        dplyr::mutate(shiny_highlight_input = purrr::map2(shiny_highlight_input_id, line, function (id_, l_) {
          # shiny::checkboxInput(inputId = ns(sc_), label = stringr::str_sub(l_, 1, 16), value = FALSE)
          shinyWidgets::materialSwitch(
            inputId = ns(id_),
            label = NULL, # shiny::HTML("<i class='fa fa-eye'></i>"),
            value = FALSE, right = TRUE)
        })) %>% 
        dplyr::mutate(shiny_text_input_id = paste0(shiny_class, "_text")) %>% 
        dplyr::mutate(shiny_text_input = purrr::map(shiny_text_input_id, function(id_) {
          shiny::textInput(ns(id_), label = NULL, placeholder = "Set input value...")
        }))
      
      rv_svg$tibble     <- svg_base_plus 
      rv_svg_mod$tibble <- svg_base_plus
    })
    
    observers <- shiny::reactive({
      
      rv_svg_ <- dplyr::filter(rv_svg$tibble, shiny_lgl)
      
      obs_view <- purrr::map(rv_svg_$shiny_highlight_input_id, function(id_) {
        shiny::observeEvent(input[[id_]], {
          update <- rv_svg_mod$tibble
          update[update[["shiny_highlight_input_id"]] == id_, "shiny_highlighted"] <- input[[id_]]
          rv_svg_mod$tibble <- update
        })
      })
      
      obs_text <- purrr::map(rv_svg_$shiny_text_input_id, function(id_) {
        shiny::observeEvent({ input[[id_]] } , {
          if (!is.null(input[[id_]])) {
            update    <- rv_svg_mod$tibble
            if (input[[id_]] == "") {
              update[update[["shiny_text_input_id"]] == id_, "shiny_value"] <- NA
            } else {
              update[update[["shiny_text_input_id"]] == id_, "shiny_value"] <- input[[id_]]
            }
            rv_svg_mod$tibble <- update
          }
        })
      })
      
      c(obs_view, obs_text)
    })
    
    output$controls <- shiny::renderUI({
      shiny::req(rv_svg$tibble, observers)
      observers()
      
      shiny::isolate({
        
        shiny::req(rv_svg_mod$tibble)
        
        rv_svg_ <- dplyr::filter(rv_svg$tibble, shiny_lgl) 
        
        ui <- shiny::tagList(
          shiny::wellPanel(
            style = "overflow-y:scroll; max-height: 500px; background:white;",
            shiny::fluidRow(
              style = "font-style: italic; margin: 5px 0;",
              shiny::column(width = 5, style = "text-align:right;", "SVG element"),
              shiny::column(width = 4, style = "text-align:center;", "Shiny input value"),
              shiny::column(width = 3, style = "text-align:left; padding-left:20px;", shiny::HTML("<i class='fa fa-eye'></i>"))
            ),
            purrr::pmap(
              list(rv_svg_$line, rv_svg_$shiny_highlight_input, rv_svg_$shiny_text_input),
              function (li_, cbi_, ti_) {
                shiny::fluidRow(
                  shiny::column(
                    style = "font-weight: bold; text-align: right;",
                    width = 5, stringr::str_sub(li_, 1, 16)),
                  shiny::column(width = 4, ti_),
                  shiny::column(width = 3, cbi_)
                )
              }
            )
          )
        )
      })
      
      ui
    })
    
    output$svg_highlight_ui <- shiny::renderUI({
      shiny::req(rv_svg$tibble)
      shiny::isolate({
        
        shiny::req(rv_svg_mod$tibble)
        
        shiny::HTML(rv_svg_mod$tibble$shiny_line)
      })
    })
    
    shiny::observeEvent(rv_svg_mod$tibble, {
      
      rv_svg_mod_ <- rv_svg_mod$tibble
      
      if (any(rv_svg_mod_$shiny_highlighted)) {
        
        rv_svg_mod_ %>% 
          dplyr::filter(!shiny_highlighted) %>% 
          dplyr::pull(shiny_class) %>% 
          purrr::walk(function(sc_) {
            shinyjs::removeClass(selector = paste0(".", sc_), class = "opaque")
            shinyjs::removeClass(selector = paste0(".", sc_), class = "background")
          })
        
        rv_svg_mod_ %>% 
          dplyr::filter(shiny_highlighted) %>% 
          dplyr::pull(shiny_class) %>% 
          purrr::walk(function(sc_) {
            shinyjs::addClass(selector = paste0(".", sc_), class = "opaque")
            shinyjs::addClass(selector = paste0(".", sc_), class = "background")
          })
        
      } else {
        
        rv_svg_mod_$shiny_class %>% 
          purrr::walk(function(sc_) {
            shinyjs::removeClass(selector = paste0(".", sc_), class = "opaque")
            shinyjs::removeClass(selector = paste0(".", sc_), class = "background")
          })
        
      }
    })
    
    svg_inputs <- shiny::reactive({
      shiny::req(rv_svg_mod$tibble )
      rv_svg_mod$tibble 
    })
    
    return(svg_inputs)
  })    
}