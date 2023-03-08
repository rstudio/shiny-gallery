library(shiny)
library(shinyjs)
library(magrittr)

# Sys.setenv(BROWSE = "TRUE")
Sys.setenv(BROWSE = "FAKE NEWS")

source("R/code_files.R", local = TRUE)

source("R/mod_tutorial.R", local = TRUE)
source("R/mod_create_svg_input.R", local = TRUE)
source("R/mod_upload_svg.R", local = TRUE)
source("R/mod_set_inputs.R", local = TRUE)
source("R/mod_download_files.R", local = TRUE)
source("R/mod_header.R", local = TRUE)

source("R/mod_get_started.R", local = TRUE)

source("R/mod_example_header.R", local = TRUE)
source("R/mod_example_titanic.R", local = TRUE)
source("R/mod_example_iris.R", local = TRUE)
source("R/mod_example_mtcars.R", local = TRUE)

source("R/svg_iris.R", local = TRUE)
source("R/svg_mtcars_02.R", local = TRUE)
source("R/svg_mtcars_03.R", local = TRUE)

source("R/svg_header_07.R", local = TRUE)

data("Titanic")
titanic_class <- c("Crew" = "crew", "1st" = "first", "2nd" = "second" , "3rd" = "third")
titanic <- dplyr::as_tibble(Titanic) %>% 
  dplyr::mutate(class_input = titanic_class[Class])

ui <- fluidPage(  
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  shiny::fluidRow(
    shiny::column(
      width = 2, 
      shiny::div(style = "padding-top:50px;margin:auto;max-width:300px;", 
                 svg_header_07("menu"))
    ),
    shiny::column(
      width = 10, 
      shiny::tabsetPanel(
        id = "wizard", type = "hidden",
        shiny::tabPanelBody(value = "panel_hex_11"),
        shiny::tabPanelBody(value = "panel_hex_21", mod_get_started_ui("gs")),
        shiny::tabPanelBody(value = "panel_hex_13"),
        shiny::tabPanelBody(value = "panel_hex_22", mod_example_header_ui("ei_h", "Select Iris sepals or petals"), mod_example_iris_ui("ei")),
        # shiny::tabPanelBody(value = "panel_hex_23", mod_example_header_ui("e2_h", "Select")),
        shiny::tabPanelBody(value = "panel_hex_31", mod_example_header_ui("et_h", "Select Titanic class"), mod_example_titanic_ui("et")),
        shiny::tabPanelBody(value = "panel_hex_32", mod_example_header_ui("em_h", "Select mtcars variable"), mod_example_mtcars_ui("em")),
        # shiny::tabPanelBody(value = "panel_hex_41", mod_example_header_ui("e1_h", "Select")),
        shiny::tabPanelBody(value = "panel_hex_15"),
        shiny::tabPanelBody(
          value = "panel_hex_24", 
          mod_header_ui("c1", "CREATE", "1. Upload SVG", left = FALSE), 
          mod_upload_svg_ui("up")),
        shiny::tabPanelBody(
          value = "panel_hex_33", 
          mod_header_ui("c2", "CREATE", "2. Set inputs"),
          mod_set_inputs_ui("si")),
        shiny::tabPanelBody(
          value = "panel_hex_43", 
          mod_header_ui("c3", "CREATE", "3. Download files", right = FALSE),
          mod_download_files_ui("df"))
      )
    )
  ),
  if (Sys.getenv("BROWSE") == "TRUE") { shiny::actionButton("browse", "Browse")}
)

server <- function(input, output, session) {
  
  shiny::observeEvent(input$browse, { browser() })
  
  rv_menu_hex <- shiny::reactiveVal("hex_11")
  
  shiny::observeEvent(input$menu, { rv_menu_hex(input$menu) })
  
  menu_hex <- shiny::reactive({ rv_menu_hex() })
  
  menu_group <- shiny::reactive({
    shiny::req(menu_hex)
    if (menu_hex() %in% c("hex_11", "hex_21")) { "get_started" } 
    else if (menu_hex() %in% c("hex_13", "hex_22", "hex_23", "hex_31", "hex_32", "hex_41")) { "gallery" } 
    else if (menu_hex() %in% c("hex_15", "hex_24", "hex_33", "hex_43")) { "create" }
  })
  
  shiny::observeEvent(menu_hex(), { 
    menu_hex_ <- menu_hex()
    panel_    <- stringr::str_sub(input$wizard, 7, -1)
    if (menu_hex_ == "hex_11") { menu_hex_ <- "hex_21"}
    if (menu_hex_ == "hex_13") { menu_hex_ <- "hex_22"}
    if (menu_hex_ == "hex_15") { menu_hex_ <- "hex_24"}
    # If panel is not in create yet start at hex_24
    if (menu_hex_ %in% c("hex_43", "hex_33") & !panel_ %in% c("hex_15", "hex_24", "hex_33", "hex_43")) { menu_hex_ <- "hex_24" }
    shiny::updateTabsetPanel(session, inputId = "wizard", selected = paste0("panel_", menu_hex_)) 
  })
  
  shiny::observeEvent(menu_group(), {
    purrr::walk(
      c("hex_11", "hex_21", "hex_13", "hex_22", "hex_23", "hex_31", "hex_32", "hex_41", "hex_15", "hex_24", "hex_33", "hex_43"),
      ~ shinyjs::addClass(selector = paste0("[data-shinyclick = '", ., "']"), class = "opaque_menu"))
    if (menu_group() == "get_started") {
      purrr::walk(
        c("hex_11", "hex_21"),
        ~ shinyjs::removeClass(selector = paste0("[data-shinyclick = '", ., "']"), class = "opaque_menu"))
    } else if (menu_group() == "gallery") {
      purrr::walk(
        c("hex_13", "hex_22", "hex_23", "hex_31", "hex_32", "hex_41"),
        ~ shinyjs::removeClass(selector = paste0("[data-shinyclick = '", ., "']"), class = "opaque_menu"))
    } else if (menu_group() == "create") {
      purrr::walk(
        c("hex_15", "hex_24", "hex_33", "hex_43"),
        ~ shinyjs::removeClass(selector = paste0("[data-shinyclick = '", ., "']"), class = "opaque_menu"))
    }
  })
  
  get_started_click <- mod_get_started_server(
    "gs", 
    parent = session, 
    tabs = c("titanic" = "hex_31", "iris" = "hex_22", "mtcars"   = "hex_32",
             "upload"  = "hex_24", "set"  = "hex_33", "download" = "hex_43"))
  
  shiny::observeEvent(get_started_click(), { if (get_started_click() != "") { rv_menu_hex(get_started_click()) } })
  
  # # panel_hex_41
  # mod_example_header_server("e1_h", parent = session, prev_tab = "panel_hex_23", next_tab = "panel_hex_31")
  
  mod_example_titanic_server("et") # panel_hex_31
  mod_example_header_server("et_h", parent = session, prev_tab = "panel_hex_32", next_tab = "panel_hex_22")
  
  mod_example_iris_server("ei")    # panel_hex_22
  mod_example_header_server("ei_h", parent = session, prev_tab = "panel_hex_31", next_tab = "panel_hex_32")
  
  mod_example_mtcars_server("em")  # panel_hex_32
  mod_example_header_server("em_h", parent = session, prev_tab = "panel_hex_22", next_tab = "panel_hex_31")
  
  # # panel_hex_23
  # mod_example_header_server("e2_h", parent = session, prev_tab = "panel_hex_32", next_tab = "panel_hex_41")
  
  mod_header_server("c1", parent = session, prev_tab = "",             next_tab = "panel_hex_33")
  mod_header_server("c2", parent = session, prev_tab = "panel_hex_24", next_tab = "panel_hex_43")
  mod_header_server("c3", parent = session, prev_tab = "panel_hex_33", next_tab = "")
  
  create_upload <- mod_upload_svg_server("up", parent = session, tab = "panel_hex_33")
  svg_inputs    <- mod_set_inputs_server("si", create_upload, parent = session, tab = "panel_hex_43")
  mod_download_files_server("df", create_upload, svg_inputs)
  
}

shinyApp(ui, server)
