
mod_get_started_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::HTML('<h2 class="hh2" style="color:#565656;text-align:center;margin-bottom:25px;"><b>&nbsp;OVERVIEW&nbsp;</b></h2>'),
    shiny::div(
      style = "height:200px; width:100%; background-color:#cbd5e8;", class = "raisedbox",
      shiny::fluidRow(
        style = "color:#565656",
        shiny::column(
          width = 7, style = "padding:20px;padding-left:35px;",
          shiny::HTML("<h3>Make <b>Shiny inputs</b> from your <b>SVG images</b>!</h3>"),
          shiny::div(style = "height:10px;"),
          shiny::HTML("<h4>The <b>svgInput</b> tool will generate your <b>input function</b> and <b>Javascript Shiny binding</b> automatically.</h4>")),
        shiny::column(
          width = 2, style = "padding:30px;",
          shiny::div(style = "width:100px;margin:auto;text-align:center;", "Click image", svg_mtcars_03(ns("jumbo")))),
        shiny::column(
          width = 3, style = "padding-top:50px;",
          shiny::uiOutput(ns("jumbo_ui"))
        )
      )
    ),
    shiny::HTML('<h2 style="color:#565656;text-align:center;margin-bottom:25px;"><b>Create</b> SVG input in 3 easy steps!</h2>'),
    shiny::fluidRow(
      shiny::column(
        width = 4, 
        actionButton(
          ns("create_1_btn"), 
          shiny::tagList(
            shiny::h4("1. Upload your SVG file"),
            shiny::img(src = "get_started_upload3.png", width = "100%", style = "opacity:0.5;")),
          class = "raisedbox")
      ),
      shiny::column(
        width = 4, 
        actionButton(
          ns("create_2_btn"),
          shiny::tagList(
            shiny::h4("2. Set Shiny inputs"),
            shiny::img(src = "get_started_set_inputs4.png", width = "100%", style = "opacity:0.5;")),
          class = "raisedbox")
      ),
      shiny::column(
        width = 4, 
        actionButton(
          ns("create_3_btn"), 
          shiny::tagList(
            shiny::h4("3. Download Shiny-ready scripts"),
            shiny::img(src = "get_started_download3.png", width = "100%", style = "opacity:0.5;")),
          class = "raisedbox")
      )
    ),
    shiny::hr(),
    shiny::HTML('<h2 style="color:#565656;text-align:center;margin-bottom:25px;">Checkout the examples <b>gallery</b></h2>'),
    shiny::fluidRow(
      shiny::column(
        width = 4, 
        actionButton(
          ns("example_mtcars_btn"), 
          shiny::tagList(
            shiny::h4("mtcars"),
            shiny::img(src = "get_started_example_mtcars2.png", width = "100%", style = "opacity:0.5;")),
          class = "raisedbox")
      ),
      shiny::column(
        width = 4, 
        actionButton(
          ns("example_iris_btn"),
          shiny::tagList(
            shiny::h4("Iris"),
            shiny::img(src = "get_started_example_iris2.png", width = "100%", style = "opacity:0.5;")),
          class = "raisedbox")
      ),
      shiny::column(
        width = 4, 
        actionButton(
          ns("example_titanic_btn"), 
          shiny::tagList(
            shiny::h4("Titanic"),
            shiny::img(src = "get_started_example_titanic2.png", width = "100%", style = "opacity:0.5;")),
          class = "raisedbox")
      )
    ),
    if (Sys.getenv("BROWSE") == "TRUE") { shiny::actionButton(ns("browse"), "Browse")}
  )
}

mod_get_started_server <- function(id, parent, tabs) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$browse, { browser() })
    
    output$jumbo_ui <- shiny::renderUI({
      shiny::HTML(paste0("You selected<br><b>", input$jumbo, "</b>"))
    })
    
    rv_return <- shiny::reactiveVal("")
    
    shiny::observeEvent(input$example_mtcars_btn,  { rv_return(tabs[["mtcars"]]) })
    shiny::observeEvent(input$example_iris_btn,    { rv_return(tabs[["iris"]]) })
    shiny::observeEvent(input$example_titanic_btn, { rv_return(tabs[["titanic"]]) })
    
    shiny::observeEvent(input$create_1_btn,  { rv_return(tabs[["upload"]]) })
    shiny::observeEvent(input$create_2_btn,  { rv_return(tabs[["set"]]) })
    shiny::observeEvent(input$create_3_btn,  { rv_return(tabs[["download"]]) })
    
    shiny::reactive({ rv_return() })
  })
}

