fluentPage(
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "assets/shark.png"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/sass.min.css"),
    tags$script(src = "js/move.js"),
    tags$script(src = "js/difficulty.js"),
    tags$script(src = "js/timer.js"),
    tags$script(src = "js/tutorial.js"),
    tags$script(src = "js/countdown.js")
  ),
  useShinyjs(),
  withReact(
    div(class = "countdown"),
    gameStart$ui("gameStart"),
    uiOutput("grid"),
    gameOver$ui("gameOver")
  )
)
