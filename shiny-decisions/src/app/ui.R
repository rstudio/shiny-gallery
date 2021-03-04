blankPage(
  title = "Shiny Decisions",
  theme = "nes",

  tags$head(
    tags$head(includeHTML(("google-analytics.html"))),
    tags$script(src = "scripts/analytics-events.js"),
    tags$link(rel="shortcut icon", href="assets/map/tree.png"),
    tags$meta(name="apple-mobile-web-app-capable", content="yes"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles/sass.min.css")
  ),

  gameManager$ui$gameStages(),

  div(
    class = "app-map",
    gameManager$ui$map("map")
  ),
  gridPanel(
    id = "page-wrapper",
    rows = "100px 1fr 50px 150px",
    columns = "80px 1fr 1fr 1fr 1fr 1fr 1fr 80px",
    areas = c(
      "app-metrics  app-metrics app-metrics app-metrics app-metrics app-metrics app-metrics app-metrics",
      "app-karma    ...         ...         ...         app-cards   app-cards   app-cards   app-cards",
      "app-karma    ...         ...         app-week    app-week    ...         ...         ...",
      "...          ...         app-task    app-task    app-task    app-task    ...         ..."
    ),

    gridPanel(
      class = "app-metrics",
      gameManager$ui$metrics("metrics")
    ),

    gridPanel(
      class = "app-karma ui-element-style",
      gameManager$ui$karma("metrics")
    ),

    gridPanel(
      class = "app-cards",
      gameManager$ui$cardStack()
    ),

    div(
      id = "app_week",
      class = "app-week",
      p(class = "week-content")
    ),

    div(
      id = "card_stack_message",
      class = "app-task ui-element-style",
      p(class = "message-content")
    )
  )
)
