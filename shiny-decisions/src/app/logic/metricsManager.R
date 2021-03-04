import("R6")
import("shiny")
import("glue")
import("shiny.grid")
import("shiny.blank")

export("metricsManager")

metricCard <- function(id, label, class, icon) {
  div(
    class = glue::glue("{class} metric-wrapper ui-element-style"),

    div(
      class = "metric-icon",
      style = glue::glue("background-image: url('{icon}')")
    ),
    tags$label(label),
    uiOutput(id, class = id)
  )
}

karma_ui <- function(id) {
  ns <- NS(id)

  gridPanel(
    class = "metric-karma",
    rows = "1fr 50vh 1fr",
    columns = "80px",
    areas = c(
      "...",
      "karma-container",
      "..."
    ),
    gridPanel(
      class = "karma-container",
      rows = "50px 1fr 50px",
      columns = "80px",
      areas = c(
        "karma-good",
        "karma-value",
        "karma-bad"
      ),

      div(
        class = "karma-good",
        style = "background-image: url('assets/ui/icons/halo.png')"
      ),
      div(
        class = "karma-bad",
        style = "background-image: url('assets/ui/icons/evil.png')"
      ),
      uiOutput(ns("stateKarma"), class = "karma-value")
    )
  )
}

metrics_ui <- function(id) {
  ns <- NS(id)

  gridPanel(
    rows = "80px",
    columns = "1fr 3fr 1fr 3fr 1fr 3fr 1fr",
    areas = c(
      "... metric-wealth ... metric-opinion ... metric-environment ..."
    ),
    class = "metrics",

    metricCard(
      ns("stateWealth"),
      "Wealth",
      "metric-wealth",
      "assets/ui/icons/gold.png"
      ),
    metricCard(
      ns("stateOpinion"),
      "Opinion",
      "metric-opinion",
      "assets/map/smile.png"
      ),
    metricCard(
      ns("stateEnvironment"),
      "Environment",
      "metric-environment",
      "assets/map/tree.png"
    )
  )
}

server <- function(input, output, session, state) {
  ns <- session$ns

  output$stateKarma <- renderUI(
    div(
      class = "karma-wrapper",
      progress(
        ns("stateKarmaNegative"),
        value = ifelse(state$karma < 50, (50 - state$karma) * 2, 0),
        type = "is-error negative"
      ),
      progress(
        ns("stateKarmaPositive"),
        value = ifelse(state$karma > 49, (state$karma - 50) * 2, 0),
        type = "is-primary positive"
      )
    )
  )
  output$stateWealth <- renderUI(
    progress(ns("stateWealth"), value = state$wealth, type = "is-wealth")
  )
  output$stateOpinion <- renderUI(
    progress(ns("stateOpinion"), value = state$opinion, type = "is-opinion")
  )
  output$stateEnvironment <- renderUI(
    progress(ns("stateEnvironment"), value = state$environment, type = "is-environment")
  )
}

# Manages the UI displaying the state metrics.
metricsManager <- R6Class("metricsManager",
  private = list(
    server = server
  ),

  public = list(
    karma_ui = karma_ui,
    metrics_ui = metrics_ui,
    init_server = function(id, state) {
      callModule(private$server, id, state)
    }
  )
)
