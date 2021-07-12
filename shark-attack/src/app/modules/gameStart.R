import("shiny")
import("shinyjs")
import("shiny.fluent")
import("glue")

export("ui", "init_server")

ui <- function(id) {
  ns <- NS(id)
  reactOutput(ns("startModal"))
}

init_server <- function(id, ObjectsManager, consts) {
  callModule(server, id, ObjectsManager, consts)
}

server <- function(input, output, session, ObjectsManager, consts) {
  ns <- session$ns
  
  output$startModal <- renderReact({
    shinyjs::runjs("setLevelClick();")
    reactWidget(
      Modal(
        className = "modal",
        isOpen = session$userData$isStartModalOpen(), isBlocking = FALSE,
        div(
          class = "start-grid",
          div(
            div(ShinyComponentWrapper(IconButton(ns("step_left"), iconProps = list(iconName = "ChevronLeft"))), class = "arrow arrow--left"),
            div(ShinyComponentWrapper(SwatchColorPicker(ns("tutorial_step"), "1", colorCells = tutorial_steps_config, columnCount = length(tutorial_steps_config))), class = "steps"),
            div(class = "image", img(src = "./assets/tutorial1.png")),
            div(class = "text", h4(consts$tutorial[[1]])),
            div(ShinyComponentWrapper(IconButton(ns("step_right"), iconProps = list(iconName = "ChevronRight"))), class = "arrow arrow--right"),
            class = "start-element start-element--tutorial"
          ),
          div(
            class = "start-element start-element--confirm",
            ShinyComponentWrapper(PrimaryButton(ns("startGame"), text = "I understand, let's clean!"))
          ),
          span(
            class = "start-element--levels",
            div(class = "level-icon--text", h4("Select difficulty to start the game")),
            div(class = "level-icon--easy", level_icon("easy")),
            div(class = "level-icon--medium", level_icon("medium")),
            div(class = "level-icon--hard", level_icon("hard"))
          )
        )
      )
    )
  })
  
  level_icon <- function(type) {
    div(img(src = glue("./assets/{type}.png")), class = "start-element start-element--icon", id = glue("level-{type}"))#, onClick = JS(glue("() => setLevel('{type}');")))
  }
  
  tutorial_steps_config <- lapply(seq_along(consts$tutorial), function(index) {
    list(id = as.character(index), color = consts$colors$navy)
  })
  
  observeEvent(input$tutorial_step, {
    text <- consts$tutorial[[input$tutorial_step]]
    shinyjs::runjs(glue("updateTutorial('{input$tutorial_step}', '{text}');"))
  })
  
  observeEvent(input$step_left, {
    step <- as.numeric(input$tutorial_step)
    if(step > 1) {
      updateSwatchColorPicker(session, "tutorial_step", as.character(step - 1))
    }
  })
  
  observeEvent(input$step_right, {
    step <- as.numeric(input$tutorial_step)
    if(step < length(consts$tutorial)) {
      updateSwatchColorPicker(session, "tutorial_step", as.character(step + 1))
    }
  })
  
  observeEvent(input$startGame, {
    shinyjs::runjs("$('.start-element--confirm').css('visibility', 'hidden'); $('.start-element--levels').css('visibility', 'visible');")
  })
}
