import("shiny")
import("shinyjs")
import("shiny.fluent")
import("glue")
import("purrr")
import("googlesheets4")

export("ui", "init_server")

ui <- function(id) {
  ns <- NS(id)
  reactOutput(ns("endModal"))
}

init_server <- function(id, ObjectsManager, LeaderboardManager, consts) {
  callModule(server, id, ObjectsManager, LeaderboardManager, consts)
}

server <- function(input, output, session, ObjectsManager, LeaderboardManager, consts) {
  ns <- session$ns
  
  # MODAL ----
  output$endModal <- renderReact({
    is_success <- session$userData$isSuccessModalOpen()
    is_failure <- session$userData$isBiteModalOpen()
    
    text <- ifelse(is_failure, consts$texts$gameOver, consts$texts$gameSuccess)
    icon_name <- ifelse(is_failure, "failure", "success")
    scores <- ObjectsManager$score_manager$get_scores(session$userData$level(), is_failure)
    
    reactWidget(
      Modal(
        className = "modal",
        isOpen = is_success || is_failure,
        isBlocking = FALSE,
        div(
          class = "end-grid",
          build_text(text),
          build_scores_table(scores),
          build_icon(icon_name, "modal-element--icon"),
          build_leaderboard(leaderboard(), session$userData$level(), scores$current, ask = is_success || is_failure),
          build_buttons()
        )
      )
    )
  })
  
  # LEADERBOARD ----
  leaderboard_trigger <- reactiveVal(0)
  is_submit_disabled <- reactiveVal(TRUE)
  is_submit_clicked <- reactiveVal(FALSE)
  
  leaderboard <- reactive({
    leaderboard_trigger()
    LeaderboardManager$get_leaderboard(session$userData$level())
  })
  
  output$leaderboardPartOne <- renderDataTable(
    leaderboard()[1:5,],
    options = list(
      dom = "t",
      searching = FALSE,
      ordering = FALSE
    )
  )
  output$leaderboardPartTwo <- renderDataTable(
    leaderboard()[6:10,],
    options = list(
      dom = "t",
      searching = FALSE,
      ordering = FALSE
    )
  )
  
  observeEvent(is_submit_disabled(), {
    if(is_submit_disabled()) {
      shinyjs::addClass(id = "submit", class = "hidden")
    } else {
      shinyjs::removeClass(id = "submit", class = "hidden")
    }
  })
  
  observeEvent(input$nick, {
    if(input$nick == "" || is_submit_clicked()) {
      shinyjs::addClass(id = "submit", class = "hidden")
      is_submit_disabled(TRUE)
    } else {
      is_submit_disabled(FALSE)
    }
  })
  
  # CONTENT ----
  build_scores_table <- function(scores_list) {
    div(
      class = "modal-element modal-element--scores",
      div(
        class = "scores-grid",
        div(
          class = "scores-current-text",
          consts$texts$currentScore
        ),
        div(
          class = "scores-high-text",
          consts$texts$highScore
        ),
        div(
          class = "scores-current-score",
          p(scores_list$current)
        ),
        build_level_score(scores_list, "easy"),
        build_level_score(scores_list, "medium"),
        build_level_score(scores_list, "hard")
      )
    )
  }
  
  build_text <- function(text) {
    div(
      div(Text(variant = "xLarge", text)),
      class = "modal-element modal-element--text"
    )
  }
  
  build_level_score <- function(scores_list, level) {
    div(
      class = glue("scores-{level}-score"),
      p(scores_list[[level]]),
      build_icon(level)
    )
  }
  
  build_icon <- function(type, class = "") {
    div(div(img(src = glue("./assets/{type}.png"))), class = paste("modal-element", class))
  }
  
  build_buttons <- function() {
    div(
      class = "buttons-grid",
      div(
        ShinyComponentWrapper(PrimaryButton(ns("playAgain"), text = "Play Again!")),
        class = "modal-element button-play"
      ),
      div(
        ShinyComponentWrapper(DefaultButton(ns("mainMenu"), text = "Back to Menu")),
        class = "modal-element button-menu"
      ),
      div(
        ShinyComponentWrapper(DefaultButton(ns("learnMore"), text = "Learn More")),
        class = "modal-element button-learn"
      ),
      div(uiOutput(ns("footer")), class = "buttons-footer")
    )
  }
  
  output$footer <- renderUI({
    HTML(consts$texts$modalFooter)
  })
  
  build_submit <- function(leaderboard, score) {
    if(score == "X" || (length(leaderboard$score) == 10 && as.numeric(score) < min(leaderboard$score))) {
      div(consts$texts$noHighScore)
    } else {
      div(
        ShinyComponentWrapper(TextField(ns("nick"), label = "Save your highscore to leaderboard")),
        ShinyComponentWrapper(PrimaryButton(ns("submit"), text = "Submit")),
        class = "modal-element button-nick"
      )
    }
  }
  
  build_leaderboard <- function(leaderboard, level, score, ask = TRUE) {
    if(ask) {
      div(
        class = "modal-element modal-element--leaderboard",
        div(
          class = "leaderboard-grid",
          build_submit(leaderboard, score),
          div(
            p(glue("Overall leaderboard for {level} level:")),
            build_icon(level, "image-small"),
            dataTableOutput(ns("leaderboardPartOne")),
            dataTableOutput(ns("leaderboardPartTwo")),
            class = "modal-element table-leaderboard"
          )
        )
      )
    } else {
      NULL
    }
  }
  
  
  # BUTTONS ----
  observeEvent(input$submit, {
    req(input$nick != "")
    LeaderboardManager$save_to_leaderboard(
      input$nick,
      session$userData$level(),
      as.numeric(ObjectsManager$score_manager$get_scores(session$userData$level())$current)
    )
    leaderboard_trigger(leaderboard_trigger() + 1)
    is_submit_disabled(TRUE)
    is_submit_clicked(TRUE)
  })
  
  observeEvent(input$learnMore, {
    purrr::walk(consts$links, ~open_in_new_tab(.x))
  })
  
  open_in_new_tab <- function(url) {
    shinyjs::runjs(glue("window.open('{url}', '_blank');"))
  }
  
  observeEvent(input$mainMenu, {
    session$userData$isBiteModalOpen(FALSE)
    session$userData$isSuccessModalOpen(FALSE)
    session$userData$isStartModalOpen(TRUE)
    is_submit_clicked(FALSE)
  })
  
  observeEvent(input$playAgain, {
    session$userData$isBiteModalOpen(FALSE)
    session$userData$isSuccessModalOpen(FALSE)
    ObjectsManager$place_objects(session$userData$level())
    is_submit_clicked(FALSE)
  })
  
}
