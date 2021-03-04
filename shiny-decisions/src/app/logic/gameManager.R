import("R6")
import("utils")
import("jsonlite")
import("glue")
import("shiny")
import("shiny.blank")
import("shiny.grid")

export("gameManager")

stateManager <- use("logic/stateManager.R")$stateManager
deckManager <- use("logic/deckManager.R")$deckManager
mapManager <- use("logic/mapManager.R")$mapManager
metricsManager <- use("logic/metricsManager.R")$metricsManager

dataManager <- use("logic/dataManager.R")$DataManager

ui_icon <- function(icon, link) {
  tags$a(
    href = link,
    target = "_blank",
    onclick = glue::glue("sendAnalyticsEvent({{category: 'social', action: '{icon}', label: '{icon}'}})"),
    tags$i(class = glue::glue("nes-icon is-large {icon}"))
  )
}

game_buttons <- function() {
  div(
    class = "navigation",
    lapply(
      list(
        list(id = "startGameEasy", text = "Easy Mode"),
        list(id = "startGameMedium", text = "Medium Mode"),
        list(id = "startGameHard", text = "Hard Mode")
      ),
      function(options) {
        button(
          options$id,
          options$text,
          actions = list(
            click = glue::glue("
              modal_gameOverScreen.classList.remove('open')
              modal_entryScreen.classList.remove('open');
              modal_attributionScreen.classList.remove('open');
              modal_projectDetailsScreen.classList.remove('open');
            ")
          )
        )
      }
    )
  )
}

# data related to game state
ui <- function() {
  tagList(
    div(
      id = "pause-game",
      style = "background-image: url(assets/ui/pause.png)"
    ),
    tags$script(glue::glue(
      '$( document ).ready(function() {{
        $("#pause-game").on("click", function(e){{
          modal_entryScreen.classList.toggle("open");
          modal_attributionScreen.classList.toggle("open");
          modal_projectDetailsScreen.classList.toggle("open");
        }});
      }});'
    )),
    modal(
      "attributionScreen",
      content = gridPanel(
        class = "entry-screen nes-container is-dark",

        gridPanel(
          id = "author-details",
          areas = c(
            "title  title title",
            "avatar name  name ",
            "avatar links links"
          ),
          rows = "25px 25px 65px",
          columns = "115px 110px 110px",

          div(class = "title", "About the author"),
          div(class = "name", "Pedro Silva"),
          div(class = "avatar", style = "background-image: url(assets/ui/author.png)"),
          div(
            class = "links",
            ui_icon("twitter", "https://twitter.com/sparktuga"),
            ui_icon("linkedin", "https://www.linkedin.com/in/pedrocoutinhosilva/"),
            ui_icon("github", "https://github.com/pedrocoutinhosilva"),
          )
        )
      ),
      open = TRUE,
      softClose = FALSE,
      closeButton = FALSE
    ),
    modal(
      "projectDetailsScreen",
      content = gridPanel(
        class = "entry-screen nes-container is-dark",

        gridPanel(
          id = "project-details",
          areas = c(
            "title",
            "... ",
            "links"
          ),
          rows = "25px 25px 65px",
          columns = "165px",

          div(class = "title", "Repository"),
          div(
            class = "links",
            ui_icon("github", "https://github.com/pedrocoutinhosilva/shiny.decisions"),
          )
        )
      ),
      open = TRUE,
      softClose = FALSE,
      closeButton = FALSE
    ),
    modal(
      "entryScreen",
      content = gridPanel(
        class = "entry-screen nes-container is-dark",
        areas = c("intro", "options", "navigation"),
        rows = "1fr 100px 100px",

        div(
          class = "options",
          checkbox("showTutorial", "Show tutorial at start", value = TRUE, class = "is-dark")
        ),
        div(
          class = "intro",
          p("Welcome to shiny decisions! A game about making the best of bad situations"),
          p("Try your best to lead your world in good (and hard) times and see how long you can keep it up!")
        ),
        game_buttons()
      ),
      open = TRUE,
      softClose = FALSE,
      closeButton = FALSE
    ),

    modal(
      "gameOverScreen",
      content = gridPanel(
        rows = "1fr 100px",
        areas = c("intro", "navigation"),
        class = "entry-screen",

        div(
          class = "intro",
          p(class = "title", "Game over"),
          p("You survived for:"),
          p(id = "game_over_message", "Week"),
          p("Would you like to go again?")
        ),
        div(
          class = "navigation",
          button(
            "restartGame",
            "Back to start",
            actions = list(
              click = glue::glue("
                modal_gameOverScreen.classList.remove('open');
                modal_entryScreen.classList.add('open');
                modal_attributionScreen.classList.add('open');
                modal_projectDetailsScreen.classList.add('open');
              ")
            )
          )
        )
      ),
      open = FALSE,
      softClose = FALSE,
      closeButton = FALSE
    )
  )
}

gameManager <- R6Class("gameManager",

  private = list(
    stateManager = NULL,
    deckManager = NULL,
    mapManager = NULL,
    metricsManager = NULL,
    dataManager = NULL,

    session = NULL,
    gameType = "Medium",

    resetState = function() {
      if (is.null(private$dataManager)) {
        private$dataManager <- dataManager$new(
          "1LwIPKAxbKvuGyMKktcTVuYZbTda0WMQxmNMhxqaQhGg"
        )
      }
      private$stateManager <- stateManager$new()
      private$metricsManager <- metricsManager$new()
      private$mapManager <- mapManager$new(private$stateManager, private$dataManager)
      private$deckManager <- deckManager$new(private$dataManager, private$stateManager)

      self$ui = list(
        gameStages = ui,
        metrics = private$metricsManager$metrics_ui,
        karma = private$metricsManager$karma_ui,
        map = private$mapManager$ui,
        cardStack = private$deckManager$ui
      )
    },

    triggerDeathPhase = function() {
      private$deckManager$triggerDeathPhase()
    }
  ),

  public = list(
    ui = NULL,
    init_server = function(session) {
      private$session <- session

      private$session$sendCustomMessage("init_card_stack", TRUE)
      private$metricsManager$init_server("metrics", private$stateManager$state)
      private$mapManager$init_server("map")
    },

    resetGame = function(gameType = private$gameType) {
      self$startGame(gameType, TRUE)
    },

    startGame = function(gameType, skipTutorial = FALSE) {
      private$session$sendCustomMessage( "clear_card_stack", TRUE)

      private$resetState()
      private$stateManager$resetState()
      private$deckManager$resetState(
        gameType,
        skipTutorial,
        private$dataManager,
        private$stateManager
      )
      private$mapManager$updateState(private$session)

      private$session$sendCustomMessage(
        "trackEvent",
        list(
          category = "Game state",
          action = "Game started",
          label = glue::glue("Game started, dificulty {gameType}")
        )
      )

      private$session$sendCustomMessage( "add_card", self$popCard())
    },

    popCard = function() {
      private$deckManager$popCard()
    },

    updateState = function(newState) {
      private$stateManager$updateState(newState)

      if(private$stateManager$isDeathState()) {
        private$triggerDeathPhase()
      }

      private$mapManager$updateState(private$session)
      card <- self$popCard()

      if (!is.null(card) && card == "GAMEOVER") {
        private$session$sendCustomMessage(
          "game_over",
          private$stateManager$state$week
        )
        private$session$sendCustomMessage(
          "trackEvent",
          list(
            category = "Game state",
            action = "Game end",
            label = glue::glue("Game ended, score was {private$stateManager$state$week}")
          )
        )
      } else {
        private$session$sendCustomMessage("add_card", card)
      }
    },

    initialize = function() {
      private$resetState()
    }
  )
)
