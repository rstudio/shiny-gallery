import("R6")
import("utils")
import("jsonlite")
import("glue")
import("dplyr")
import("utils")
import("htmltools")

export("deckManager")

cleanCardMessage <- function(string) {
  stringr::str_replace_all(
    string,
    c("\\{" = "{`", "\\}" = "`}")
  )
}

ui <- function(inputId = "card_stack") {
  tagList(
    div(
      id = glue::glue("{inputId}_wrapper"),
      div(id = inputId)
    ),
    tags$script(src = "scripts/hammer.min.js"),
    tags$script(src = "scripts/card_stack.js")
  )
}

deckManager <- R6Class("deckManager",
  private = list(
    gameSettings = NULL,
    dataManager = NULL,
    stateManager = NULL,

    gameFlow = NULL,
    currentDeck = NULL,

    getCardType = function(pool, prob, saving_grace) {

      # Get random card type
      cardType <- sample(
        pool,
        size = 1,
        prob = prob,
        replace = TRUE
      )

      if (!(cardType %in% c("Tutorial", "Death"))) {
        # roll aditional checks based on karma
        # If karma is low, double roll for bad cards based on how low
        if (saving_grace < 50 && cardType != "Bad") {
          save_roll <- sample( c(1:1000), size = 1)
          # if the save roll fails, card type is automatically set to bad
          if (save_roll < 100 - saving_grace) {
            cardType <- "Bad"
          }
        }
        # If karma is high, double roll for good kards based on how high
        if (saving_grace > 50 && cardType == "Bad") {
          grace_roll <- sample( c(1:1000), size = 1)
          # if the grace roll passes, card type is automatically set to good
          if (grace_roll < saving_grace) {
            cardType <- "Good"
          }
        }
      }

      return(cardType)
    },

    generateTemplateCard = function() {
      deckOptions <- private$gameFlow[[private$currentDeck]]

      cardType <- private$getCardType(
        strsplit(deckOptions$`Card Pool`, ", ")[[1]],
        strsplit(deckOptions$`Pool Weight`, ", ")[[1]],
        private$stateManager$state$karma
      )

      if (private$gameFlow[[private$currentDeck]]$`Order Fixed`) {
        deckLimit       <- nrow(private$dataManager$getCards()[[cardType]])
        deckSize        <- as.numeric(private$gameFlow[[private$currentDeck]]$`Deck Size`)
        currentCardRow  <- deckLimit - deckSize
        cardTemplate    <- private$dataManager$getCards()[[cardType]][currentCardRow, ]
      } else {
        cardTemplate <- sample_n(private$dataManager$getCards()[[cardType]], 1)
      }

      state_min_intensity <- max(deckOptions$`Min Intensity`, cardTemplate$`Min Intensity`)
      state_max_intensity <- min(deckOptions$`Max Intensity`, cardTemplate$`Max Intensity`)

      if(state_min_intensity == state_max_intensity)
        state_max_intensity <- state_max_intensity + 1

      # Get random intensity level
      intensity_level <- sample(
        c(state_min_intensity:state_max_intensity),
        size = 1,
        prob = c(state_max_intensity:state_min_intensity),
        replace = TRUE
      )
      intensityMultiplier <- 1 + ((intensity_level - 1) / 20)

      # Generate random options
      options <- vector("list", length(names(private$dataManager$getOptions())))
      names(options) <- names(private$dataManager$getOptions())
      for ( option in names(options)){
          options[option] <- sample_n(private$dataManager$getOptions(option), 1)
      }

      options <- modifyList(options, list(
        `Danger Level` = private$dataManager$getOptions("Danger Level")[intensity_level, ],
        `Prosperity Level` = private$dataManager$getOptions("Prosperity Level")[intensity_level, ]
      ))

      color_scale <- private$dataManager$getOptions(
        as.character(glue::glue("{cardType} Color Scale"))
      )

      background_colors <- list(
        left = as.character(color_scale[((intensity_level * 2) - 1), ]),
        right = as.character(color_scale[(intensity_level * 2), ])
      )

      background_image <- ifelse (
        cardTemplate$`Image` == "random",
        paste0(sample(list.files('www/assets/cards'), 1)),
        glue::glue("{cardTemplate$`Image`}.png")
      )

      # Fills in any potential glue string options
      options <- as.list(sapply(names(options), function(option) {
          do.call(glue::glue, modifyList(list(cleanCardMessage(options[[option]])), options))
      }, USE.NAMES = TRUE))

      left_values = list(
        message = do.call(
          glue::glue,
          modifyList(list(cleanCardMessage(cardTemplate$`Left Message`)), options)
        ),
        delta = list(
          karma = cardTemplate$`Left Karma` * intensityMultiplier,
          wealth = cardTemplate$`Left Wealth` * intensityMultiplier,
          opinion = cardTemplate$`Left Opinion` * intensityMultiplier,
          environment = cardTemplate$`Left Environment` * intensityMultiplier
        )
      )
      right_values = list(
        message = do.call(
          glue::glue,
          modifyList(list(cleanCardMessage(cardTemplate$`Right Message`)), options)
        ),
        delta = list(
          karma = cardTemplate$`Right Karma` * intensityMultiplier,
          wealth = cardTemplate$`Right Wealth` * intensityMultiplier,
          opinion = cardTemplate$`Right Opinion` * intensityMultiplier,
          environment = cardTemplate$`Right Environment` * intensityMultiplier
        )
      )

      # To spice things up, randomly switches left and right values half of the time
      if (!(cardType %in% c("Tutorial", "Death"))) {
        if(sample( c(TRUE, FALSE), size = 1)) {
          temp <- left_values
          left_values <- right_values
          right_values <- temp
        }
      }

      card <- list(
        background = list(
          image = glue::glue("assets/cards/{background_image}"),
          color_left = background_colors$left,
          color_right = background_colors$right
        ),
        message = list (
          task = do.call(
            glue::glue,
            modifyList(list(cleanCardMessage(cardTemplate$`Template`)), options)
          ),
          left = left_values$message,
          right = right_values$message,
          week = switch(
            cardType,
            "Tutorial" = list(
              text = "Tutorial",
              increment = 0
            ),
            "Death" = list(
              text = "Afterlife",
              increment = 0
            ),
            list(
              text = paste0("Week ", private$stateManager$state$week),
              increment = 1
            )
          )
        ),
        delta = list(
          left = left_values$delta,
          right = right_values$delta
        )
      )

      return (card)
    }
  ),

  public = list(
    ui = ui,
    getCurrentDeck = function() { private$currentDeck },

    triggerDeathPhase = function() {
      private$currentDeck = "Death"
    },

    resetState = function(gameType = "Medium",
                          skipTutorial = FALSE,
                          dataManager,
                          stateManager) {
      private$dataManager <- dataManager
      private$stateManager <- stateManager

      private$gameSettings <- private$dataManager$getSettings(gameType)

      private$gameFlow <- list()
      specialDecks <- strsplit(private$gameSettings$`Special Decks`, ", ")[[1]]
      gameTypeDecks <- strsplit(private$gameSettings$`Fixed Decks`, ", ")[[1]]
      gameDeckSizes <- strsplit(private$gameSettings$`Deck Sizes`, ", ")[[1]]

      lapply(gameTypeDecks, function(deckName) {
        options <- private$dataManager$getDeckOptions(deckName)

        deckIndex <- which(gameTypeDecks == deckName)[1]
        nextDeckIndex <- deckIndex + 1
        if(nextDeckIndex > length(gameTypeDecks)) nextDeckIndex <- length(gameTypeDecks)
        options$`Next Deck` <- gameTypeDecks[[nextDeckIndex]]
        options$`Deck Size` <- gameDeckSizes[[deckIndex]]
        private$gameFlow[[deckName]] <- options
      })

      lapply(specialDecks, function(deckName) {
        options <- private$dataManager$getDeckOptions(deckName)

        options$`Next Deck` <- deckName
        options$`Deck Size` <- nrow(private$dataManager$getCards()[[deckName]])

        private$gameFlow[[deckName]] <- options
      })

      if(!skipTutorial) {
        private$currentDeck <- "Tutorial"
        private$gameFlow[["Tutorial"]]$`Next Deck` <- gameTypeDecks[1]
      } else {
        private$currentDeck <- gameTypeDecks[1]
      }
    },

    popCard = function() {
      if(private$gameFlow[[private$currentDeck]]$`Deck Size` == 0) {
        if(private$currentDeck == "Death") return("GAMEOVER")

        private$currentDeck <- private$gameFlow[[private$currentDeck]]$`Next Deck`
      }
      newSize <- as.numeric(private$gameFlow[[private$currentDeck]]$`Deck Size`) - 1
      private$gameFlow[[private$currentDeck]]$`Deck Size` <- newSize

      return(private$generateTemplateCard())
    },

    initialize = function(dataManager, stateManager) {
      self$resetState(
        dataManager = dataManager,
        stateManager = stateManager
      )
    }
  )
)
