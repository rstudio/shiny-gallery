import("R6")
import("googlesheets")
import("tidyr")
import("glue")

export("DataManager")

# Deals with external data
DataManager <- R6Class("DataManager",
  private = list(
    data = NULL,
    cities = NULL,
    options = NULL,
    settings = NULL,

    cards = list(),
    decks = list(),

    cardTypes = c("Tutorial", "Bad", "Good", "Special", "Death")
  ),

  public = list(
    initialize = function(sheetId) {
      sheet_metadata <- googlesheets::gs_url(
          glue::glue("https://docs.google.com/spreadsheets/d/{sheetId}"),
          visibility = "public",
          lookup = FALSE
      )

      print("Updating from online data")
      gs_download(sheet_metadata, to = "data/options.xlsx", overwrite = TRUE)

      private$settings  <- readxl::read_xlsx("data/options.xlsx", "Game Settings")
      private$decks     <- readxl::read_xlsx("data/options.xlsx", "Decks")
      private$options   <- readxl::read_xlsx("data/options.xlsx", "Options")
      private$cities    <- readxl::read_xlsx("data/options.xlsx", "Map Cities")

      lapply(private$cardTypes, function(type) {
        private$cards[[type]] <- readxl::read_xlsx("data/options.xlsx", sheet = type)
      })
    },

    getCities = function() {
      return(private$cities)
    },

    getSettings = function(gameType) {
      return(private$settings[which(private$settings$`Game Type` == gameType), ])
    },

    getOptions = function(attribute) {
      return(drop_na(private$options[attribute]))
    },

    getDeckOptions = function(name) {
      decks <- private$decks
      return(decks[which(decks$`Deck Name` == name), ])
    },

    getCards = function() {
      return(private$cards)
    }
  )
)
