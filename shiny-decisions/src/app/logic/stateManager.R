import("R6")
import("shiny")

export("stateManager")

# Manages information about the current state of the game
stateManager <- R6Class("stateManager",
  private = list(
    default_state = list(
      karma = 60,
      wealth = 50,
      opinion = 50,
      environment = 50,
      week = 1
    )
  ),
  public = list(
    # Metrics for game state
    state = reactiveValues(
      karma = 50,
      wealth = 0,
      opinion = 0,
      environment = 0,
      week = 0
    ),
    # Markers currently in the map
    markers = list(),

    # Allows reseting the state of the manager.
    resetState = function(state = private$default_state) {
      isolate(self$updateState(state, TRUE))
      self$markers = list()
    },

    # Checks for death states
    isDeathState = function() {
      if(self$state$wealth < 1 ||
         self$state$opinion < 1 ||
         self$state$environment < 1
      ) {
        return(TRUE)
      }

      return(FALSE)
    },

    # If force is false, newState contains the delta values to update
    # If force is true, newState contains the new values for the state attribute
    updateState = function(newState, force = FALSE) {
      lapply(names(newState), function(attribute) {
          self$state[[attribute]] <- ifelse (
            force,
            newState[[attribute]],
            self$state[[attribute]] + as.numeric(newState[[attribute]])
          )

          # Ignore week limits
          if(attribute != "week") {
            if(self$state[[attribute]] < 0)
              self$state[[attribute]] <- 0
            if(self$state[[attribute]] > 100)
              self$state[[attribute]] <- 100
          }
      })
    }
  )
)
