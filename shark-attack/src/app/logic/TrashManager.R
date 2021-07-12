import("R6")
import("utils")
import("purrr")
import("shiny")
import("shinyjs")
import("glue")

export("TrashManager")

ObjectsManager <- use("logic/ObjectsManager.R")$ObjectsManager

TrashManager <- R6::R6Class(
  "TrashManager",
  inherit = ObjectsManager,
  
  private = list(
    initial_trash_count = 15,
    new_trash_count = 1,
    
    can_trash_pass = function(new_location) {
      new_location_id <- private$prepare_grid_element_id(new_location[1], new_location[2])
      if(new_location_id %in% c(private$occupied_grids(self$objects), self$occupied_trash(self$trash))) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    },
    
    random_trash = function() {
      sample(names(self$trash_chances), 1, prob = unlist(self$trash_chances))
    }
  ),
  
  public = list(
    trash = NULL,
    trash_chances = NULL,
    trash_points = NULL,
    
    objects = NULL,
    
    occupied_trash = function(trash_list) {
      trash_list %>% unlist() %>% unname()
    },
    
    place_trash = function(count = private$initial_trash_count, col_range = 1:private$number_of_columns, row_range = 1:private$number_of_rows) {
      purrr::walk(
        seq_len(count),
        function(index) {
          trash_name <- private$random_trash()
          self$add_trash_on_grid(
            trash_name,
            private$random_grid_location(
              col_range,
              row_range,
              c(private$occupied_grids(self$objects), self$occupied_trash(self$trash))
            ),
            index = length(self$trash[[trash_name]]) + 1
          )
        }
      )
    },
    
    # Trash goes with the water flow from right to left and from surface to the seafloor.
    # All trash moves together, but each in it's own independent direction.
    move_all_trash = function() {
      purrr::walk(
        names(self$trash),
        function(trash_name) {
          iwalk(self$trash[[trash_name]], ~self$move_trash(trash_name, .x, sample(c("left", "down"), 1), .y))
        }
      )
      # New trashes appear on two locations: last column and first row.
      self$place_trash(private$new_trash_count, c(private$number_of_columns, private$number_of_columns), 1:private$number_of_rows)
      self$place_trash(private$new_trash_count, 1:private$number_of_columns, c(1, 1))
    },
    
    remove_trash = function(trash_name, location) {
      self$trash[[trash_name]] <- setdiff(self$trash[[trash_name]], location)
    },
    
    add_trash_on_grid = function(trash_name, location, index = 1) {
      private$clean_locations(location)
      self$trash[[trash_name]][index] <- location
      
      self$place_image_on_grid(location, trash_name)
    },
    
    move_trash = function(trash_name, location, direction, index = 1) {
      new_location <- private$get_new_location(location, direction)
      if(private$can_object_move(new_location)) {
        if(private$can_trash_pass(new_location)) {
          private$clean_locations(location)
          new_location_id <- private$prepare_grid_element_id(new_location[1], new_location[2])
          
          self$add_trash_on_grid(
            trash_name,
            new_location_id,
            index = index
          )
        }
      } else {
        # Once trash moves outside the grid it is considered as not collected and lost in the oceans depth.
        private$clean_locations(location)
        self$remove_trash(trash_name, location)
      }
    },
    
    initialize = function() {
      self$trash <- list(
        organic = c(),
        glass = c(),
        metal = c(),
        electro = c(),
        plastic = c(),
        radio = c()
      )
      self$trash_chances <- list(
        organic = 0.1,
        glass = 0.1,
        metal = 0.15,
        electro = 0.2,
        plastic = 0.4,
        radio = 0.05
      )
      self$trash_points <- list(
        organic = 1,
        glass = 3,
        metal = 3,
        electro = 7,
        plastic = 5,
        radio = 15
      )
      invisible(self)
    }
  )
)
