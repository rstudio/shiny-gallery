import("R6")
import("utils")
import("purrr")
import("shinyjs")
import("glue")

GridManager <- use("logic/GridManager.R")$GridManager

export("ObjectsManager")

ObjectsManager <- R6::R6Class(
  "ObjectsManager",
  inherit = GridManager,
  
  private = list(
    
    diver_current_side = "left",
    diver_start_location = "1-2",
    timer = "59",
    
    objects = list(
      diver = c(),
      shark = c(),
      boat = c(),
      plants = c()
    ),
    
    level = NULL,
    
    number_of_plants = 20, 
    number_of_sharks = list(
      easy = 4,
      medium = 8,
      hard = 12
    ),
    
    get_move_vector = function(direction) {
      switch (
        direction,
        up = c(0, -1),
        right = c(1, 0),
        down = c(0, 1),
        left = c(-1, 0)
      )
    },
    
    get_new_location = function(location, direction) {
      private$get_grid_element_location(location) + private$get_move_vector(direction)
    },
    
    # Function keep objects inside the grid.
    can_object_move = function(new_location) {
      if(isTRUE(any(new_location == 0) || new_location[1] > private$number_of_columns || new_location[2] > private$number_of_rows)) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    },
    
    # Function prevents objects overlapping.
    can_object_pass = function(object_name, new_location) {
      new_location_id <- private$prepare_grid_element_id(new_location[1], new_location[2]) 
      if(object_name == "shark") {
        # Starting location added to avoid biting before the player even moves, very annoying.
        if(new_location_id %in% setdiff(private$occupied_grids(private$objects), private$objects$diver) || new_location_id %in% self$trash_manager$occupied_trash(self$trash_manager$trash) || new_location_id == private$diver_start_location) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else if(object_name == "diver") {
        # Block diver movement on boat while no trash is collected, to avoid unintended game ending.
        if(new_location_id %in% private$objects$plants || (self$score_manager$get_scores(private$level)$current == 0 && new_location_id == private$objects$boat)) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else if(object_name == "boat") {
        if(new_location_id %in% c(private$occupied_grids(private$objects), self$trash_manager$occupied_trash(self$trash_manager$trash))) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      }
    },
    
    occupied_grids = function(objects_list) {
      objects_list %>% unlist() %>% unname()
    },
    
    get_image_name = function(object_name) {
      if(object_name == "shark") {
        return(private$level)
      } else if(object_name == "diver") {
        return(paste0("diver-", private$diver_current_side))
      } else {
        return(object_name)
      }
    },
    
    start_moving = function(countdown_time = 3) {
      timeout_time <- (countdown_time + 1) * 1000
      shinyjs::runjs(glue(
        "runCountdown({countdown_time});
        setTimeout(() => runTimer({private$timer}), {timeout_time});
        setTimeout(() => randomMove('shark', {private$number_of_sharks[[private$level]]}), {timeout_time});"
      ))
    }
  ),
  public = list(
    
    score_manager = NULL,
    trash_manager = NULL,
    
    #' Main function to place objects on grid element.
    #'
    #' @param object_name string; name of the object as listed on `objects`
    #' @param location string; grid unique id where to place
    #' @param image_name string; name of the png from www/assets, if different than object_name
    #' @param index numeric; for objects with multiple instances (e.g. sharks) index of object from that group
    #' @param extra_content string; HTML code that can be placed additionally on grid
    #'
    #' @return
    add_on_grid = function(object_name, location, image_name = object_name, index = 1, extra_content = NULL) {
      private$clean_locations(location)
      
      self$save_object_location(object_name, location, index)
        
      self$place_image_on_grid(location, object_name, image_name, extra_content)
    },
    
    save_object_location = function(object_name, location, index = 1) {
      private$objects[[object_name]][index] <- location
      self$trash_manager$objects <- private$objects # Need to keep updated objects available for trash to know where they can move.
    },
    
    place_image_on_grid = function(location, object_name, image_name = object_name, extra_content = NULL) {
      shinyjs::runjs(glue("$('#{location}').css('background-image', 'url(./assets/{image_name}.png)'); $('#{location}').addClass('{object_name}');"))
      if(!is.null(extra_content)) {
        shinyjs::runjs(glue("$('#{location}').append('{extra_content}');"))
      }
    },
    
    #' Function that locates all elements on the grid and triggers the new game.
    #'
    #' @param level string; what is the currently selected game level, save it on private list.
    place_objects = function(level) {
      self$clean_it_all()

      private$level <- level

      self$place_diver_and_boat()
      
      self$place_sharks()
      self$place_plants()
      
      self$trash_manager$place_trash()
      
      private$start_moving()
    },
    
    place_diver_and_boat = function() {
      self$add_on_grid(
        "diver",
        private$diver_start_location,
        image_name = private$get_image_name("diver"),
        extra_content = glue("<p class=timer>0:{private$timer}</p><p class=score>0</p>")
      )
      self$add_on_grid(
        "boat",
        private$prepare_grid_element_id(1, 1)
      )
    },
    
    place_sharks = function() {
      purrr::walk(
        seq_len(private$number_of_sharks[[private$level]]),
        function(index) {
          self$add_on_grid(
            "shark",
            private$random_grid_location(
              2:private$number_of_columns,
              2:private$number_of_rows,
              private$occupied_grids(private$objects)
            ),
            image_name = private$level,
            index = index
          )
        }
      )
    },
    
    place_plants = function() {
      purrr::walk(
        seq_len(private$number_of_plants),
        function(index) {
          self$add_on_grid(
            "plants",
            private$random_grid_location(
              1:private$number_of_columns,
              2:private$number_of_rows,
              private$occupied_grids(private$objects)
            ),
            index = index
          )
        }
      )
    },
    
    check_shark_bite = function() {
      if(private$objects$diver %in% private$objects$shark) {
        shinyjs::runjs("stopMove();")
        private$clean_locations(private$objects$diver)
        self$add_on_grid("shark", private$objects$shark, private$level)
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    
    check_success = function() {
      if(isTRUE(private$objects$diver == private$objects$boat)) {
        shinyjs::runjs("stopMove();")
        self$add_on_grid(
          "boat",
          private$objects$boat
        )
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    
    check_collect = function() {
      if(length(self$trash_manager$occupied_trash(self$trash_manager$trash)) > 0 && private$objects$diver %in% self$trash_manager$occupied_trash(self$trash_manager$trash)) {
        trash_collected <- names(self$trash_manager$trash)[purrr::map_lgl(self$trash_manager$trash, ~private$objects$diver %in% .x)]
        self$score_manager$update_score(self$trash_manager$trash_points, trash_collected, private$level)
        self$trash_manager$remove_trash(trash_collected,  private$objects$diver)
      }
    },
    
    
    move_object = function(object_name, direction, index = 1, extra_content = NULL) {
      location <- private$objects[[object_name]][index]
      new_location <- private$get_new_location(location, direction)
      # Objects can only move if the new locations is valid i.e. still on grid and not overlapping other objects/trash.
      # If the object cannot move that it "waits" and try to move next "round" in the new random direction.
      if(private$can_object_move(new_location) && private$can_object_pass(object_name, new_location)) {
        private$clean_locations(location)
        
        new_location_id <- private$prepare_grid_element_id(new_location[1], new_location[2])
        
        self$add_on_grid(
          object_name, new_location_id,
          image_name = private$get_image_name(object_name),
          index = index,
          extra_content = extra_content
        )
        if(object_name != "diver") {
          # Diver cannot use the "rotated" class as it would rotate the score and timer texts on this grid as well.
          # There are methods in css to rotate only background image, but here it was decided to just use rotated image, see `set_diver_side` function.
          private$rotate_element(new_location_id, direction)
        } else {
          current_points <- self$score_manager$get_scores(private$level)$current
          shinyjs::runjs(glue("$('.diver').append('<p class=score>{current_points}</p>');"))
        }
      }
    },
    
    set_diver_side = function(direction) {
      if(direction %in% c("left", "right")) {
        private$diver_current_side <- direction
      }
    },
    
    clean_it_all = function() {
      private$clean_grid(".single-grid")
      shinyjs::runjs("stopMove();")
      
      purrr::walk(names(private$objects), function(object_name) private$objects[[object_name]] <- c())
      purrr::walk(names(self$trash_manager$trash), function(trash_name) self$trash_manager$trash[[trash_name]] <- c())
      
      self$score_manager$reset_scores()
    },
    
    initialize = function(score_manager, trash_manager) {
      self$score_manager <- score_manager
      self$trash_manager <- trash_manager
      invisible(self)
    }
  )
)
