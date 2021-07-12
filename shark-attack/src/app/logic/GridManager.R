import("R6")
import("utils")
import("purrr")
import("shiny")
import("shinyjs")
import("glue")

export("GridManager")

GridManager <- R6::R6Class(
  "GridManager",
  private = list(
    number_of_columns = 20,
    number_of_rows = 10,
    
    single_grid_element = function(id) {
      div(
        class = "single-grid",
        id = id
      )
    },
    
    prepare_grid_element_id = function(id_col, id_row) {
      paste0(id_col, "-", id_row)
    },
    
    get_grid_element_location = function(grid_id) {
      strsplit(grid_id, "-") %>% unlist() %>% as.numeric()
    },
    
    # Crucial function for preparing starting grid elements: it is required to be sure that every object is placed,
    # thus if the one location is already occupied the function tries to find a good spot until success.
    random_grid_location = function(column_range, row_range, occupied_grids) {
      location <- private$prepare_grid_element_id(
        sample(column_range, 1),
        sample(row_range, 1)
      )
      
      if(location %in% occupied_grids) {
        private$random_grid_location(column_range, row_range, occupied_grids)
      } else {
        return(location)
      }
    },
    
    clean_grid = function(selector) {
      shinyjs::runjs(glue(
        "$('{selector}').css('background-image', 'none').removeClass('rotated').html('').attr('class', 'single-grid');"
      ))
    },
    
    clean_locations = function(locations) {
      purrr::walk(
        locations, 
        function(location) {
          private$clean_grid(glue("#{location}"))
        }
      )
    },
    
    create_grid = function() {
      # Columns go first, that is how grid is organized, column-wise.
      grid_elements_ids <- expand.grid(
        x = seq_len(private$number_of_columns),
        y = seq_len(private$number_of_rows)
      ) %>% {private$prepare_grid_element_id(id_col = .$x, id_row = .$y)}
      
      div(
        style = sprintf(
          "display: grid;
           grid-template-columns: repeat(%s, 1fr);
           grid-template-rows: repeat(%s, 1fr);",
          private$number_of_columns, private$number_of_rows
        ),
        class = "all-grid",
        lapply(grid_elements_ids, private$single_grid_element)
      )
    },
    
    rotate_element = function(location, direction) {
      if(direction == "left") {
        shinyjs::runjs(glue("$('#{location}').removeClass('rotated');"))
      } else if(direction == "right") {
        shinyjs::runjs(glue("$('#{location}').addClass('rotated');"))
      }
    }
  ),
  
  public = list(
    grid = NULL,
    initialize = function() {
      self$grid <- private$create_grid()
    }
  )
)
