server <- function(input, output, session) {

  # PREPARE GRID ----
  GridManager <- GridManager$new()
  ScoreManager <- ScoreManager$new()
  TrashManager <- TrashManager$new()
  ObjectsManager <- ObjectsManager$new(ScoreManager, TrashManager)
  LeaderboardManager <- LeaderboardManager$new()
  
  output$grid <- renderUI({
    GridManager$grid
  })
  
  # START GAME ----
  session$userData$isStartModalOpen <- reactiveVal(TRUE)
  gameStart$init_server("gameStart", ObjectsManager, consts)
  
  session$userData$level <- reactiveVal("")
  
  observeEvent(input$level, {
    # To allow selecting the same values (e.g. same level, same move direction) in JS and react for them in Shiny input values are "reset" after each usage.
    req(input$level != "clean")
    
    session$userData$isStartModalOpen(FALSE)
    session$userData$level(input$level)
    ObjectsManager$place_objects(input$level)
    
    shinyjs::runjs("cleanObject('level');")
  })
  
  # TIMES UP ----
  observeEvent(input$stop_game, {
    req(isTRUE(input$stop_game))
    
    shinyjs::runjs("stopMove();")
    session$userData$isBiteModalOpen(TRUE)
    
    shinyjs::runjs("cleanObject('stop_game');")
  })
  
  # MOVE OBJECTS ----
  observeEvent(input$diver_direction, {
    req(input$diver_direction != "clean")
    
    ObjectsManager$set_diver_side(input$diver_direction)
    ObjectsManager$move_object("diver", input$diver_direction, extra_content = glue("<p class=timer>{input$time_left}</p>"))
    
    # The diver can swim onto shark and that is considered as bite the same as the shark swims onto diver.
    if(ObjectsManager$check_shark_bite()) {
      session$userData$isBiteModalOpen(TRUE)
    }
    
    if(ObjectsManager$check_success()) {
      session$userData$isSuccessModalOpen(TRUE)
    }
    
    ObjectsManager$check_collect()
    
    shinyjs::runjs("cleanObject('diver_direction');")
  })
  
  observeEvent(input$shark_direction, {
    req(input$shark_direction != "clean")
    
    # Shark direction is a list of moves for each shark i.e. each one moves in independent direction.
    purrr::iwalk(input$shark_direction, ~ObjectsManager$move_object("shark", .x, index = .y))
    
    # All moving elements are based on same interval, originally created for single shark, to not overwhelm browser.
    # Move trash on average on 1/4 shark move. Direction "up" as well as selecting first shark do not matter, just needs to be one out of four options.
    if(input$shark_direction[1] == "up") {
      ObjectsManager$trash_manager$move_all_trash()
    }
    
    # Move boat on 1/2 shark speed and only left or right.
    if(input$shark_direction[1] %in% c("left", "right")) {
      ObjectsManager$move_object("boat", input$shark_direction[1])
    }
    
    if(ObjectsManager$check_shark_bite()) {
      session$userData$isBiteModalOpen(TRUE)
    }
    
    shinyjs::runjs("cleanObject('shark_direction');")
  })
  
  # GAME OVER ----
  session$userData$isBiteModalOpen <- reactiveVal(FALSE)
  session$userData$isSuccessModalOpen <- reactiveVal(FALSE)
  
  gameOver$init_server("gameOver", ObjectsManager, LeaderboardManager, consts)
}
