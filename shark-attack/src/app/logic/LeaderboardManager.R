import("R6")
import("dplyr")
import("utils")
import("googlesheets4")

export("LeaderboardManager")

LeaderboardManager <- R6::R6Class(
  "LeaderboardManager",
  
  private = list(
    leaderboard_id = "1eMgZAKkuPc1o4ZM39xQprDj97UulIhIgjdtN7Dvxuv0",
    
    load_data_gsheets = function() {
      read_sheet(private$leaderboard_id)
    }
  ),
  
  public = list(
    get_leaderboard = function(level) {
      read_sheet(private$leaderboard_id) %>% 
        dplyr::filter(level == !!level) %>% 
        dplyr::arrange(desc(score)) %>% 
        dplyr::select(!level) %>% 
        head(10L) %>% 
        tibble::rowid_to_column("#")
    },
    
    save_to_leaderboard = function(nick, level, score) {
      data <- data.frame(nick = nick, level = level, score = score)
      sheet_append(private$leaderboard_id, data)
    },
    
    initialize = function() {
      gs4_auth(path = "googlesheets_serviceaccount.json")
    }
  )
)
