#' @import dplyr
#' @import reactable
#' @noRd
create_leaderboard <- function(df_position, df_overall) {
  #position_emo, player_name, country, url, total_points, avg_position, avg_margin_victory, avg_time_from_first

  # df_position <- df_position %>%
  #   select(grand_prix_fct, direction, player_name, position, points, n_top3, n_races)
  
  df_overall <- df_overall %>% 
    select(position_emo, player_name, country, url, total_points, avg_position, avg_margin_victory, avg_time_from_first,
           n_first_race)
  
  # define reactable object
  res <- reactable(
    df_overall,
    filterable = FALSE,
    resizable = TRUE,
    showPageSizeOptions = FALSE,
    selection = 'single',
    defaultSelected = 1,
    onClick = 'select',
    highlight = TRUE,
    columns = list(
      position_emo = colDef(
        name = "",
        width = 30,
        cell = function(value, index) {
          emo_val <- dplyr::slice(df_overall, index) %>% dplyr::pull(position_emo)
          emo::ji(emo_val)
        }
      ),
      player_name = colDef(
        name = "Player",
        width = 180,
        cell = function(value, index) {
          # grab appropriate record from country flag data frame
          country_select <- dplyr::slice(df_overall, index) %>% dplyr::pull(country)
          country_url <- dplyr::slice(df_overall, index) %>% dplyr::pull(url)
          div(
            img(class = "flag", src = country_url),
            value
          )
        }),
      country = colDef(show = FALSE),
      url = colDef(show = FALSE),
      total_points = colDef(name = "Total Points"),
      avg_position = colDef(name = "Avg Race Finish", format = colFormat(digits = 3)),
      avg_margin_victory = colDef(
        name = "Avg Margin of Victory",
        cell = function(value, index) {
          avg_margin_victory <- dplyr::slice(df_overall, index) %>% dplyr::pull(avg_margin_victory) %>% round(., 3)
          n_first_race <- dplyr::slice(df_overall, index) %>% dplyr::pull(n_first_race)
          
          if (n_first_race < 1) {
            res <- "No wins"
          } else {
            win_text <- ifelse(n_first_race < 2, "win", "wins")
            res <- glue::glue("{avg_margin_victory} ({n_first_race} {win_text})")
          }
          
          return(res)
        }
      ),
      avg_time_from_first = colDef(name = "Avg Time Behind First", format = colFormat(digits = 3)),
      n_first_race = colDef(show = FALSE)
    ),
    details = function(index) {
      player_selected <- dplyr::slice(df_overall, index) %>% dplyr::pull(player_name)
      gp_df <- filter(df_position, player_name == !!player_selected) %>%
        select(., -player_name, -n_races, -race_points_total)
      htmltools::div(
        style = "padding: 12px",
        reactable(
          gp_df, 
          columns = list(
            grand_prix_fct = colDef(name = "Grand Prix"),
            direction = colDef(name = "Direction"),
            position = colDef(name = "GP Finish"),
            points = colDef(name = "GP Points"),
            n_first = colDef(show = FALSE),
            n_second = colDef(show = FALSE),
            n_third = colDef(show = FALSE),
            avg_position = colDef(name = "Avg Race Finish", format = colFormat(digits = 3)),
            med_position = colDef(show = FALSE),
            avg_margin_victory = colDef(
              name = "Avg Margin of Victory",
              cell = function(value, index) {
                value <- round(value, 3)
                n_first_race <- dplyr::slice(gp_df, index) %>% pull(n_first)
                
                if (n_first_race < 1) {
                  res <- "No wins"
                } else {
                  win_text <- ifelse(n_first_race < 2, "win", "wins")
                  res <- glue::glue("{value} ({n_first_race} {win_text})")
                }
                
                return(res)
              }
            ),
            n_top3 = colDef(name = "Top 3 Finishes"),
            avg_time_from_first = colDef(name = "Avg Time Behind First", format = colFormat(digits = 3))
          ),
          theme = reactableTheme(
            color = "hsl(233, 9%, 87%)",
            backgroundColor = "hsl(233, 9%, 19%)",
            borderColor = "hsl(233, 9%, 22%)",
            stripedColor = "hsl(233, 12%, 22%)",
            highlightColor = "hsl(233, 12%, 24%)",
            inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
          ),
          outlined = TRUE)
      )
    },
    theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    )
  )
}

#' @import dplyr
#' @import reactable
#' @noRd
create_gp_table <- function(df_position, player_selected = NULL) {
  if (is.null(player_selected)) {
    gp_df <- df_position %>%
      select(., -n_races)
  } else {
    gp_df <- filter(df_position, player_name == !!player_selected) %>%
      select(., -player_name, -n_races)
  }
  
  res <- reactable(
    gp_df, 
    theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    ),
    outlined = TRUE)
  
  return(res)
}
  
  

#' @import dplyr
#' @import reactable
#' @noRd
create_track_table <- function(raw_df) {
  
  # overall grand prix stats
  gp_stats <- raw_df %>%
    gen_tidy_race_data() %>%
    gen_grandprix_summary(direction_group = FALSE) %>%
    mutate(grand_prix = as.character(grand_prix_fct))
  
  # get all available tracks from built in set
  region_df <- tibble::enframe(hotshot_data$tracks) %>%
    tidyr::unnest(cols = value) %>%
    rename(region = name, track = value)
  
  gp_df <- tibble::enframe(hotshot_data$grand_prix) %>%
    tidyr::unnest(cols = value) %>%
    rename(grand_prix = name, track = value)
  
  track_df <- raw_df %>%
    gen_tidy_race_data() %>%
    filter(position == 1)  %>%
    select(grand_prix, track, direction, winner = player_name, winning_time = player_time, margin_victory, top_3_sep) %>%
    distinct()
    
    
  
  # obtain tracks used thus far
  current_tracks <- raw_df %>%
    select(grand_prix, track) %>%
    distinct() %>%
    left_join(region_df, by = "track") %>%
    tidyr::nest(tracks = c(track, region))
  
  table_df <- left_join(gp_stats, current_tracks, by = "grand_prix") %>%
    select(grand_prix, tracks, n_racers, n_unique_winners, avg_margin_victory, avg_top_3_sep)
  
  res <- reactable(
    table_df,
    #filterable = TRUE,
    resizable = TRUE,
    showPageSizeOptions = FALSE,
    selection = NULL,
    highlight = TRUE,
    columns = list(
      grand_prix = colDef(name = "Grand Prix", width = 100),
      n_racers = colDef(name = "Racers"),
      n_unique_winners = colDef(name = "Unique Race Winners"),
      avg_margin_victory = colDef(name = "Avg Margin of Victory", format = colFormat(digits = 3)),
      avg_top_3_sep = colDef(name = "Avg Top 3 Separation", format = colFormat(digits = 3)),
      tracks = colDef(
        name = "Tracks",
        width = 400,
        cell = function(value, index) {
          track_names <- slice(table_df, index) %>% 
            tidyr::unnest(tracks) %>%
            pull(track)
          
          glue::glue_collapse(track_names, sep = ", ", last = "")
        }
      )
    ),
    details = colDef(
      details = function(index) {
        # obtain the selected grand prix
        grand_prix_selected <- slice(table_df, index) %>% pull(grand_prix)
        
        track_df_sub <- filter(track_df, grand_prix == grand_prix_selected) %>%
          select(., -grand_prix)
        
        htmltools::div(
          style = "padding: 16px",
          reactable(
            track_df_sub, 
            columns = list(
              track = colDef(name = "Track"),
              direction = colDef(name = "Direction"),
              winner = colDef(name = "Winner"),
              winning_time = colDef(name = "Time"),
              margin_victory = colDef(name = "Margin of Victory", format = colFormat(digits = 3)),
              top_3_sep = colDef(name = "Top 3 Separation", format = colFormat(digits = 3))
            ),
            theme = reactableTheme(
              color = "hsl(233, 9%, 87%)",
              backgroundColor = "hsl(233, 9%, 19%)",
              borderColor = "hsl(233, 9%, 22%)",
              stripedColor = "hsl(233, 12%, 22%)",
              highlightColor = "hsl(233, 12%, 24%)",
              inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
              selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
              pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
              pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
            ),
            outlined = TRUE
          )
        ) 
      }
    ),
    theme = reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
    )
  )
  
  return(res)
}

