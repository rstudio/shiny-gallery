#' @import dplyr
#' @noRd
gen_tidy_race_data <- function(raw_df) {
  grand_prix_order <- names(hotshot_data$grand_prix)
  track_order <- purrr::simplify(hotshot_data$grand_prix)
  names(track_order) <- NULL
  
  df2 <- raw_df %>%
    mutate(player_time = stringi::stri_replace_last(player_time, replacement = ".", regex = "\\:"),
           race_time = as.numeric(as.difftime(player_time, format="%M:%OS", units="secs")),
           grand_prix_fct = factor(grand_prix, levels = grand_prix_order),
           track_fct = factor(track, levels = track_order),
           direction_fct = factor(direction, levels = c("normal", "mirrored")))
  
  df2 <- add_margin_of_victory(df2)
  return(df2)
}

#' @import dplyr
#' @noRd
gen_tidy_racers <- function(hotshot_data) {
  df <- hotshot_data
  df$tracks <- NULL
  df$grand_prix <- NULL
  
  res <- df %>%
    tibble::as_tibble(.) %>%
    tidyr::unnest_wider(col = "drivers") %>%
    tidyr::unnest_longer(col = "cars") %>%
    tidyr::unnest_wider(col = "cars")
  return(res)
}

#' @import tidyr
#' @import dplyr
gen_track_compare <- function(df2) {
  # reshape to get track times in separate cols
  df_wide <- df2 %>%
    select(grand_prix_fct, track_fct, direction_fct, player_name, race_time) %>%
    tidyr::pivot_wider(names_from = direction_fct, values_from = race_time) %>%
    mutate(race_diff_time = normal - mirrored)
  return(df_wide)
}

#' @import dplyr
#' @noRd
add_margin_of_victory <- function(df2) {
  df2 <- df2 %>%
    group_by(grand_prix_fct, direction_fct, track_fct) %>%
    mutate(winning_time = min(race_time, na.rm = TRUE),
           diff_time = race_time - winning_time) %>%
    ungroup()
  
  df_mov <- df2 %>%
    group_by(grand_prix_fct, direction_fct, track_fct) %>%
    arrange(grand_prix_fct, direction_fct, track_fct, diff_time) %>%
    mutate(diff_index = row_number()) %>%
    filter(diff_index == 2) %>%
    rename(margin_victory = diff_time) %>%
    ungroup() %>%
    select(grand_prix_fct, direction_fct, track_fct, margin_victory)
  
  df_top3 <- df2 %>%
    group_by(grand_prix_fct, direction_fct, track_fct) %>%
    arrange(grand_prix_fct, direction_fct, track_fct, diff_time) %>%
    mutate(diff_index = row_number()) %>%
    filter(diff_index == 3) %>%
    rename(top_3_sep = diff_time) %>%
    ungroup() %>%
    select(grand_prix_fct, direction_fct, track_fct, top_3_sep)
  
  df2 <- left_join(df2, df_mov, by = c("grand_prix_fct", "direction_fct", "track_fct"))
  df2 <- left_join(df2, df_top3, by = c("grand_prix_fct", "direction_fct", "track_fct"))
  
  df2 <- df2 %>%
    mutate(player_margin_victory = ifelse(position == 1, margin_victory, NA))
  
  return(df2)  
}

#' @import dplyr
#' @noRd
gen_summary_gp_data <- function(df2) {
  df_summary1 <- df2 %>%
    group_by(grand_prix_fct, direction, player_name) %>%
    summarize(race_points_total = sum(points, na.rm = TRUE),
              n_races = n(),
              n_first = sum(position == 1),
              n_second = sum(position == 2),
              n_third = sum(position == 3),
              n_top3 = sum(position <= 3),
              avg_position = mean(position, na.rm = TRUE),
              med_position = median(position, na.rm = TRUE),
              avg_margin_victory = mean(player_margin_victory, na.rm = TRUE),
              avg_time_from_first = mean(diff_time, na.rm = TRUE)) %>%
    arrange(grand_prix_fct, desc(direction), desc(race_points_total), desc(n_first), desc(n_second), desc(n_third)) %>%
    ungroup() %>%
    group_by(grand_prix_fct, direction) %>%
    mutate(position = row_number()) %>%
    left_join(hotshot_points, by = "position") %>%
    ungroup()
  
  return(df_summary1)
}

gen_summary_overall <- function(df_summary1) {
  
  df_summary2 <- df_summary1 %>%
    group_by(grand_prix_fct, player_name) %>%
    summarize(total_gp_points = sum(points, na.rm = TRUE),
              n_races = sum(n_races),
              n_top3 = sum(n_top3),
              n_first_race = sum(n_first),
              n_first_gp = sum(position == 1),
              n_second_gp = sum(position == 2),
              n_third_gp = sum(position == 3)) %>%
    ungroup() %>%
    arrange(grand_prix_fct, desc(total_gp_points), desc(n_first_gp), desc(n_second_gp), desc(n_third_gp)) %>%
    group_by(player_name) %>%
    summarize(total_points = sum(total_gp_points),
              n_races = sum(n_races),
              n_top3 = sum(n_top3),
              n_first_race = sum(n_first_race),
              n_first_gp = sum(n_first_gp),
              n_second_gp = sum(n_second_gp),
              n_third_gp = sum(n_third_gp)) %>%
    ungroup() %>%
    arrange(desc(total_points), desc(n_first_gp), desc(n_second_gp), desc(n_third_gp))
  
  return(df_summary2)
}

gen_grandprix_summary <- function(df2, direction_group = TRUE) {
  # prep a grouped version of data set
  if (direction_group) {
    df_g <- df2 %>%
      group_by(grand_prix_fct, direction_fct)
  } else {
    df_g <- df2 %>%
      group_by(grand_prix_fct)
  }
  
  # obtain the number of unique winners
  df_winners <- df_g %>%
    filter(position == 1) %>%
    summarize(n_unique_winners = length(unique(player_name))) %>%
    ungroup()
  
  df_summary3 <- df_g %>%
    summarize(avg_margin_victory = mean(margin_victory, na.rm = TRUE),
              avg_top_3_sep = mean(top_3_sep, na.rm = TRUE),
              n_racers = length(unique(player_name))) %>%
    ungroup() %>%
    left_join(df_winners)

  return(df_summary3)
}

gen_track_summary <- function(df2, direction_group = TRUE) {
  # prep a grouped version of data set
  if (direction_group) {
    df_g <- df2 %>%
      group_by(grand_prix_fct, track_fct, direction_fct)
  } else {
    df_g <- df2 %>%
      group_by(grand_prix_fct, track_fct)
  }
}

gen_player_car_stats <- function(df2) {
  # obtain player performance based on type of car
  df_cars <- gen_tidy_racers(hotshot_data) %>%
    select(driver = driver_name, car = car_name, type, driver_img_url, car_img_url)
  
  df_player_stats3 <- df2 %>%
    left_join(df_cars, by = c("driver", "car")) %>%
    group_by(player_name, type) %>%
    summarize(n_races = length(type),
              n_first = sum(position == 1L),
              n_top3 = sum(position <= 3),
              avg_position = mean(position, na.rm = TRUE),
              avg_margin_victory = mean(player_margin_victory, na.rm = TRUE)) %>%
    ungroup()
  
  return(df_player_stats3)
}

gen_player_driver_stats <- function(df2) {
  # obtain player performance based on type of car
  df_cars <- gen_tidy_racers(hotshot_data) %>%
    select(driver = driver_name, car = car_name, type, driver_img_url, car_img_url)
  
  df_player_stats3 <- df2 %>%
    left_join(df_cars, by = c("driver", "car")) %>%
    group_by(player_name, driver) %>%
    summarize(n_races = length(type),
              n_first = sum(position == 1L),
              n_top3 = sum(position <= 3),
              avg_position = mean(position, na.rm = TRUE),
              avg_margin_victory = mean(player_margin_victory, na.rm = TRUE)) %>%
    ungroup()
  
  return(df_player_stats3)
}

gen_player_summary <- function(df2) {
  df_player_stats1 <- df2 %>%
    group_by(player_name) %>%
    summarize(n_races = length(position),
              n_first = sum(position == 1L),
              n_top3 = sum(position <= 3),
              avg_position = mean(position, na.rm = TRUE),
              med_position = median(position, na.rm = TRUE),
              avg_margin_victory = mean(player_margin_victory, na.rm = TRUE),
              avg_time_from_first = mean(diff_time, na.rm = TRUE)) %>%
    ungroup()
  
  # obtain how many grand prix the players won
  df_player_stats2 <- df2 %>%
    gen_summary_gp_data() %>%
    gen_summary_overall() %>%
    select(player_name, n_first_gp)
  
  df_final <- left_join(df_player_stats1, df_player_stats2, by = "player_name") %>%

  return(df_final)
  
}

gen_overall_summary <- function(df2) {
  
  # unique first place winners
  fp_winners <- df2 %>%
    filter(position == 1) %>%
    pull(player_name) %>%
    unique()
  
  dnf_losers <- df2 %>%
    filter(player_time == "DNF") %>%
    pull(player_name) %>%
    unique()
  
  df_summary4 <- df2 %>%
    summarize(n_grand_prix = length(unique(grand_prix)),
              n_laps_complete = n_grand_prix * 8,
              n_races = length(unique(track)) * 2,
              n_racers = length(unique(player_name)),
              total_time_seconds = sum(race_time, na.rm = TRUE),
              total_time_minutes = total_time_seconds / 60,
              total_time_hours = total_time_minutes / 60,
              avg_margin_victory = mean(margin_victory),
              avg_top_3_sep = mean(top_3_sep)) %>%
    mutate(n_firstplace_winners = length(fp_winners),
           n_dnf_losers = length(dnf_losers))
  
  return(df_summary4)
}

gen_plot_data <- function(df2, all_drivers = TRUE) {
  
  # create a unique variable of track + direction combo
  race_grp <- df2 %>%
    select(grand_prix_fct, track_fct, direction_fct) %>%
    distinct() %>%
    mutate(race_index = row_number())
  
  df2 <- left_join(df2, race_grp)
  
  if (all_drivers) {
    # obtain all racers
    all_racers <- unique(df2$player_name)
    
    # find which racers are not in all races and create custom subset for them
    race_check_df <- df2 %>%
      select(race_index, player_name) %>%
      #group_by(race_index) %>%
      tidyr::nest(data = c(player_name)) %>%
      rowwise() %>%
      mutate(missing_df = purrr::map(data, ~{
        # grab list of players in race
        current_players <- .x
        names(current_players) <- NULL
        missing_players <- setdiff(all_racers, current_players)
        df <- tibble::tibble(
          player_name = missing_players,
          points = 0,
          position = 999,
          race_time = NA
        )
        return(df)
      })) %>%
      ungroup() %>%
      select(race_index, missing_df) %>%
      tidyr::unnest(cols = missing_df) %>%
      left_join(
        df2 %>%
          select(., -points, -position, -player_name, -player_time, -screenshot_link, -race_time) %>%
          distinct())
    
    df_chart <- bind_rows(df2, race_check_df)
  } else {
    df_chart <- df2
  }
  
  df_chart <- df_chart %>%
    arrange(race_index, position) %>%
    mutate(top3_finish = ifelse(position <= 3, 1, 0)) %>%
    mutate(dnf_finish = ifelse(player_time == "DNF", 1, 0),
           dnf_finish = ifelse(is.na(dnf_finish), 0, dnf_finish)) %>%
    group_by(player_name) %>%
    mutate(points_running = cumsum(points),
           top3_running = cumsum(top3_finish),
           dnf_running = cumsum(dnf_finish)) %>%
    ungroup() %>%
    select(race_index, grand_prix_fct, grand_prix, track_fct, track, direction_fct, direction, player_name, position, points, points_running, top3_running, dnf_running) %>%
    mutate(frame_label = glue::glue("{track} ({direction})")) %>%
    mutate(frame_label_fct = factor(frame_label, levels = unique(.[["frame_label"]])))
  
  return(df_chart)
}