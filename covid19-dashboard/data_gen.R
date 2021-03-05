per_country_data <- function(covid_data) {
  
  no_china <- covid_data %>% filter(Country.Region != "China") 
  no_china <- data.frame(Province.State = "", Country.Region = "AA - Global (without China)", Lat = 0, Long = 0,
                         t(colSums(no_china[,c(-1, -2, -3, -4)], na.rm = TRUE)))
  hubei <- covid_data %>% filter(Province.State == "Hubei") %>% mutate(Country.Region = "China (only Hubei)")
  if ("Combined_key" %in% covid_data) {
    nyc <- covid_data %>%
      filter(Province.State %in% c("New York City, NY") | Combined_key == "New York City, New York, US") %>%
      mutate(Country.Region = "New York City")
    
  } else {
    nyc <- covid_data %>%
      filter(Province.State %in% c("New York City, NY")) %>%
      mutate(Country.Region = "New York City")
  }
  china_no_hubei <- covid_data %>% filter(Province.State != "Hubei", Country.Region == "China") %>%
    mutate(Country.Region = "China (without Hubei)") 
  global <- data.frame(Province.State = "", Country.Region = "AA - Global", Lat = 0, Long = 0,
                       t(colSums(covid_data[,c(-1, -2, -3, -4)], na.rm = TRUE)))
  rbind(covid_data, no_china, global, hubei, china_no_hubei, nyc) %>%
    mutate(Country.Region = str_replace(Country.Region, "\\*", "")) %>%
    select(-Province.State, -Lat, -Long) %>%
    group_by(Country.Region) %>%
    summarise_all(funs(sum), na.rm = TRUE)
}

per_country_daily <- function(daily_data) {
  daily_data <- daily_data %>% mutate(Country.Region = case_when(
    Country.Region == "Mainland China" ~ "China",
    TRUE ~ as.character(Country.Region)
  ))
  
  if ("Combined_Key" %in% names(daily_data)) {
    nyc <- daily_data %>%
      filter(Province.State %in% c("New York City, NY") | Combined_Key == "New York City, New York, US") %>%
      mutate(Country.Region = "New York City") %>% select(
        Province.State, Country.Region, Last.Update, Confirmed, Deaths, Recovered
      )
    daily_data <- daily_data %>% select(
      Province.State, Country.Region, Last.Update, Confirmed, Deaths, Recovered
    )
  } else {
    nyc <- daily_data %>%
      filter(Province.State %in% c("New York City, NY")) %>%
      mutate(Country.Region = "New York City")
  }
  if (nrow(nyc) == 0) {
    nyc <- data.frame(
      Country.Region = "New York City",
      Province.State = "New York City, NY",
      Last.Update = daily_data$Last.Update[1],
      Confirmed = 0,
      Deaths = 0,
      Recovered = 0
    )
  }
  
  no_china <- daily_data %>% filter(Country.Region != "China") 
  no_china <- data.frame(Province.State = "", Country.Region = "AA - Global (without China)", Last.Update = 0,
                         t(colSums(no_china[,c(-1, -2, -3)], na.rm = TRUE)))
  hubei <- daily_data %>% filter(Province.State == "Hubei") %>% mutate(Country.Region = "China (only Hubei)") 
  china_no_hubei <- daily_data %>% filter(Province.State != "Hubei", Country.Region == "China") %>%
    mutate(Country.Region = "China (without Hubei)") 
  global <- data.frame(Province.State = "", Country.Region = "AA - Global", Last.Update = 0,
                       t(colSums(daily_data[,c(-1, -2, -3)], na.rm = TRUE)))
  
  rbind(daily_data, no_china, global, hubei, china_no_hubei, nyc) %>%
    mutate(Country.Region = str_replace(Country.Region, "\\*", "")) %>%
    select(-Province.State, -Last.Update) %>%
    group_by(Country.Region) %>%
    summarise_all(funs(sum), na.rm = TRUE)
}

generate_from_daily <- function(folder = "COVID-19/csse_covid_19_data/csse_covid_19_daily_reports", column = "Recovered") {
  
  recovered <- do.call(rbind,
                       lapply(list.files(folder, pattern = ".csv", full.names = T), function(x){
                         
                         data_day <- read.csv(x)
                         names(data_day)[grepl("Province", names(data_day))] <- "Province.State"
                         names(data_day)[grepl("Country", names(data_day))] <- "Country.Region"
                         names(data_day)[grepl("Last", names(data_day))] <- "Last.Update"
                         if ("Combined_Key" %in% names(data_day)) {
                           data_day <- data_day %>% select(
                             Province.State, Country.Region, Last.Update, Confirmed, Deaths, Recovered, Combined_Key
                           )
                         } else {
                           data_day <- data_day %>% select(
                             Province.State, Country.Region, Last.Update, Confirmed, Deaths, Recovered
                           )
                         }
                         data_day <- per_country_daily(data_day)
                         date_match <- stringr::str_match(string = x, pattern = "(\\d\\d)\\-(\\d\\d)\\-(\\d\\d\\d\\d)")
                         data.frame(
                           date = as.Date(paste0(date_match[,4], "-", date_match[,2], "-", date_match[,3])),
                           country = data_day$Country.Region,
                           recovered = data_day$Recovered,
                           confirmed = data_day$Confirmed,
                           deaths = data_day$Deaths
                         )
                       }))
  recovered <- recovered  %>% 
    group_by(country) %>%
    mutate(
      recovered = case_when(
        is.na(recovered) ~ lag(recovered),
        TRUE ~ recovered
      ),
      confirmed = case_when(
        is.na(confirmed) ~ lag(confirmed),
        TRUE ~ confirmed
      ),
      deaths = case_when(
        is.na(deaths) ~ lag(deaths),
        TRUE ~ deaths
      ),
      active = as.numeric(confirmed) - as.numeric(deaths) - as.numeric(recovered)
    ) %>%
    ungroup()
  return(recovered)
}
generate_all_from_daily <- function(folder = "COVID-19/csse_covid_19_data/csse_covid_19_daily_reports") {
  
  recovered <- do.call(rbind,
                       lapply(list.files(folder, pattern = ".csv", full.names = T), function(x){
                         
                         data_day <- read.csv(x)
                         names(data_day)[grepl("Province", names(data_day))] <- "Province.State"
                         names(data_day)[grepl("Country", names(data_day))] <- "Country.Region"
                         names(data_day)[grepl("Last", names(data_day))] <- "Last.Update"
                         date_match <- stringr::str_match(string = x, pattern = "(\\d\\d)\\-(\\d\\d)\\-(\\d\\d\\d\\d)")
                         if ("Combined_Key" %in% names(data_day)) {
                           data_day <- data_day %>% select(
                             Province.State, Country.Region, Last.Update, Confirmed, Deaths, Recovered, Long_, Lat, Combined_Key
                           )
                           data.frame(
                             date = as.Date(paste0(date_match[,4], "-", date_match[,2], "-", date_match[,3])),
                             country = data_day$Country.Region,
                             province = data_day$Province.State,
                             recovered = data_day$Recovered,
                             deaths = data_day$Deaths,
                             confirmed = data_day$Confirmed,
                             Longitude = data_day$Long_,
                             Latitude = data_day$Lat,
                             Combined_Key = data_day$Combined_Key
                           )
                         } else if ("Longitude" %in% names(data_day)) {
                           data_day <- data_day %>% select(
                             Province.State, Country.Region, Last.Update, Confirmed, Deaths, Recovered, Longitude, Latitude
                           )
                           data.frame(
                             date = as.Date(paste0(date_match[,4], "-", date_match[,2], "-", date_match[,3])),
                             country = data_day$Country.Region,
                             province = data_day$Province.State,
                             recovered = data_day$Recovered,
                             deaths = data_day$Deaths,
                             confirmed = data_day$Confirmed,
                             Longitude = data_day$Longitude,
                             Latitude = data_day$Latitude,
                             Combined_Key = NA
                           )
                           
                         } else {
                           
                           data_day <- data_day %>% select(
                             Province.State, Country.Region, Last.Update, Confirmed, Deaths, Recovered
                           )
                           data.frame(
                             date = as.Date(paste0(date_match[,4], "-", date_match[,2], "-", date_match[,3])),
                             country = data_day$Country.Region,
                             province = data_day$Province.State,
                             recovered = data_day$Recovered,
                             deaths = data_day$Deaths,
                             confirmed = data_day$Confirmed,
                             Longitude = NA,
                             Latitude = NA,
                             Combined_Key = NA
                           )
                         }
                       }))
  
  
  recovered[which(recovered$date == "2020-01-22"), ] <- recovered[which(recovered$date == "2020-01-22"), ] %>%
    tidyr::replace_na(list(recovered = 0, confirmed = 0, deaths = 0))
  
  
  
  all_data <- recovered %>% 
    mutate(country = case_when(
      as.character(country) == "Mainland China" ~ "China",
      TRUE ~ as.character(country)
    ),
    Combined_Key = case_when(
      province == "" & is.na(Combined_Key) ~ as.character(country),
      is.na(Combined_Key) ~ paste(as.character(province), as.character(country), sep = ", "),
      TRUE ~ as.character(Combined_Key)
    )
    ) %>%
    group_by(country, province) %>%
    mutate(recovered = case_when(
      is.na(recovered) ~ lag(recovered),
      TRUE ~ recovered
    ),
    confirmed = case_when(
      is.na(confirmed) ~ lag(confirmed),
      TRUE ~ confirmed
    ),
    deaths = case_when(
      is.na(deaths) ~ lag(deaths),
      TRUE ~ deaths
    )
    
    ) %>%
    ungroup() %>% mutate(active = confirmed - deaths - recovered)
  
  
  long_lat_old <- read.csv(file.path(folder, "03-02-2020.csv")) %>%
    rename(province = Province.State, country = Country.Region, Lat = Latitude, Long = Longitude) %>%
    mutate(Combined_Key = case_when(
      province == "" ~ as.character(country),
      TRUE ~ paste(as.character(province), as.character(country), sep = ", ")
    )
             ) %>%
    select(province, country, Long, Lat, Combined_Key)
  
  long_lat_old2 <- read.csv(file.path(folder, "03-08-2020.csv")) %>%
    rename(province = Province.State, country = Country.Region, Lat = Latitude, Long = Longitude) %>%
    mutate(Combined_Key = case_when(
      province == "" ~ as.character(country),
      TRUE ~ paste(as.character(province), as.character(country), sep = ", ")
    )
             ) %>%
    select(province, country, Long, Lat, Combined_Key)
  
  long_lat_old3 <- read.csv(file.path(folder, "03-11-2020.csv")) %>%
    rename(province = Province.State, country = Country.Region, Lat = Latitude, Long = Longitude) %>%
    mutate(Combined_Key = case_when(
      province == "" ~ as.character(country),
      TRUE ~ paste(as.character(province), as.character(country), sep = ", ")
    )
             ) %>%
    select(province, country, Long, Lat, Combined_Key)
  
  all_files <- list.files(folder, pattern = ".csv", full.names = T)
  long_lat <- read.csv(all_files[length(all_files)])
  names(long_lat)[grepl("Province", names(long_lat))] <- "Province.State"
  names(long_lat)[grepl("Country", names(long_lat))] <- "Country.Region"
  names(long_lat)[grepl("Long", names(long_lat))] <- "Longitude"
  names(long_lat)[grepl("Lat", names(long_lat))] <- "Latitude"
  long_lat <- long_lat %>%
    rename(province = Province.State, country = Country.Region, Long = Longitude, Lat = Latitude) %>%
    select(province, country, Long, Lat, Combined_Key)
  
  long_lat <- rbind(long_lat, long_lat_old, long_lat_old2, long_lat_old3)
  long_lat <- long_lat[!duplicated(long_lat$Combined_Key), ]
  
  return(left_join(all_data, long_lat, by = c("Combined_Key")))
  
}

new_data_gen <- function(covid_data = NULL, countries = NULL, growth_add = TRUE) {
  
  # start_date <- str_replace(names(covid_data)[2], "X", "0") %>%
  #   as.Date(format="%m.%d.%y")
  # end_date <- str_replace(names(covid_data)[length(names(covid_data))], "X", "0") %>%
  #   as.Date(format="%m.%d.%y")
  # 
  # dates_covid_19_confirmed <- seq.Date(start_date, to = end_date, by = 1)
  # stopifnot((ncol(covid_data) - 1) == length(dates_covid_19_confirmed))
  # 
  # covid_data_selected <- covid_data %>%
  #   filter(Country.Region %in% countries)
  # 
  # covid_data_selected <- as.data.frame(t(covid_data_selected))
  # covid_data_selected$dates <- c("Country", dates_covid_19_confirmed)
  # colnames(covid_data_selected) <- c(covid_data_selected[1, 1:length(countries)] %>% unlist() %>% as.character(), "country")
  # covid_data_selected <- covid_data_selected[-1,]
  # covid_data_selected$country <- dates_covid_19_confirmed
  # selected_data <- if (is.na(colnames(covid_data_selected))) {
  #   covid_data_selected[, -which(is.na(colnames(covid_data_selected)))]
  # } else {
  #   covid_data_selected
  # }
  # country_data <- gather(selected_data, key = country) %>%
  #   filter(!is.na(country))
  
  if (growth_add) {
    
    return(
      covid_data %>%
        add_growth_rate() %>%
        group_by(country) %>%
        mutate(value = case_when(
          is.na(value) ~ lag(value),
          TRUE ~ value
          ),
          change = case_when(
            !is.na(lag(as.numeric(value))) ~ abs(as.numeric(value) - lag(as.numeric(value))),
            TRUE ~ 0
          )
        ) %>% ungroup()
    )
  } else {
    return(
      covid_data %>%
        group_by(country) %>%
        mutate(value = case_when(
          is.na(value) ~ lag(value),
          TRUE ~ value
          ),
          change = case_when(
            !is.na(lag(as.numeric(value))) ~ abs(as.numeric(value) - lag(as.numeric(value))),
            TRUE ~ 0
          )
        ) %>% ungroup()
    )
  }
}

add_rel_data <- function(covid_data, pop_data) {
  
  
  pop_data <- pop_data %>% filter(Country %in% c(countries))
  
  covid_data %>% rowwise() %>%
    mutate(
      value_rel = 100 * as.numeric(value) / pop_data %>%
        filter(Country == country) %>%
        pull()
    ) %>%
    rbind()
}

add_growth_rate <- function(covid_data_long) {
  covid_data_long %>%
    group_by(country) %>%
    mutate(
      doubling_days = ifelse(
        lag(as.numeric(value), n = 4) > 15,
        4 * log(2)/log(as.numeric(value)/as.numeric(lag(value, n = 4))),
        Inf
      ),
      growth.factor = ifelse(
        lag(as.numeric(value), n = 4) > 15,
        (as.numeric(value) - as.numeric(lag(value, n = 4)))
             /
               (as.numeric(lag(value, n = 4)) - as.numeric(lag(value, n = 8))),
        0
      )
    ) %>%
    mutate(
      growth.factor = ifelse(growth.factor > 5, 5, ifelse(growth.factor < 0, 0, growth.factor)),
      doubling_days = ifelse(doubling_days > 1000, Inf, doubling_days)
    ) %>%
    replace_na(list(growth.factor = 0, doubling_days = 0)) %>%
    # Check if there was exponential growth
    mutate(is_exponential = case_when(
      doubling_days > 0 & doubling_days < 6.5 ~ 1,
      TRUE ~ 0
    )) %>%
    add_doubling_rates() %>%
    ungroup() 
}

add_doubling_rates <- function(covid_data_long_grouped) {
  covid_data_long_grouped %>%
    mutate(id = row_number()) %>%
    # Calculate how many days in a row the current situation is on:
    # https://stackoverflow.com/questions/32994060/r-cumulative-sum-by-condition-with-reset
    group_by(group_rm = paste(country, cumsum(c(0, diff(is_exponential) != 0)))) %>%
    mutate(days_in_a_row = row_number()) %>%
    ungroup() %>%
    select(-group_rm)
}

breaks_colors <- function(vec, reverse = FALSE) {
  brks <- quantile(vec, probs = seq(.05, .95, .05), na.rm = TRUE)
  # clrs <- rev(round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  # {paste0("rgb(255,", ., ",", ., ")")})
  clrs_hex <- colorRampPalette(c("#051923", "#003554", "#2196F3", "#16B0F7"))(length(brks) + 1)
  
  clrs <- apply(
    if(reverse) col2rgb(rev(clrs_hex)) else col2rgb(clrs_hex),
    2,
    function(x) paste0("rgb(", x["red"],",",x["green"],",",x["blue"],")")
  )
  list(brks = brks, clrs = clrs)
}

key_factors <- function(covid_data_long, population_data) {
  
  max_exp_data <- covid_data_long %>%
    group_by(country) %>%
    filter(is_exponential == 1) %>%
    summarize(max_exponential_time = max(days_in_a_row)) %>%
    ungroup()
  
  factor_data <- left_join(max_exp_data,
            covid_data_long %>%
              filter(country %in% max_exp_data$country) %>%
              
              group_by(country) %>% 
              filter(date == max(date)) %>%
              mutate(
                doubling_days = round(doubling_days, 2),
                still_exponential = ifelse(is_exponential == 1, "yes", "no")
              ) %>%
              select(doubling_days, still_exponential, value, deaths, recovered)
            , by = "country") %>%
    rename(Country = country)
  
  factor_data <- left_join(factor_data, population_data, by = "Country") %>% 
    filter(!is.na(value)) %>%
    rename( population = Year_2016) %>%
    mutate(per_100000 = round(as.numeric(value)/(population/100000), 1),
           population = round(population/1000000, 2),
           mortality_rate = round(100 * as.numeric(deaths)/(as.numeric(value)), 2),
           active = as.numeric(value) - as.numeric(deaths) - as.numeric(recovered)
           ) %>%
    select(-Country_Code) %>%
    dplyr::arrange(-per_100000)
  return(factor_data)
}

add_mortality <- function(dataset) {
  dataset %>%
    mutate(mortality = as.numeric(deaths)/ as.numeric(value) * 100,
           active = as.numeric(value) - ifelse(is.na(as.numeric(deaths)), 0, as.numeric(deaths)) - as.numeric(recovered)
           ) %>%
    replace_na(list("mortality" = 0, "active" = 0))
}

wait_show <- function(session) {
  shinyjs::runjs("$('#wait').css('display', 'block');")
  material_spinner_show(session,"wait")
}
wait_hide <- function(session) {
  material_spinner_hide(session, "wait")
  shinyjs::runjs("$('#wait').css('display', 'none');")
}