output$total_cases <- renderText({
  a <- get_country()
  # display custom text based on what was clicked
  if (a != "world") {
    text <- paste("Coronavirus cases in ", a, ":", sep = "")
  } else {
    text <- "Worldwide Coronavirus cases:"
  }
  text
})

# default text
output$text_default <- renderText({
  "Worldwide Coronavirus cases:"
})

output$all_default <- renderText({
  # get the daterange
  daterange <- get_date()
  # check if the first date changed
  check1 <- daterange[1] == (min(corona_sf$date) + 1)
  # check if the second date changed
  check2 <- daterange[2] == max(corona_sf$date)
  # calculate the number of cases
  if (all(c(check1, check2))) {
    paste(
      "<font size = 3em> confirmed:</font><br>",
      sum(corona_sf[corona_sf$date == max(corona_sf$date), ]$confirmed,
        na.rm = TRUE
      )
    )
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(
      corona_sf[corona_sf$date == daterange[1], ]$confirmed,
      na.rm = TRUE
    )
    cases_end <- sum(
      corona_sf[corona_sf$date == daterange[2], ]$confirmed,
      na.rm = TRUE
    )
    paste(
      "<font size = 3em> confirmed:</font><br>", cases_end - cases_beginning
    )
  }
})

output$all_country <- renderText({
  # get the country
  country <- get_country()
  daterange <- input$date
  # calculate cases based on whether a country was clicked
  if (country != "world") {
    country_df <- st_as_sf(countries[countries$ADMIN == country, ], crs = 4326)
    corona_sf <- st_as_sf(corona_sf, crs = 4326)
    if (country == "Israel") {
      corona_frame <- corona_sf[corona_sf$`Country/Region` == "Israel", ]
    } else {
      corona_frame <- corona_sf[unlist(st_contains(country_df, corona_sf)), ]
    }
    if (country == "Italy") {
      corona_frame <- corona_frame[corona_frame$`Country/Region` == "Italy", ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_sf[corona_sf$`Province/State` == country, ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_sf[corona_sf$`Country/Region` == country, ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_frame[seq_len(length(unique(corona_sf$date))), ]
      corona_frame$`Province/State` <- country
      corona_frame$`Country/Region` <- country
      corona_frame$confirmed <- 0
      corona_frame$deaths <- 0
      corona_frame$recovered <- 0
      corona_frame$date <- unique(corona_sf$date)
      corona_frame$geometry <- country_df$geometry
    }
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(
      corona_frame[corona_frame$date == daterange[1], ]$confirmed,
      na.rm = TRUE
    )
    cases_end <- sum(
      corona_frame[corona_frame$date == daterange[2], ]$confirmed,
      na.rm = TRUE
    )
    paste(
      "<font size = 3em> confirmed:</font><br>", cases_end - cases_beginning
    )
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(
      corona_sf[corona_sf$date == daterange[1], ]$confirmed,
      na.rm = TRUE
    )
    cases_end <- sum(
      corona_sf[corona_sf$date == daterange[2], ]$confirmed,
      na.rm = TRUE
    )
    paste(
      "<font size = 3em> confirmed:</font><br>", cases_end - cases_beginning
    )
  }
})

output$recovered_default <- renderText({
  # get the daterange
  daterange <- get_date()
  # check if the first date changed
  check1 <- daterange[1] == (min(corona_sf$date) + 1)
  # check if the second date changed
  check2 <- daterange[2] == max(corona_sf$date)
  # calculate the number of cases
  if (all(c(check1, check2))) {
    paste(
      "<font size = 3em> recovered:</font><br>",
      sum(corona_sf[corona_sf$date == max(corona_sf$date), ]$recovered,
        na.rm = TRUE
      )
    )
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(
      corona_sf[corona_sf$date == daterange[1], ]$recovered,
      na.rm = TRUE
    )
    cases_end <- sum(
      corona_sf[corona_sf$date == daterange[2], ]$recovered,
      na.rm = TRUE
    )
    paste(
      "<font size = 3em> recovered:</font><br>", cases_end - cases_beginning
    )
  }
})

output$recovered_country <- renderText({
  # get the country
  country <- get_country()
  daterange <- input$date
  # calculate cases based on whether a country was clicked
  if (country != "world") {
    country_df <- st_as_sf(countries[countries$ADMIN == country, ], crs = 4326)
    corona_sf <- st_as_sf(corona_sf, crs = 4326)
    if (country == "Israel") {
      corona_frame <- corona_sf[corona_sf$`Country/Region` == "Israel", ]
    } else {
      corona_frame <- corona_sf[unlist(st_contains(country_df, corona_sf)), ]
    }
    if (country == "Italy") {
      corona_frame <- corona_frame[corona_frame$`Country/Region` == "Italy", ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_sf[corona_sf$`Province/State` == country, ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_sf[corona_sf$`Country/Region` == country, ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_frame[seq_len(length(unique(corona_sf$date))), ]
      corona_frame$`Province/State` <- country
      corona_frame$`Country/Region` <- country
      corona_frame$confirmed <- 0
      corona_frame$deaths <- 0
      corona_frame$recovered <- 0
      corona_frame$date <- unique(corona_sf$date)
      corona_frame$geometry <- country_df$geometry
    }
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(
      corona_frame[corona_frame$date == daterange[1], ]$recovered,
      na.rm = TRUE
    )
    cases_end <- sum(
      corona_frame[corona_frame$date == daterange[2], ]$recovered,
      na.rm = TRUE
    )
    paste(
      "<font size = 3em> recovered:</font><br>", cases_end - cases_beginning
    )
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(
      corona_sf[corona_sf$date == daterange[1], ]$recovered,
      na.rm = TRUE
    )
    cases_end <- sum(
      corona_sf[corona_sf$date == daterange[2], ]$recovered,
      na.rm = TRUE
    )
    paste(
      "<font size = 3em> recovered:</font><br>", cases_end - cases_beginning
    )
  }
})



output$death_default <- renderText({
  # get the daterange
  daterange <- get_date()
  # check if the first date changed
  check1 <- daterange[1] == (min(corona_sf$date) + 1)
  # check if the second date changed
  check2 <- daterange[2] == max(corona_sf$date)
  # calculate the number of cases
  if (all(c(check1, check2))) {
    paste(
      "<font size = 3em> deceased:</font><br>",
      sum(corona_sf[corona_sf$date == max(corona_sf$date), ]$deaths,
        na.rm = TRUE
      )
    )
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(
      corona_sf[corona_sf$date == daterange[1], ]$deaths,
      na.rm = TRUE
    )
    cases_end <- sum(
      corona_sf[corona_sf$date == daterange[2], ]$deaths,
      na.rm = TRUE
    )
    paste("<font size = 3em> deceased:</font><br>", cases_end - cases_beginning)
  }
})

output$death_country <- renderText({
  country <- get_country()
  daterange <- input$date
  # calculate the number of cases based on whether a country was clicked
  if (country != "world") {
    country_df <- st_as_sf(countries[countries$ADMIN == country, ], crs = 4326)
    corona_sf <- st_as_sf(corona_sf, crs = 4326)
    if (country == "Israel") {
      corona_frame <- corona_sf[corona_sf$`Country/Region` == "Israel", ]
    } else {
      corona_frame <- corona_sf[unlist(st_contains(country_df, corona_sf)), ]
    }
    if (country == "Italy") {
      corona_frame <- corona_frame[corona_frame$`Country/Region` == "Italy", ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_sf[corona_sf$`Province/State` == country, ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_sf[corona_sf$`Country/Region` == country, ]
    }
    if (nrow(corona_frame) == 0) {
      corona_frame <- corona_frame[seq_len(length(unique(corona_sf$date))), ]
      corona_frame$`Province/State` <- country
      corona_frame$`Country/Region` <- country
      corona_frame$confirmed <- 0
      corona_frame$deaths <- 0
      corona_frame$recovered <- 0
      corona_frame$date <- unique(corona_sf$date)
      corona_frame$geometry <- country_df$geometry
    }
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(
      corona_frame[corona_frame$date == daterange[1], ]$deaths,
      na.rm = TRUE
    )
    cases_end <- sum(
      corona_frame[corona_frame$date == daterange[2], ]$deaths,
      na.rm = TRUE
    )
    paste("<font size = 3em> deceased:</font><br>", cases_end - cases_beginning)
  } else {
    daterange[1] <- daterange[1] - 1
    cases_beginning <- sum(
      corona_sf[corona_sf$date == daterange[1], ]$deaths,
      na.rm = TRUE
    )
    cases_end <- sum(
      corona_sf[corona_sf$date == daterange[2], ]$deaths,
      na.rm = TRUE
    )
    paste("<font size = 3em> deceased:</font><br>", cases_end - cases_beginning)
  }
})

# default
output$show_everything <- renderText({
  "To show all cases, simply click anywhere in the ocean"
})
