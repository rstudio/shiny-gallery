# this part pretty generates some basic sentences for some parts of the timeline or global leaders
# i won't go into detail
infected_world <- daily_cases[
  daily_cases$date == max(daily_cases$date), ]$confirmed
died <- daily_cases[daily_cases$date == max(daily_cases$date), ]$deaths
file_1[14] <- paste(
  "To date",
  format(infected_world, big.mark = "."),
  "people have been infected with the virus and",
  format(died, big.mark = "."),
  "have died."
)
today <- daily_cases2[daily_cases2$date == max(daily_cases2$date), ]
country <- as.character(today[today$confirmed == max(today$confirmed), 2])
if (country == "US") {
  country <- "the United States"
}
file_2[8] <- paste(
  "In ",
  country,
  ", ",
  format(
    as.numeric(today[today$confirmed == max(today$confirmed), 3]),
    big.mark = "."
  ),
  " people are infected with the virus, making it the country with the most
  infections worldwide.",
  sep = ""
)
country <- as.character(today[today$deaths == max(today$deaths), 2])
if (country == "US") {
  country <- "the United States"
}
file_3[8] <- paste(
  format(
    as.numeric(today[today$deaths == max(today$deaths), 4]),
    big.mark = "."
  ),
  "people have died in",
  country,
  "as a result of the coronavirus. <br>"
)

file_3[9] <- paste(
  "This makes",
  country,
  "the country with the most deaths attributable to the virus."
)
country <- as.character(today[today$confirmed == max(today$confirmed), 2])
if (country == "US") {
  country <- "the United States"
}

new_in_1 <- sum(
  !top15_confirmed_per_capita$country %in% top15_confirmed$`Country/Region`
  )
if (new_in_1 > 0) {
  text_1 <- paste(
    new_in_1,
    "countries that are not among the countries with the most infections are
    among the countries with the highest infection rate."
  )
} else {
  text_1 <- "It's still the same countries."
}

appears_1 <- today[
  today$confirmed == max(today$confirmed),
  ]$`Country/Region` %in% top15_confirmed_per_capita$country
if (!appears_1) {
  text_2 <- paste(
    "Interestingly, the country with the most infections, ",
    country,
    ", does not appear here.",
    sep = ""
  )
} else {
  pos <- match(
    as.character(today[today$confirmed == max(today$confirmed), 2]),
    top15_confirmed_per_capita$country
  )
  text_2 <- paste(
    "The country with the most infections, ",
    country,
    ", is ",
    ordinal(pos),
    ".",
    sep = ""
  )
}

file_4[9] <- paste(text_1, text_2, sep = "<br>")

country <- as.character(today[today$deaths == max(today$deaths), 2])
if (country == "US") {
  country <- "the United States"
}

new_in_2 <- sum(
  !top15_deaths_per_capita$country %in% top15_deaths$`Country/Region`
)
if (new_in_2 > 0) {
  if (new_in_1 == new_in_2) {
    start <- "Once again,"
  } else {
    start <- "This time,"
  }
  text_1 <- paste(
    start,
    new_in_2,
    "countries that are not among the countries with the most deaths are among
    the countries with the highest death rate."
  )
} else {
  if (new_in_1 == 0) {
    text_1 <- "Once again, the same countries remain."
  } else {
    text_1 <- "It's still the same countries."
  }
}

appears_2 <- today[
  today$deaths == max(today$deaths),
]$`Country/Region` %in% top15_confirmed_per_capita$country
if (!appears_2) {
  if (!appears_1) {
    text_2 <- paste(
      "Once again, the country with the most deaths, ",
      country,
      ", does not appear here.",
      sep = ""
    )
  } else {
    text_2 <- paste(
      "Interestingly, the country with the most deaths, ",
      country,
      ", does not appear here.",
      sep = ""
    )
  }
} else {
  pos <- match(
    as.character(today[today$deaths == max(today$deaths), 2]),
    top15_deaths_per_capita$country
  )
  text_2 <- paste(
    "The country with the most deaths, ",
    country,
    ", is ",
    ordinal(pos),
    ".",
    sep = ""
  )
}

file_5[9] <- paste(text_1, text_2, sep = "<br>")

country_1 <- df_most[1, ]$country
if (country_1 == "US") {
  country_1 <- "the United States"
}
text_1 <- paste(
  "In ",
  country_1,
  ", the number of new infections has increased from ",
  format(df_most[1, ]$old, big.mark = "."), " to ",
  format(df_most[1, ]$new, big.mark = "."), ", an increase of ",
  format(df_most[1, ]$increase, big.mark = "."),
  "%.",
  sep = ""
)
text_2 <- "However, these numbers can easily be influenced by outliers.
Countries that didn't have a lot of cases a fortnight ago."
text_3 <- paste(
  "Let's see what the data looks like when a country must have had at least",
  format(limit, big.mark = "."),
  "infections two weeks ago."
)


file_6[9] <- paste(text_1, text_2, sep = "<br>")
file_6[12] <- text_3

country_2 <- df_limit_most[1, ]$country
if (country_2 == "US") {
  country_2 <- "the United States"
}

if (country_1 != country_2) {
  if (country_2 == "the United States") {
    country_2 <- "The United States"
  }

  text_1 <- paste(
    country_2,
    " now has the highest increase in new infections.
    The figures rose from <br>",
    format(df_limit_most[1, ]$old, big.mark = "."), " to ",
    format(df_limit_most[1, ]$new, big.mark = "."), ", an increase of <br>",
    format(df_limit_most[1, ]$increase, big.mark = "."),
    "%.",
    sep = ""
  )
  if (country_1 %in% df_limit_most$country) {
    text_1 <- paste(
      text_1,
      paste(
        country_1,
        " is in ", 
        ordinal(
          match(
            df_most[1, ]$country,
            df_limit_most$country
          )
        ),
        "place.",
        sep = ""
      ),
      sep = "<br>"
    )
  } else {
    text_1 <- paste(
      text_1,
      paste(
        "The previous leader, ",
        country_1,
        ", is not even in the top ",
        nrow(df_limit_most),
        ".",
        sep = ""
      ),
      sep = "<br>"
    )
  }
}

file_7[9] <- text_1

if (df_limit_few$country[1] == "China") {
  text_1 <- paste(
    "China.<br>The country where it all began.<br>Only ",
    format((df_limit_few$new - df_limit_few$old)[1], big.mark = "."),
    " new cases.<br>An increase of only ", df_limit_few$increase[1], "%.",
    sep = ""
  )
} else {
  text_1 <- paste(
    as.character(df_limit_few$country[1]), " only reported ",
    format((df_limit_few$new - df_limit_few$old)[1], big.mark = "."),
    "new cases.<br>An increase of only ", df_limit_few$increase[1], "%.",
    sep = ""
  )
}

file_8[9] <- text_1

text_1 <- paste(
  "In 30 days we could have anywhere between",
  format(max(forecast_df$lower95_conf, na.rm = TRUE), big.mark = "."),
  "and",
  format(max(forecast_df$upper95_conf, na.rm = TRUE), big.mark = "."),
  "confirmed cases."
)

text_2 <- paste(
  "And how many people might have died by",
  format(Sys.Date() + 30, "%B %d?")
)

file_9[9] <- text_1
file_9[12] <- text_2

text_1 <- paste(
  "Anywhere between ",
  format(max(forecast_df$lower95_death, na.rm = TRUE), big.mark = "."),
  " and <br>",
  format(max(forecast_df$upper95_death, na.rm = TRUE), big.mark = "."),
  ".",
  sep = ""
)

file_10[9] <- text_1

writeLines(file_1, "html_files/today_1.html")
writeLines(file_2, "html_files/today_2.html")
writeLines(file_3, "html_files/today_3.html")
writeLines(file_4, "html_files/leaders_4.html")
writeLines(file_5, "html_files/leaders_5.html")
writeLines(file_6, "html_files/leaders_6.html")
writeLines(file_7, "html_files/leaders_7.html")
writeLines(file_8, "html_files/leaders_8.html")
writeLines(file_9, "html_files/text_12.html")
writeLines(file_10, "html_files/today.html")
