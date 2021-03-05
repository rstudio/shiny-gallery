head(daily_cases2)
daily_cases4 <- daily_cases2
colnames(daily_cases4)[2] <- "SOVEREIGNT"
continents <- st_drop_geometry(countries_old[, c("SOVEREIGNT", "CONTINENT")])
daily_cases5 <- join(daily_cases4, continents, by = "SOVEREIGNT")
daily_cases5[daily_cases5$SOVEREIGNT == "Eswatini"]$CONTINENT = "Africa"
daily_cases5[daily_cases5$SOVEREIGNT == "North Macedonia"]$CONTINENT = "Europe"
daily_cases5[daily_cases5$SOVEREIGNT == "Bahamas"]$CONTINENT = "North America"
daily_cases5[daily_cases5$SOVEREIGNT == "Burma"]$CONTINENT = "Asia"
daily_cases5[daily_cases5$SOVEREIGNT == "Congo (Brazzaville)"]$CONTINENT = "Africa"
daily_cases5[daily_cases5$SOVEREIGNT == "Congo (Kinshasa)"]$CONTINENT = "Africa"
daily_cases5[daily_cases5$SOVEREIGNT == "Cote d'Ivoire"]$CONTINENT = "Africa"
daily_cases5[daily_cases5$SOVEREIGNT == "Korea, South"]$CONTINENT = "Asia"
daily_cases5[daily_cases5$SOVEREIGNT == "Sao Tome and Principe"]$CONTINENT = "Africa"
daily_cases5[daily_cases5$SOVEREIGNT == "Serbia"]$CONTINENT = "Europe"
daily_cases5[daily_cases5$SOVEREIGNT == "Taiwan*"]$CONTINENT = "Asia"
daily_cases5[daily_cases5$SOVEREIGNT == "Tanzania"]$CONTINENT = "Africa"
daily_cases5[daily_cases5$SOVEREIGNT == "Timor-Leste"]$CONTINENT = "Asia"
daily_cases5[daily_cases5$SOVEREIGNT == "US"]$CONTINENT = "North America"
daily_cases6 <- daily_cases5[
  !(
    (daily_cases5$SOVEREIGNT == "Australia" & daily_cases5$CONTINENT %in% c("Asia", "Seven seas (open ocean)")) |
    (daily_cases5$SOVEREIGNT == "Denmark" & daily_cases5$CONTINENT %in% c("North America"))|
    (daily_cases5$SOVEREIGNT == "Netherlands" & daily_cases5$CONTINENT %in% c("North America"))|
    (daily_cases5$SOVEREIGNT == "France" & daily_cases5$CONTINENT %in% c("North America", "Oceania", "Seven seas (open ocean)")) |
    (daily_cases5$SOVEREIGNT == "United Kingdom" & daily_cases5$CONTINENT %in% c("North America", "South America", "Oceania", "Seven seas (open ocean)"))
    )]
daily_cases6 <- daily_cases6[!duplicated(daily_cases6)]
daily_cases4$SOVEREIGNT == daily_cases6$SOVEREIGNT
daily_cases6 <- daily_cases6[, .(
  confirmed = sum(confirmed),
  deaths = sum(deaths),
  recovered = sum(recovered)
), keyby = .(date, CONTINENT)]
daily_cases6 <- daily_cases6[complete.cases(daily_cases6)]
daily_cases6$new_confirmed <- 0
daily_cases6$new_confirmed[8:nrow(daily_cases6)] <- daily_cases6$confirmed[-(1:7)] - daily_cases6$confirmed[1:(nrow(daily_cases6) - 7)]
highchart() %>%
  hc_xAxis(
    categories = unique(daily_cases6$date),
    gridLineWidth = 0,
    title = list(text = "Date")
  ) %>%
  hc_yAxis(title = list(text = "Confirmed cases")) %>%
  hc_add_series(
    name = "Africa",
    data = daily_cases6$new_confirmed[daily_cases6$CONTINENT == "Africa"],
    color = "#EAC435"
  ) %>%
  hc_add_series(
    name = "Asia",
    data = daily_cases6$new_confirmed[daily_cases6$CONTINENT == "Asia"],
    color = "#5EFC8D"
  ) %>%
  hc_add_series(
    name = "Europe",
    data = daily_cases6$new_confirmed[daily_cases6$CONTINENT == "Europe"],
    color = "#347FD5"
  ) %>%
  hc_add_series(
    name = "North America",
    data = daily_cases6$new_confirmed[daily_cases6$CONTINENT == "North America"],
    color = "#EE6352"
  ) %>%
  hc_add_series(
    name = "Oceania",
    data = daily_cases6$new_confirmed[daily_cases6$CONTINENT == "Oceania"],
    color = "#E475FA"
  ) %>%
  hc_add_series(
    name = "South America",
    data = daily_cases6$new_confirmed[daily_cases6$CONTINENT == "South America"],
    color = "#FF334E"
  ) %>%
  hc_add_theme(hc_theme_monokai()) %>%
  hc_title(
    text = "Daily Number of confirmed
      cases"
  ) %>%
  hc_legend(align = "left") %>%
  hc_chart(backgroundColor = "#161616") %>%
  hc_plotOptions(
    line = list(marker = list(enabled = FALSE))
  )
