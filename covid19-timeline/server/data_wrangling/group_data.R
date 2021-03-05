daily_cases <- corona_sf[, .(
  confirmed = sum(confirmed),
  deaths = sum(deaths),
  recovered = sum(recovered)
), by = date]

daily_cases2 <- corona_sf[, .(
  confirmed = sum(confirmed),
  deaths = sum(deaths),
  recovered = sum(recovered)
), keyby = .(date, `Country/Region`)]
daily_cases2 <- daily_cases2[order(
  daily_cases2$date, daily_cases2$`Country/Region`
), ]
a <- corona_sf[!duplicated(corona_sf$`Country/Region`), ]
a <- a[order(a$`Country/Region`), ]
daily_cases2$geometry <- rep(a$geometry, length(unique(daily_cases2$date)))
