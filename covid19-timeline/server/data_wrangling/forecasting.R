inds <- daily_cases$date

## Create a time series object

myts_conf <- ts(daily_cases$confirmed, # random data
  start = c(2020, as.numeric(format(inds[1], "%j"))),
  frequency = 365
)
myts_death <- ts(daily_cases$deaths, # random data
  start = c(2020, as.numeric(format(inds[1], "%j"))),
  frequency = 365
)

# forecast the confirmed cases
forecast_conf <- forecast(auto.arima(myts_conf), h = 30, level = 95)
# round the numbers
forecast_conf$lower <- round(forecast_conf$lower)
forecast_conf$upper <- round(forecast_conf$upper)
forecast_conf$mean <- round(forecast_conf$mean)
# forecast the number of deaths
forecast_death <- forecast(auto.arima(myts_death), h = 30, level = 95)
# round the numbers
forecast_death$lower <- round(forecast_death$lower)
forecast_death$upper <- round(forecast_death$upper)
forecast_death$mean <- round(forecast_death$mean)
# create dates for the forecast
dates_ts <- seq(
  from = daily_cases$date[1],
  to = max(daily_cases$date) + 30,
  by = 1
)
# create forecast data.frame
forecast_df <- data.frame(
  date = dates_ts,
  confirmed = c(
    daily_cases$confirmed,
    rep(NA, length(dates_ts) - length(daily_cases$confirmed))
  ),
  fitted_conf = c(
    rep(NA, length(dates_ts) - length(forecast_conf$mean)),
    forecast_conf$mean
  ),
  upper95_conf = c(
    rep(NA, length(dates_ts) - length(forecast_conf$mean)),
    forecast_conf$upper
  ),
  lower95_conf = c(
    rep(NA, length(dates_ts) - length(forecast_conf$mean)),
    forecast_conf$lower
  ),
  deaths = c(
    daily_cases$deaths,
    rep(NA, length(dates_ts) - length(daily_cases$deaths))
  ),
  fitted_death = c(
    rep(NA, length(dates_ts) - length(forecast_death$mean)),
    forecast_death$mean
  ),
  upper95_death = c(
    rep(NA, length(dates_ts) - length(forecast_death$mean)),
    forecast_death$upper
  ),
  lower95_death = c(
    rep(NA, length(dates_ts) - length(forecast_death$mean)),
    forecast_death$lower
  )
)