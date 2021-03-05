# get data for the last 14 days
daily_cases3 <- daily_cases2[
  daily_cases2$date %in% c(max(
    daily_cases2$date),
    max(daily_cases2$date) - 14
  ),
]
# remove cruise ships
daily_cases3 <- daily_cases3[
  daily_cases3$`Country/Region` != "Diamond Princess",
]
daily_cases3 <- daily_cases3[, .(
  confirmed = sum(confirmed),
  deaths = sum(deaths)
), keyby = .(date, `Country/Region`)]
# split the data by day
splitted <- split(daily_cases3, daily_cases3$date)
# calculate the increases
df <- data.table(
  country = splitted[[1]]$`Country/Region`,
  increase = (splitted[[2]]$confirmed / splitted[[1]]$confirmed) * 100 - 100,
  old = splitted[[1]]$confirmed,
  new = splitted[[2]]$confirmed
)

df <- df[!is.infinite(df$increase), ]
df$increase <- round(df$increase)
# get countries with the highest increase
df_most <- df[
  df$increase %in% df[order(
    df$increase, decreasing = TRUE
  ), ][1:15, ]$increase,
]
df_most <- df_most[order(df_most$increase, decreasing = TRUE), ]
# set some limit
if (any(df_most$old[1:3] < 25)) {
  limit <- 250
} else if (any(df_most$old[1:3] < 50)) {
  limit <- 500
} else {
  limit <- round_any(mean(df_most$old), 10000)
}
df_limit <- df[df$old >= limit, ]
# get countries with the highest increase
df_limit_most <- df_limit[
  df_limit$increase %in% df_limit[order(
    df_limit$increase, decreasing = TRUE
  ), ][1:15, ]$increase, 
]
df_limit_most <- df_limit_most[
  order(df_limit_most$increase, decreasing = TRUE), ]
# get countries with the lowest increase
df_limit_few <- df_limit[
  df_limit$increase %in% df_limit[order(
    df_limit$increase
  ), ][1:15, ]$increase,
]
df_limit_few <- df_limit_few[order(df_limit_few$increase), ]
df_few <- df[df$increase %in% df[order(df$increase), ][1:15, ]$increase, ]
df_few <- df_few[order(df_few$increase), ]
