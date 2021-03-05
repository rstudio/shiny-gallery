# load lockdown data
locked <- read_csv("https://raw.githubusercontent.com/nicoFhahn/covid_shiny/master/data/locked.csv")
# we want to calculate the growth rate for each of the 14 days prior to the lockdown
locked_days <- daily_cases2[
  daily_cases2$date %in% seq(min(locked$date) - 15, max(daily_cases2$date), by = 1),
]
# join the daily cases by the data
locked_days <- right_join(locked_days, locked, by = "Country/Region")
locked_days <- locked_days[locked_days$lockdown != "parts_special", ]
# throw away to old data
locked_days <- locked_days[locked_days$date.x + 15 >= locked_days$date.y, ]
# split by the type of lockdown
locked_split <- split(locked_days, locked_days$lockdown)
change <- lapply(locked_split, function(x) {
  # get data ahead of the lockdown
  pre <- x[x$date.x < x$date.y, ]
  # order it
  pre <- pre[order(pre$`Country/Region`), ]
  # get number of day (1-14)
  pre$day <- unlist(lapply(as.numeric(table(pre$`Country/Region`) - 1), seq, from = 0, by = 1))
  # calculate the growth
  pre$growth_conf <- pre$confirmed / lag(pre$confirmed, default = first(pre$confirmed))
  # throw away growth on day 0, infinite and nans
  pre <- pre[pre$day != 0, ]
  pre <- pre[!is.infinite(pre$growth_conf), ]
  pre <- pre[!is.nan(pre$growth_conf), ]
  # calculate mean growth rate by country
  pre <- pre %>%
    group_by(`Country/Region`) %>%
    summarise(
      growth = mean(growth_conf, na.rm = TRUE)
    )
  # do the same again for the days after
  post <- x[x$date.x >= x$date.y - 1, ]
  post <- post[order(post$`Country/Region`), ]
  post$day <- unlist(lapply(as.numeric(table(post$`Country/Region`) - 1), seq, from = 0, by = 1))
  post$growth_conf <- post$confirmed / lag(post$confirmed, default = first(post$confirmed))
  post <- post[post$day != 0, ]
  post <- post[!is.infinite(post$growth_conf), ]
  post <- post[!is.nan(post$growth_conf), ]
  post <- post %>%
    group_by(`Country/Region`) %>%
    summarise(
      growth = mean(growth_conf, na.rm = TRUE)
    )
  list(
    data.frame(
      pre_lockdown = mean(pre$growth),
      post_lockdown = mean(post$growth)
    ),
    pre$growth
  )
})
# get the countries with no lockdown
no_lock <- daily_cases2[!daily_cases2$`Country/Region` %in% locked$`Country/Region`, ]
# order them
no_lock <- no_lock[order(no_lock$`Country/Region`), ]
# set the day
no_lock$day <- unlist(lapply(as.numeric(table(no_lock$`Country/Region`) - 1), seq, from = 0, by = 1))
# calculate the growth rates
no_lock$growth_conf <- no_lock$confirmed / lag(no_lock$confirmed, default = first(no_lock$confirmed))
# throw away growth on day 0, infinite and nans
no_lock <- no_lock[no_lock$day != 0, ]
no_lock <- no_lock[!is.infinite(no_lock$growth_conf), ]
no_lock <- no_lock[!is.nan(no_lock$growth_conf), ]
# calculate for each coutnry
no_lock <- no_lock %>%
  group_by(`Country/Region`) %>%
  summarise(
    growth = mean(growth_conf, na.rm = TRUE)
  )
# calculate the mean growth rate in countries with no lockdown
# and countries with a lockdown but prior to it
rate <- mean(c(no_lock$growth, unlist(lapply(change, function(x) x[[2]]))))
change_df <- Reduce(rbind, lapply(change, function(x) x[[1]]))
# create a data frame
lock_df <- data.frame(
  lockdown = c(unlist(lapply(names(change), rep, 2)), "No lockdown"),
  infection_rate = round(unlist(c(change_df[1, ], change_df[2, ], change_df[3, ], rate)), 2),
  time = c(rep(c("Pre or no lockdown", "Post lockdown"), 3), "Pre or no lockdown")
)
# edit some variables for plotting purposes
lock_df$lockdown <- as.character(lock_df$lockdown)
lock_df$lockdown[lock_df$lockdown == "Yes"] <- "Full lockdown"
lock_df$lockdown[lock_df$lockdown == "Partial"] <- "Partial lockdown"
lock_df$lockdown[lock_df$lockdown == "parts"] <- "Lockdown in parts of country"
lock_df$lockdown <- ordered(lock_df$lockdown, levels = c(
  "No lockdown", "Lockdown in parts of country", "Partial lockdown", "Full lockdown"
))
lock_df$time <- ordered(lock_df$time, levels = c("Pre or no lockdown", "Post lockdown"))
lock_df <- lock_df[order(lock_df$lockdown), ]

# create a sf data frame that contains polygon and type of lockdown
locked_2 <- locked
colnames(locked_2)[1] <- "ADMIN"
missing_frame <- data.frame(
  ADMIN = countries$ADMIN[!countries$ADMIN %in% locked_2$ADMIN],
  lockdown = "No lockdown",
  date = NA
)
locked_2 <- rbind(locked_2, missing_frame)
locked_countries <- right_join(countries, locked_2, by = "ADMIN")
locked_countries <- locked_countries[locked_countries$ADMIN != "Antarctica", ]
locked_countries$lockdown[locked_countries$lockdown == "Yes"] <- "Full lockdown"
locked_countries$lockdown[locked_countries$lockdown == "Partial"] <- "Partial lockdown"
locked_countries$lockdown[locked_countries$lockdown == "parts"] <- "Lockdown in parts of country"
locked_countries$lockdown[locked_countries$lockdown == "parts_special"] <- "Lockdown in parts of country"
locked_countries$lockdown <- ordered(locked_countries$lockdown, levels = c(
  "No lockdown", "Lockdown in parts of country", "Partial lockdown", "Full lockdown"
))
pal <- colorFactor(c("#161616", "#EABF00", "#F26D00", "#FF003F"), domain = locked_countries$lockdown)
