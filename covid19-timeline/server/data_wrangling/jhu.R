# download the data
confirmed <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
confirmed[confirmed$`Country/Region` == "Israel", 5:ncol(confirmed)] <- confirmed[confirmed$`Country/Region` == "Israel", 5:ncol(confirmed)] + confirmed[confirmed$`Country/Region` == "West Bank and Gaza", 5:ncol(confirmed)]
confirmed <- confirmed[confirmed$`Country/Region` != "West Bank and Gaza", ]
# now a whole bunch of preprocessing
# get the cases for the first day
confirmed_long <- confirmed[, 1:5]
# i hate dates and found no easier way to do this even though i know there is
date <- str_split(colnames(confirmed)[5], "/")[[1]]
date_new <- paste(ifelse(nchar(date[1]) == 2, date[1], paste(0, date[1], sep = "")),
  ifelse(nchar(date[2]) == 2, date[2], paste(0, date[2], sep = "")),
  paste("20", date[3], sep = ""),
  sep = "/"
)
confirmed_long$date <- as.Date(date_new, format = "%m/%d/%Y")
colnames(confirmed_long)[5] <- "confirmed"
# "slowly" transform into a long data set. once again better ways exist but yeah
confirmed_long2 <- lapply(6:ncol(confirmed), function(i, ...) {
  confirmed_new <- subset(confirmed, select = c(1:4, i))
  colnames(confirmed_new)[5] <- "confirmed"
  date <- str_split(colnames(confirmed)[i], "/")[[1]]
  date_new <- paste(ifelse(nchar(date[1]) == 2, date[1], paste(0, date[1], sep = "")),
    ifelse(nchar(date[2]) == 2, date[2], paste(0, date[2], sep = "")),
    paste("20", date[3], sep = ""),
    sep = "/"
  )
  confirmed_new$date <- as.Date(date_new, format = "%m/%d/%Y")
  confirmed_new
})
confirmed_long <- rbind(confirmed_long, rbindlist(confirmed_long2))
# old death
deaths <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
deaths[deaths$`Country/Region` == "Israel", 5:ncol(deaths)] <- deaths[deaths$`Country/Region` == "Israel", 5:ncol(deaths)] + deaths[deaths$`Country/Region` == "West Bank and Gaza", 5:ncol(deaths)]
deaths <- deaths[deaths$`Country/Region` != "West Bank and Gaza", ]
if (any(!confirmed$`Country/Region` %in% deaths$`Country/Region`)) {
  frames <- deaths[1:sum(!confirmed$`Country/Region` %in% deaths$`Country/Region`), ]
  frames$`Province/State` <- confirmed$`Province/State`[
    !confirmed$`Country/Region` %in% deaths$`Country/Region`
  ]
  frames$`Country/Region` <- confirmed$`Country/Region`[
    !confirmed$`Country/Region` %in% deaths$`Country/Region`
  ]
  frames$Lat <- confirmed$Lat[
    !confirmed$`Country/Region` %in% deaths$`Country/Region`
  ]
  frames$Long <- confirmed$Long[
    !confirmed$`Country/Region` %in% deaths$`Country/Region`
  ]
  frames[
    seq_len(
      sum(!confirmed$`Country/Region` %in% deaths$`Country/Region`)
    ),
    5:ncol(frames)
  ] <- 0
  deaths <- rbind(deaths, frames)
}

deaths_long <- deaths[, 1:5]
colnames(deaths_long)[5] <- "deaths"
deaths_long$date <- as.Date(date_new, format = "%m/%d/%Y")
deaths_long2 <- lapply(6:ncol(deaths), function(i, ...) {
  deaths_new <- subset(deaths, select = c(1:4, i))
  colnames(deaths_new)[5] <- "deaths"
  date <- str_split(colnames(deaths)[i], "/")[[1]]
  date_new <- paste(ifelse(nchar(date[1]) == 2, date[1], paste(0, date[1], sep = "")),
    ifelse(nchar(date[2]) == 2, date[2], paste(0, date[2], sep = "")),
    paste("20", date[3], sep = ""),
    sep = "/"
  )
  deaths_new$date <- as.Date(date_new, format = "%m/%d/%Y")
  deaths_new
})

deaths_long <- rbind(deaths_long, rbindlist(deaths_long2))

recovered <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recovered[recovered$`Country/Region` == "Israel", 5:ncol(recovered)] <- recovered[recovered$`Country/Region` == "Israel", 5:ncol(recovered)] + recovered[recovered$`Country/Region` == "West Bank and Gaza", 5:ncol(recovered)]
recovered <- recovered[recovered$`Country/Region` != "West Bank and Gaza", ]
if (any(!confirmed$`Country/Region` %in% recovered$`Country/Region`)) {
  frames <- recovered[1:sum(!confirmed$`Country/Region` %in% recovered$`Country/Region`), ]
  frames$`Province/State` <- confirmed$`Province/State`[
    !confirmed$`Country/Region` %in% recovered$`Country/Region`
  ]
  frames$`Country/Region` <- confirmed$`Country/Region`[
    !confirmed$`Country/Region` %in% recovered$`Country/Region`
  ]
  frames$Lat <- confirmed$Lat[
    !confirmed$`Country/Region` %in% recovered$`Country/Region`
  ]
  frames$Long <- confirmed$Long[
    !confirmed$`Country/Region` %in% recovered$`Country/Region`
  ]
  frames[
    seq_len(
      sum(!confirmed$`Country/Region` %in% recovered$`Country/Region`)
    ),
    5:ncol(frames)
  ] <- 0
  recovered <- rbind(recovered, frames)
}

recovered_long <- recovered[, 1:5]
colnames(recovered_long)[5] <- "recovered"
recovered_long$date <- as.Date(date_new, format = "%m/%d/%Y")
recovered_long2 <- lapply(6:ncol(recovered), function(i, ...) {
  recovered_new <- subset(recovered, select = c(1:4, i))
  colnames(recovered_new)[5] <- "recovered"
  date <- str_split(colnames(recovered)[i], "/")[[1]]
  date_new <- paste(ifelse(nchar(date[1]) == 2, date[1], paste(0, date[1], sep = "")),
    ifelse(nchar(date[2]) == 2, date[2], paste(0, date[2], sep = "")),
    paste("20", date[3], sep = ""),
    sep = "/"
  )
  recovered_new$date <- as.Date(date_new, format = "%m/%d/%Y")
  recovered_new
})
recovered_long <- rbind(recovered_long, rbindlist(recovered_long2))

canada_confirmed <- confirmed_long[confirmed_long$`Country/Region` == "Canada", ][, .(
  `Province/State` = unique(`Country/Region`),
  `Country/Region` = unique(`Country/Region`),
  confirmed = sum(confirmed),
  Lat = mean(Lat),
  Long = mean(Long),
  date = unique(date)
), keyby = .(date, `Country/Region`)][, 3:8]

canada_deaths <- deaths_long[deaths_long$`Country/Region` == "Canada", ][, .(
  `Province/State` = unique(`Country/Region`),
  `Country/Region` = unique(`Country/Region`),
  Lat = mean(Lat),
  Long = mean(Long),
  deaths = sum(deaths),
  date = unique(date)
), keyby = .(date, `Country/Region`)][, 3:8]

confirmed_long <- rbind(confirmed_long[confirmed_long$`Country/Region` != "Canada"], canada_confirmed)
deaths_long <- rbind(deaths_long[deaths_long$`Country/Region` != "Canada"], canada_deaths)

confirmed_long <- confirmed_long[order(confirmed_long$date, confirmed_long$`Country/Region`, confirmed_long$`Province/State`), ]
deaths_long <- deaths_long[order(deaths_long$date, deaths_long$`Country/Region`, deaths_long$`Province/State`), ]
recovered_long <- recovered_long[order(recovered_long$date, recovered_long$`Country/Region`, recovered_long$`Province/State`), ]

confirmed_long[, deaths := deaths_long[, "deaths"]]
confirmed_long[, recovered := recovered_long[, "recovered"]]
confirmed_long[confirmed_long$`Country/Region` == "Canada", ]$Lat <- 53.91469
confirmed_long[confirmed_long$`Country/Region` == "Canada", ]$Long <- -106.48272
confirmed_long[confirmed_long$`Country/Region` == "Holy See", ]$`Country/Region` <- "Vatican"
corona <- confirmed_long
# add a day 0 frame
rows <- seq_len(length(unique(paste(corona$`Country/Region`, corona$`Province/State`))))
corona_0 <- corona[rows, ]
corona_0$confirmed <- 0
corona_0$deaths <- 0
corona_0$recovered <- 0
corona_0$date <- min(corona$date) - 1
corona <- rbind(corona_0, corona)
# save the first and last date for later
dates <- c(min(corona$date), max(corona$date))
# turn into a geo data.frame
corona_sf <- data.table(st_as_sf(corona, coords = c("Long", "Lat"), crs = 4326))
# replace empty province with the country/region
corona_sf$`Province/State`[is.na(corona_sf$`Province/State`)] <- corona_sf$`Country/Region`[is.na(corona_sf$`Province/State`)]
