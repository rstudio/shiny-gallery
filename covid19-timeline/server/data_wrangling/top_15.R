# get countries with most confirmed cases
top15_confirmed <- today[today$confirmed %in% sort(today$confirmed, decreasing = TRUE)[1:15], ]
top15_confirmed <- top15_confirmed[order(top15_confirmed$confirmed, decreasing = TRUE), ]
# get countries with most deaths
top15_deaths <- today[today$confirmed %in% sort(today$confirmed, decreasing = TRUE)[1:15], ]
top15_deaths <- top15_deaths[order(top15_deaths$deaths, decreasing = TRUE), ]
# spatial intersection to calculate per capita stats
# we don't want countries
# today_inter <- st_intersection(countries[seq_len(nrow(countries)), ], today)
countries_sf <- st_as_sf(countries)
today_sf <- st_as_sf(today, crs = 4326)
today_inter <- st_intersection(countries_sf[seq_len(nrow(countries)), ], today_sf)
# calculate per capita stats
per_capita <- data.table(
  country = today_inter$Country.Region,
  confirmed = today_inter$confirmed,
  confirmed_per_capita = round(today_inter$confirmed / today_inter$POP_EST, 5) * 1000,
  deaths = today_inter$deaths,
  deaths_per_capita = round(today_inter$deaths / today_inter$POP_EST, 5) * 1000
)
# get the top 15 for both again
top15_confirmed_per_capita <- per_capita[per_capita$confirmed_per_capita %in% sort(per_capita$confirmed_per_capita, decreasing = TRUE)[1:15], ]
top15_deaths_per_capita <- per_capita[per_capita$deaths_per_capita %in% sort(per_capita$deaths_per_capita, decreasing = TRUE)[1:15], ]
top15_confirmed_per_capita <- top15_confirmed_per_capita[order(top15_confirmed_per_capita$confirmed_per_capita, decreasing = TRUE), ]
top15_deaths_per_capita <- top15_deaths_per_capita[order(top15_deaths_per_capita$deaths_per_capita, decreasing = TRUE), ]
