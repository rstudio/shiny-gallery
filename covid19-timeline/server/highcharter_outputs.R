output$highcharter_1 <- renderHighchart({
  highchart() %>%
    hc_yAxis_multiples(
      list(title = list(text = "Number of cases in China")),
      list(
        opposite = TRUE,
        title = list(text = "Number of cases outside China")
      )
    ) %>%
    hc_xAxis(
      categories = january$date, title = list(text = "Date"),
      gridLineWidth = 0
    ) %>%
    hc_add_series(
      name = "Confirmed cases in China  ",
      data = january$confirmed[january$`Country/Region` == "China"],
      color = "#5BC0EB"
    ) %>%
    hc_add_series(
      name = "Confirmed cases outside China",
      data = january$confirmed[january$`Country/Region` != "China"],
      yAxis = 1,
      color = "#FDE74C"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed cases in January 2020") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(align = "left") %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE))
    )
})

output$highcharter_2 <- renderHighchart({
  highchart() %>%
    hc_xAxis(
      categories = unique(february$date),
      gridLineWidth = 0,
      title = list(text = "Date")
    ) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      name = "Italy",
      data = february$confirmed[february$`Country/Region` == "Italy"],
      color = "#F8333C"
    ) %>%
    hc_add_series(
      name = "Iran",
      data = february$confirmed[february$`Country/Region` == "Iran"],
      color = "#44AF69"
    ) %>%
    hc_add_series(
      name = "South Korea",
      data = february$confirmed[february$`Country/Region` == "Korea, South"],
      color = "#88CCF1"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = "Number of confirmed cases at the end of February 2020"
    ) %>%
    hc_legend(align = "left") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE))
    )
})

output$highcharter_3 <- renderHighchart({
  highchart() %>%
    hc_xAxis(
      categories = gspc$date, title = list(text = "Date"),
      gridLineWidth = 0,
      plotLines = list(
        list(
          label = list(text = "February 27, 2020"),
          color = "#FCFCFC",
          width = 2,
          value = match(as.Date("2020-02-27"), gspc$date)
        ),
        list(
          label = list(text = "2008 financial crisis"),
          color = "#FCFCFC",
          width = 2,
          value = match(as.Date("2008-10-09"), gspc$date)
        )
      )
    ) %>%
    hc_yAxis(title = list(text = "Closing value")) %>%
    hc_add_series(
      name = "Closing value",
      data = gspc$GSPC.Close,
      color = "#8AEA9A"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Closing value of the S&P 500") %>%
    hc_legend(align = "left") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE))
    )
})

output$highcharter_4 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = feb_euro$Country.Region,
      gridLineWidth = 0,
      title = list(text = "Country")
    ) %>%
    hc_add_series(
      data = feb_euro$confirmed,
      name = "Confirmed cases",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = "Number of confirmed cases at the end of February 2020"
    ) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_5 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = top10feb$`Country/Region`,
      gridLineWidth = 0,
      title = list(text = "Country")
    ) %>%
    hc_add_series(
      data = top10feb$confirmed,
      name = "Confirmed cases",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = "Number of confirmed cases at the end of February 2020"
    ) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_6 <- renderHighchart({
  highchart() %>%
    hc_xAxis(
      categories = pandemic$date,
      gridLineWidth = 0,
      type = "date",
      title = list(text = "Date"),
      plotLines = list(
        list(
          label = list(text = "2 weeks prior"),
          color = "#FCFCFC",
          width = 2,
          value = 13
        ),
        list(
          label = list(text = "Declared a pandemic"),
          color = "#FCFCFC",
          width = 2,
          value = 27
        )
      )
    ) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      data = pandemic$confirmed,
      name = "Confirmed cases",
      color = "#FDE74C"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = "Number of confirmed cases outside China over the last 28 days"
    ) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE))
    )
})

output$highcharter_7 <- renderHighchart({
  germany <- daily_cases2[daily_cases2$`Country/Region` == "Germany", c(1, 3, 4)]
  highchart() %>%
    hc_xAxis(categories = germany$date) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      data = germany$confirmed,
      name = "Confirmed cases",
      color = "#EE6352"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed cases in Germany") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE) %>%
    hc_xAxis(
      title = list(text = "Date"),
      gridLineWidth = 0,
      plotLines = list(
        list(
          label = list(text = "Contact ban", verticalAlign = "middle"),
          color = "#FCFCFC",
          width = 2,
          value = 61
        )
      )
    ) %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE))
    )
})

output$highcharter_8 <- renderHighchart({
  highchart() %>%
    hc_xAxis(
      categories = daily_cases$date,
      gridLineWidth = 0, title = list(text = "Date")
    ) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      data = daily_cases$confirmed,
      name = "Confirmed cases",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Number of confirmed cases") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE))
    )
})

output$highcharter_9 <- renderHighchart({
  daily_cases$increase <- c(
    0, daily_cases$confirmed[-1] -
      daily_cases$confirmed
  )[seq_len(nrow(daily_cases))]
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = daily_cases$date,
      gridLineWidth = 0,
      title = list(text = "Date")
    ) %>%
    hc_add_series(
      data = daily_cases$increase,
      name = "New infections",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Daily number of new infections") %>%
    hc_yAxis(title = list(text = "Confirmed infections")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_10 <- renderHighchart({
  today <- daily_cases2[daily_cases2$date == max(daily_cases2$date), ]
  most_infected <- daily_cases2[
    daily_cases2$`Country/Region` == as.character(
      today[today$confirmed == max(today$confirmed), 2]
    ),
  ]
  country <- unique(most_infected$`Country/Region`)
  if (country == "US") {
    country <- "the United States"
  }
  highchart() %>%
    hc_xAxis(
      categories = most_infected$date, title = list(text = "Date"),
      gridLineWidth = 0
    ) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      data = most_infected$confirmed,
      name = "Confirmed cases",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = paste(
        "Number of confirmed cases in",
        country
      )
    ) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE))
    )
})

output$highcharter_11 <- renderHighchart({
  today <- daily_cases2[daily_cases2$date == max(daily_cases2$date), ]
  most_infected <- daily_cases2[
    daily_cases2$`Country/Region` == as.character(
      today[today$confirmed == max(today$confirmed), 2]
    ),
  ]
  most_infected$increase <- c(
    0, most_infected$confirmed[-1] -
      most_infected$confirmed
  )[seq_len(nrow(most_infected))]
  country <- unique(most_infected$`Country/Region`)
  if (country == "US") {
    country <- "the United States"
  }
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = most_infected$date,
      gridLineWidth = 0,
      title = list(text = "Date")
    ) %>%
    hc_add_series(
      data = most_infected$increase,
      name = "Confirmed cases",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = paste(
        "Daily number of new infections in",
        country
      )
    ) %>%
    hc_yAxis(title = list(text = "Confirmed infections")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_12 <- renderHighchart({
  most_death <- daily_cases2[
    daily_cases2$`Country/Region` == as.character(
      today[today$deaths == max(today$deaths), 2]
    ),
  ]
  highchart() %>%
    hc_xAxis(
      categories = most_death$date, title = list(text = "Date"),
      gridLineWidth = 0
    ) %>%
    hc_yAxis(title = list(text = "Deaths")) %>%
    hc_add_series(
      data = most_death$deaths,
      name = "Deaths",
      color = "#fb5a19"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = paste(
        "Number of deaths in",
        unique(most_death$`Country/Region`)
      )
    ) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE))
    )
})

output$highcharter_13 <- renderHighchart({
  most_death <- daily_cases2[
    daily_cases2$`Country/Region` == as.character(
      today[today$deaths == max(today$deaths), 2]
    ),
  ]
  most_death$increase <- c(
    0, most_death$deaths[-1] - most_death$deaths
  )[seq_len(nrow(most_death))]
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = most_death$date,
      gridLineWidth = 0,
      title = list(text = "Date")
    ) %>%
    hc_add_series(
      data = most_death$increase,
      name = "Deaths",
      color = "#fb5a19"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = paste(
        "Daily number of deaths in",
        unique(most_death$`Country/Region`)
      )
    ) %>%
    hc_yAxis(title = list(text = "Deaths")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_14 <- renderHighchart({
  highchart() %>%
    hc_xAxis(
      categories = unique(daily_cases2$date),
      gridLineWidth = 0,
      title = list(text = "Date"),
      plotLines = list(
        list(
          label = list(text = "U.S. leads in confirmed cases"),
          color = "#FCFCFC",
          width = 2,
          value = 65
        )
      )
    ) %>%
    hc_yAxis(title = list(text = "Confirmed cases")) %>%
    hc_add_series(
      name = "Brazil",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "Brazil"],
      color = "#FFBD00"
    ) %>%
    hc_add_series(
      name = "China",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "China"],
      color = "#5BC0EB"
    ) %>%
    hc_add_series(
      name = "France",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "France"],
      color = "#AF3E4D"
    ) %>%
    hc_add_series(
      name = "Germany",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "Germany"],
      color = "#EE6352"
    ) %>%
    hc_add_series(
      name = "India",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "India"],
      color = "#F4F4F8"
    ) %>%
    hc_add_series(
      name = "Italy",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "Italy"],
      color = "#F8333C"
    ) %>%
    hc_add_series(
      name = "Russia",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "Russia"],
      color = "#00a896"
    ) %>%
    hc_add_series(
      name = "Spain",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "Spain"],
      color = "#F2CCC3"
    ) %>%
    hc_add_series(
      name = "UK",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "United Kingdom"],
      color = "#D4A0A7"
    ) %>%
    hc_add_series(
      name = "USA",
      data = daily_cases2$confirmed[daily_cases2$`Country/Region` == "US"],
      color = "#97C667"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(
      text = "Number of confirmed
      cases"
    ) %>%
    hc_legend(align = "left") %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE))
    )
})


output$highcharter_15 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = top15_confirmed$`Country/Region`,
      gridLineWidth = 0,
      title = list(text = "Country")
    ) %>%
    hc_add_series(
      data = top15_confirmed$confirmed,
      name = "Confirmed infections",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Countries with the most infections (as of today)") %>%
    hc_yAxis(title = list(text = "Confirmed infections")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_16 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = top15_deaths$`Country/Region`,
      gridLineWidth = 0,
      title = list(text = "Country")
    ) %>%
    hc_add_series(
      data = top15_deaths$deaths,
      name = "Deaths",
      color = "#fb5a19"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Countries with the most deaths (as of today)") %>%
    hc_yAxis(title = list(text = "Deaths")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_17 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = top15_confirmed_per_capita$country,
      gridLineWidth = 0,
      title = list(text = "Country")
    ) %>%
    hc_add_series(
      data = top15_confirmed_per_capita$confirmed_per_capita,
      name = "Infection rate per 1 000",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Infection rate per 1 000 (as of today)") %>%
    hc_yAxis(title = list(text = "Infection rate per 1 000")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_18 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = top15_deaths_per_capita$country,
      gridLineWidth = 0,
      title = list(text = "Country")
    ) %>%
    hc_add_series(
      data = top15_deaths_per_capita$deaths_per_capita,
      name = "Death rate per 1 000",
      color = "#fb5a19"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Death rate per 1 000 (as of today)") %>%
    hc_yAxis(title = list(text = "Death rate per 1 000")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_19 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = df_most$country,
      gridLineWidth = 0,
      title = list(text = "Country")
    ) %>%
    hc_add_series(
      data = df_most$increase,
      name = "Increase in %",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = "Highest increase in new infections in the last 14 days (at least 1 infection)") %>%
    hc_yAxis(title = list(text = "Increase in %")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_20 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = df_limit_most$country,
      gridLineWidth = 0,
      title = list(text = "Country")
    ) %>%
    hc_add_series(
      data = df_limit_most$increase,
      name = "Increase in %",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = paste("Most new infections in the last 14 days (at least", limit, "infections)")) %>%
    hc_yAxis(title = list(text = "Increase in %")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_21 <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_xAxis(
      categories = df_limit_few$country,
      gridLineWidth = 0,
      title = list(text = "Country")
    ) %>%
    hc_add_series(
      data = df_limit_few$increase,
      name = "Increase in %",
      color = "#00bb8b"
    ) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_title(text = paste("Fewest infections in the last 14 days (at least", limit, "infections)")) %>%
    hc_yAxis(title = list(text = "Increase in %")) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(enabled = FALSE)
})

output$highcharter_22 <- renderHighchart({
  highchart() %>%
    hc_add_series(forecast_df$confirmed, name = "Original values", color = "#00bb8b") %>%
    hc_xAxis(title = list(text = "Date"), categories = forecast_df$date) %>%
    hc_add_series(as.matrix(forecast_df[, 4:5]),
      type = "arearange", color = "#A2DFDE",
      fillOpacity = 0.3,
      name = "95% conf."
    ) %>%
    hc_add_series(forecast_df$fitted_conf, name = "Mean prediction", color = "#00A9A5") %>%
    hc_xAxis(title = list(text = "Date"), categories = forecast_df$date) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_yAxis(title = list(text = "Confirmed cases"), min = 0) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(align = "left") %>%
    hc_xAxis(
      title = list(text = "Date"),
      gridLineWidth = 0,
      plotLines = list(
        list(
          label = list(text = "Today"),
          color = "#FCFCFC",
          width = 2,
          value = nrow(daily_cases)
        )
      )
    ) %>%
    hc_title(text = "Predicted number of confirmed cases over the next 30 days") %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE)),
      arearange = list(marker = list(enabled = FALSE))
    )
})

output$highcharter_23 <- renderHighchart({
  highchart() %>%
    hc_add_series(forecast_df$deaths, name = "Original values", color = "#fb5a19") %>%
    hc_xAxis(title = list(text = "Date"), categories = forecast_df$date) %>%
    hc_add_series(as.matrix(forecast_df[, 8:9]),
      type = "arearange", color = "#F3B1B5",
      fillOpacity = 0.3,
      name = "95% conf."
    ) %>%
    hc_add_series(forecast_df$fitted_death, name = "Mean prediction", color = "#DF2935") %>%
    hc_xAxis(title = list(text = "Date"), categories = forecast_df$date) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_yAxis(title = list(text = "Deaths"), min = 0) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(align = "left") %>%
    hc_xAxis(
      title = list(text = "Date"),
      gridLineWidth = 0,
      plotLines = list(
        list(
          label = list(text = "Today"),
          color = "#FCFCFC",
          width = 2,
          value = nrow(daily_cases)
        )
      )
    ) %>%
    hc_title(text = "Predicted number of deaths over the next 30 days") %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE)),
      arearange = list(marker = list(enabled = FALSE))
    )
})


output$highcharter_24 <- renderHighchart({
  hchart(
    lock_df, "column",
    hcaes(x = lockdown, y = infection_rate, group = time),
    color = c("#65DEF1", "#D7263D")
  ) %>%
    hc_xAxis(
      title = list(text = "Lockdown status"),
      gridLineWidth = 0
    ) %>%
    hc_title(text = "Average daily growth rate before and after different types of lockdowns") %>%
    hc_yAxis(title = list(text = "Growth rate")) %>%
    hc_add_theme(hc_theme_monokai()) %>%
    hc_chart(backgroundColor = "#161616") %>%
    hc_legend(align = "left")
})
