# daterange <- c(as.Date("2020-01-22"), as.Date("2020-03-29"))
# update card on every click or changed date
observeEvent(list(
  input$date,
  input$mymap_click
), {
  if (!is.null(input$date)) {
    # get the selected dates
    daterange <- get_date()
    # daterange <- as.Date(c("2020-01-22", "2020-10-07"))
    print(daterange)
    # try and get the country
    country <- try(get_country(), silent = TRUE)
    print(country)
    corona_frame <- st_as_sf(corona_sf, crs = 4326)
    if (class(country) != "try-error") {
      # if a country was clicked, select the subset of corona data
      if (country != "world") {
        country_df <- st_as_sf(countries[countries$ADMIN == country, ], crs = 4326)
        if (country == "Israel") {
          corona_frame <- corona_frame[corona_frame$`Country/Region` == "Israel", ]
        } else {
          corona_frame <- corona_frame[unlist(st_contains(country_df, corona_frame)), ]
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
          corona_frame <- st_as_sf(corona_frame)
          corona_frame$geometry <- country_df$geometry
        }
      }
    }
    # get a frame between the two dates
    corona_frame <- corona_frame[corona_frame$date >= daterange[1] - 1 &
      corona_frame$date <= daterange[2], ]
    # get data for the last day
    corona_frame1 <- corona_frame[corona_frame$date == max(corona_frame$date), ]
    # get data for the first day
    corona_frame2 <- corona_frame[corona_frame$date == min(corona_frame$date), ]
    # calculate the number of caes in the timeframe
    corona_frame1$confirmed <- corona_frame1$confirmed -
      corona_frame2$confirmed
    corona_frame1$deaths <- corona_frame1$deaths - corona_frame2$deaths
    corona_frame <- corona_frame1
    if (any(corona_frame$`Province/State` == "")) {
      corona_frame$`Province/State` <- corona_frame$`Country/Region`
    }
    if (country == "Philippines") {
      linecol <- "#BAFF80"
    } else {
      linecol <- "#FF80ED"
    }
    ee <- FALSE
    if (country %in% c("India", "Philippines", "Vietnam", "Germany")) {
      if (country == "Philippines") {
        lng_easter <- 123.365708
        lat_egg <- 9.950766
        ee <- TRUE
      } else if (country == "India") {
        lng_easter <- 11.589492
        lat_egg <- 48.139782
        ee <- TRUE
      } else if (country == "Vietnam") {
        lng_easter <- 11.584852
        lat_egg <- 48.156953
        ee <- TRUE
      } else if (country == "Germany") {
        coords <- data.frame(lng = input$mymap_click$lng, lat = input$mymap_click$lat)
        # turn into points
        coords <- st_as_sf(coords, coords = c("lng", "lat"), crs = 4326)
        if (length(st_contains(aic, coords)[1][[1]]) > 0) {
          lng_easter <- -73.792036
          lat_egg <- 40.680398
          ee <- TRUE
        }
      }
    }
    # create the map
    if (class(country) != "try-error") {
      if (country != "world") {
        if (!ee) {
          country_df2 <- countries[countries$ADMIN != country, ]
          if (country != "Israel") {
            leafletProxy("mymap") %>%
              # remove stuff from the old map
              clearControls() %>%
              clearMarkers() %>%
              clearShapes() %>%
              clearPopups() %>%
              # add the country shapes
              addPolygons(
                data = st_as_sf(country_df2),
                weight = 0,
                color = "#000000",
                fillOpacity = 0,
                # highlight the outer lines
                highlightOptions = highlightOptions(
                  color = "#ffffff", opacity = 1, weight = 2, fillOpacity = 0,
                  sendToBack = TRUE
                )
              ) %>%
              addPolylines(
                data = st_cast(country_df, "MULTILINESTRING"),
                color = linecol,
                weight = 2
              ) %>%
              # add the corona data
              addCircles(
                data = st_centroid(st_as_sf(corona_frame, crs = 4326)[1, ]),
                fillOpacity = 0.5,
                radius = ~ sqrt(confirmed) * 250,
                color = "#3454D1",
                stroke = FALSE,
                # add labels
                label = paste(
                  corona_frame$`Province/State`, ":<br>",
                  "Confirmed cases: ", corona_frame$confirmed, "<br>",
                  "Recovered cases: ", corona_frame$recovered, "<br>",
                  "Deceased cases: ", corona_frame$deaths,
                  sep = ""
                ) %>% lapply(htmltools::HTML),
                # style the labels
                labelOptions = labelOptions(
                  style = list(
                    "font-family" = "Oswald",
                    "font-style" = "sans-serif",
                    "font-size" = "14px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  )
                )
              ) %>%
              # do the same again
              addCircles(
                data = st_centroid(st_as_sf(corona_frame, crs = 4326)[1, ]),
                fillOpacity = 0.5,
                radius = ~ sqrt(recovered) * 250,
                color = "#23F0C7",
                stroke = FALSE,
                label = paste(
                  corona_frame$`Province/State`, ":<br>",
                  "Confirmed cases: ", corona_frame$confirmed, "<br>",
                  "Recovered cases: ", corona_frame$recovered, "<br>",
                  "Deceased cases: ", corona_frame$deaths,
                  sep = ""
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                  style = list(
                    "font-family" = "Oswald",
                    "font-style" = "sans-serif",
                    "font-size" = "14px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  )
                )
              ) %>%
              # do the same again
              addCircles(
                data = st_centroid(st_as_sf(corona_frame, crs = 4326)[1, ]),
                fillOpacity = 0.5,
                radius = ~ sqrt(deaths) * 250,
                color = "#ED254E",
                stroke = FALSE,
                label = paste(
                  corona_frame$`Province/State`, ":<br>",
                  "Confirmed cases: ", corona_frame$confirmed, "<br>",
                  "Recovered cases: ", corona_frame$recovered, "<br>",
                  "Deceased cases: ", corona_frame$deaths,
                  sep = ""
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                  style = list(
                    "font-family" = "Oswald",
                    "font-style" = "sans-serif",
                    "font-size" = "14px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  )
                )
              )
          } else {
            leafletProxy("mymap") %>%
              # remove stuff from the old map
              clearControls() %>%
              clearMarkers() %>%
              clearShapes() %>%
              clearPopups() %>%
              # add the country shapes
              addPolygons(
                data = st_as_sf(country_df2),
                weight = 0,
                color = "#000000",
                fillOpacity = 0,
                # highlight the outer lines
                highlightOptions = highlightOptions(
                  color = "#ffffff", opacity = 1, weight = 2, fillOpacity = 0,
                  sendToBack = TRUE
                )
              ) %>%
              addPolygons(
                data = country_df,
                color = linecol,
                fillOpacity = 0,
                weight = 2
              ) %>%
              # add the corona data
              addCircles(
                data = st_centroid(st_as_sf(corona_frame, crs = 4326)[1, ]),
                fillOpacity = 0.5,
                radius = ~ sqrt(confirmed) * 250,
                color = "#3454D1",
                stroke = FALSE,
                # add labels
                label = paste(
                  corona_frame$`Province/State`, ":<br>",
                  "Confirmed cases: ", corona_frame$confirmed, "<br>",
                  "Recovered cases: ", corona_frame$recovered, "<br>",
                  "Deceased cases: ", corona_frame$deaths,
                  sep = ""
                ) %>% lapply(htmltools::HTML),
                # style the labels
                labelOptions = labelOptions(
                  style = list(
                    "font-family" = "Oswald",
                    "font-style" = "sans-serif",
                    "font-size" = "14px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  )
                )
              ) %>%
              # do the same again
              addCircles(
                data = st_centroid(st_as_sf(corona_frame, crs = 4326)[1, ]),
                fillOpacity = 0.5,
                radius = ~ sqrt(recovered) * 250,
                color = "#23F0C7",
                stroke = FALSE,
                label = paste(
                  corona_frame$`Province/State`, ":<br>",
                  "Confirmed cases: ", corona_frame$confirmed, "<br>",
                  "Recovered cases: ", corona_frame$recovered, "<br>",
                  "Deceased cases: ", corona_frame$deaths,
                  sep = ""
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                  style = list(
                    "font-family" = "Oswald",
                    "font-style" = "sans-serif",
                    "font-size" = "14px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  )
                )
              ) %>%
              # do the same again
              addCircles(
                data = st_centroid(st_as_sf(corona_frame, crs = 4326)[1, ]),
                fillOpacity = 0.5,
                radius = ~ sqrt(deaths) * 250,
                color = "#ED254E",
                stroke = FALSE,
                label = paste(
                  corona_frame$`Province/State`, ":<br>",
                  "Confirmed cases: ", corona_frame$confirmed, "<br>",
                  "Recovered cases: ", corona_frame$recovered, "<br>",
                  "Deceased cases: ", corona_frame$deaths,
                  sep = ""
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                  style = list(
                    "font-family" = "Oswald",
                    "font-style" = "sans-serif",
                    "font-size" = "14px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  )
                )
              )
          }
        } else {
          country_df2 <- countries[countries$ADMIN != country, ]
          leafletProxy("mymap") %>%
            # remove stuff from the old map
            clearControls() %>%
            clearMarkers() %>%
            clearShapes() %>%
            clearPopups() %>%
            # add the country shapes
            addPolygons(
              data = st_as_sf(country_df2),
              weight = 0,
              color = "#000000",
              fillOpacity = 0,
              # highlight the outer lines
              highlightOptions = highlightOptions(
                color = "#ffffff", opacity = 1, weight = 2, fillOpacity = 0,
                sendToBack = TRUE
              )
            ) %>%
            addPolylines(
              data = st_cast(country_df, "MULTILINESTRING"),
              color = linecol,
              weight = 2
            ) %>%
            # add the corona data

            # add the corona data
            addCircles(
              data = st_centroid(st_as_sf(corona_frame, crs = 4326)[1, ]),
              fillOpacity = 0.5,
              radius = ~ sqrt(confirmed) * 250,
              color = "#3454D1",
              stroke = FALSE,
              # add labels
              label = paste(
                corona_frame$`Province/State`, ":<br>",
                "Confirmed cases: ", corona_frame$confirmed, "<br>",
                "Recovered cases: ", corona_frame$recovered, "<br>",
                "Deceased cases: ", corona_frame$deaths,
                sep = ""
              ) %>% lapply(htmltools::HTML),
              # style the labels
              labelOptions = labelOptions(
                style = list(
                  "font-family" = "Oswald",
                  "font-style" = "sans-serif",
                  "font-size" = "14px",
                  "border-color" = "rgba(0,0,0,0.5)"
                )
              )
            ) %>%
            # do the same again
            addCircles(
              data = st_centroid(st_as_sf(corona_frame, crs = 4326)[1, ]),
              fillOpacity = 0.5,
              radius = ~ sqrt(recovered) * 250,
              color = "#23F0C7",
              stroke = FALSE,
              label = paste(
                corona_frame$`Province/State`, ":<br>",
                "Confirmed cases: ", corona_frame$confirmed, "<br>",
                "Recovered cases: ", corona_frame$recovered, "<br>",
                "Deceased cases: ", corona_frame$deaths,
                sep = ""
              ) %>% lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list(
                  "font-family" = "Oswald",
                  "font-style" = "sans-serif",
                  "font-size" = "14px",
                  "border-color" = "rgba(0,0,0,0.5)"
                )
              )
            ) %>%
            # do the same again
            addCircles(
              data = st_centroid(st_as_sf(corona_frame, crs = 4326)[1, ]),
              fillOpacity = 0.5,
              radius = ~ sqrt(deaths) * 250,
              color = "#ED254E",
              stroke = FALSE,
              label = paste(
                corona_frame$`Province/State`, ":<br>",
                "Confirmed cases: ", corona_frame$confirmed, "<br>",
                "Recovered cases: ", corona_frame$recovered, "<br>",
                "Deceased cases: ", corona_frame$deaths,
                sep = ""
              ) %>% lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list(
                  "font-family" = "Oswald",
                  "font-style" = "sans-serif",
                  "font-size" = "14px",
                  "border-color" = "rgba(0,0,0,0.5)"
                )
              )
            ) %>%
            addPulseMarkers(
              lng = lng_easter,
              lat = lat_egg,
              icon = makePulseIcon(heartbeat = 0.5, color = "#2E1184", iconSize = 3)
            )
        }
      } else {
        leafletProxy("mymap") %>%
          # remove stuff from the old map
          clearControls() %>%
          clearMarkers() %>%
          clearShapes() %>%
          clearPopups() %>%
          # add the country shapes
          addPolygons(
            data = st_as_sf(countries),
            weight = 0,
            color = "#000000",
            fillOpacity = 0,
            # highlight the outer lines
            highlightOptions = highlightOptions(
              color = "#ffffff", opacity = 1, weight = 2, fillOpacity = 0,
              sendToBack = TRUE
            )
          ) %>%
          addCircles(
            data = st_centroid(st_as_sf(corona_frame, crs = 4326)),
            fillOpacity = 0.5,
            radius = ~ sqrt(confirmed) * 250,
            color = "#3454D1",
            stroke = FALSE,
            # add labels
            label = paste(
              corona_frame$`Province/State`, ":<br>",
              "Confirmed cases: ", corona_frame$confirmed, "<br>",
              "Recovered cases: ", corona_frame$recovered, "<br>",
              "Deceased cases: ", corona_frame$deaths,
              sep = ""
            ) %>% lapply(htmltools::HTML),
            # style the labels
            labelOptions = labelOptions(
              style = list(
                "font-family" = "Oswald",
                "font-style" = "sans-serif",
                "font-size" = "14px",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            )
          ) %>%
          # do the same again
          addCircles(
            data = st_centroid(st_as_sf(corona_frame, crs = 4326)),
            fillOpacity = 0.5,
            radius = ~ sqrt(recovered) * 250,
            color = "#23F0C7",
            stroke = FALSE,
            label = paste(
              corona_frame$`Province/State`, ":<br>",
              "Confirmed cases: ", corona_frame$confirmed, "<br>",
              "Recovered cases: ", corona_frame$recovered, "<br>",
              "Deceased cases: ", corona_frame$deaths,
              sep = ""
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(
              style = list(
                "font-family" = "Oswald",
                "font-style" = "sans-serif",
                "font-size" = "14px",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            )
          ) %>%
          # do the same again
          addCircles(
            data = st_centroid(st_as_sf(corona_frame, crs = 4326)),
            fillOpacity = 0.5,
            radius = ~ sqrt(deaths) * 250,
            color = "#ED254E",
            stroke = FALSE,
            label = paste(
              corona_frame$`Province/State`, ":<br>",
              "Confirmed cases: ", corona_frame$confirmed, "<br>",
              "Recovered cases: ", corona_frame$recovered, "<br>",
              "Deceased cases: ", corona_frame$deaths,
              sep = ""
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(
              style = list(
                "font-family" = "Oswald",
                "font-style" = "sans-serif",
                "font-size" = "14px",
                "border-color" = "rgba(0,0,0,0.5)"
              )
            )
          )
      }
    } else {
      leafletProxy("mymap") %>%
        # remove stuff from the old map
        clearControls() %>%
        clearMarkers() %>%
        clearShapes() %>%
        clearPopups() %>%
        # add the country shapes
        addPolygons(
          data = st_as_sf(countries),
          weight = 0,
          color = "#000000",
          fillOpacity = 0,
          # highlight the outer lines
          highlightOptions = highlightOptions(
            color = "#ffffff", opacity = 1, weight = 2, fillOpacity = 0,
            sendToBack = TRUE
          )
        ) %>%
        # add the corona data
        addCircles(
          data = corona_frame,
          fillOpacity = 0.5,
          radius = ~ sqrt(confirmed) * 250,
          color = "#3454D1",
          stroke = FALSE,
          # add labels
          label = paste(
            corona_frame$`Province/State`, ":<br>",
            "Confirmed cases: ", corona_frame$confirmed, "<br>",
            "Recovered cases: ", corona_frame$recovered, "<br>",
            "Deceased cases: ", corona_frame$deaths,
            sep = ""
          ) %>% lapply(htmltools::HTML),
          # style the labels
          labelOptions = labelOptions(
            style = list(
              "font-family" = "Oswald",
              "font-style" = "sans-serif",
              "font-size" = "14px",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        ) %>%
        # add the corona data
        addCircles(
          data = corona_frame,
          fillOpacity = 0.5,
          radius = ~ sqrt(recovered) * 250,
          color = "#23F0C7",
          stroke = FALSE,
          # add labels
          label = paste(
            corona_frame$`Province/State`, ":<br>",
            "Confirmed cases: ", corona_frame$confirmed, "<br>",
            "Recovered cases: ", corona_frame$recovered, "<br>",
            "Deceased cases: ", corona_frame$deaths,
            sep = ""
          ) %>% lapply(htmltools::HTML),
          # style the labels
          labelOptions = labelOptions(
            style = list(
              "font-family" = "Oswald",
              "font-style" = "sans-serif",
              "font-size" = "14px",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        ) %>%
        # do the same again
        addCircles(
          data = corona_frame,
          fillOpacity = 0.5,
          radius = ~ sqrt(deaths) * 250,
          color = "#ED254E",
          stroke = FALSE,
          label = paste(
            corona_frame$`Province/State`, ":<br>",
            "Confirmed cases: ", corona_frame$confirmed, "<br>",
            "Recovered cases: ", corona_frame$recovered, "<br>",
            "Deceased cases: ", corona_frame$deaths,
            sep = ""
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions(
            style = list(
              "font-family" = "Oswald",
              "font-style" = "sans-serif",
              "font-size" = "14px",
              "border-color" = "rgba(0,0,0,0.5)"
            )
          )
        )
    }
  }
})

# update plotly on click or date
observeEvent(list(
  input$date,
  input$mymap_click
), {
  if (!is.null(input$date)) {
    # get the daterange
    daterange <- get_date()
    # get the country
    country <- try(get_country(), silent = TRUE)
    corona_frame <- st_as_sf(corona_sf, crs = 4326)
    if (class(country) != "try-error") {
      # if a country was clicked, select the subset of corona data
      if (country != "world") {
        country_df <- st_as_sf(countries[countries$ADMIN == country, ], crs = 4326)
        if (country == "Israel") {
          corona_frame <- corona_frame[corona_frame$`Country/Region` == "Israel", ]
        } else {
          corona_frame <- corona_frame[unlist(st_contains(country_df, corona_frame)), ]
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
      }
    }
    # remove geometry col
    corona_frame$geometry <- NULL
    # group the data
    corona_grouped <- corona_frame %>%
      group_by(date) %>%
      summarise(
        confirmed = sum(confirmed),
        deaths = sum(deaths),
        recovered = sum(recovered)
      )
    last_day <- corona_grouped[corona_grouped$date == daterange[1] - 1, ]
    corona_grouped <- corona_grouped[corona_grouped$date >= daterange[1] &
      corona_grouped$date <= daterange[2], ]
    colnames(corona_grouped) <- c("date", "confirmed", "deaths", "recovered")
    corona_grouped$confirmed <- corona_grouped$confirmed - last_day$confirmed
    corona_grouped$deaths <- corona_grouped$deaths - last_day$deaths
    corona_grouped$recovered <- corona_grouped$recovered - last_day$recovered
    # update the plot
    plotlyProxy("everything_plot", session) %>%
      # delete the old traces
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      # add new traces
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped$date,
        y = corona_grouped$deaths,
        name = "Deceased",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        fillcolor = "rgba(237,37,78,0.5)",
        hoverinfo = "text",
        text = paste(
          "Confirmed cases:", corona_grouped$confirmed, "<br>",
          "Recovered cases:", corona_grouped$recovered, "<br>",
          "Deceased cases:", corona_grouped$deaths
        ),
        line = list(color = "rgba(255, 115, 115, 1)")
      )) %>%
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped$date,
        y = corona_grouped$recovered - corona_grouped$deaths,
        name = "Recovered",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        fillcolor = "rgba(35,240,199,0.5)",
        hoverinfo = "text",
        text = paste(
          "Confirmed cases:", corona_grouped$confirmed, "<br>",
          "Recovered cases:", corona_grouped$recovered, "<br>",
          "Deceased cases:", corona_grouped$deaths
        ),
        line = list(color = "rgba(255, 115, 115, 1)")
      )) %>%
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped$date,
        y = corona_grouped$confirmed - corona_grouped$deaths - corona_grouped$recovered,
        name = "Confirmed",
        type = "scatter",
        mode = "none",
        stackgroup = "one",
        fillcolor = "rgba(52,84,209,0.5)",
        hoverinfo = "text",
        text = paste(
          "Confirmed cases:", corona_grouped$confirmed, "<br>",
          "Recovered cases:", corona_grouped$recovered, "<br>",
          "Deceased cases:", corona_grouped$deaths
        ),
        line = list(color = "rgba(255, 183, 51, 1)")
      )) %>%
      # define the layout
      plotlyProxyInvoke("relayout",
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          y = 100000
        ),
        yaxis = list(
          title = "Total cases",
          fixedrange = TRUE
        ),
        xaxis = list(fixedrange = TRUE, showspikes = TRUE, showgrid = FALSE)
      )
  }
})

observeEvent(list(
  input$date,
  input$mymap_click
), {
  if (!is.null(input$date)) {
    # first get the specifiy dataset again
    daterange <- get_date()
    country <- try(get_country(), silent = TRUE)
    corona_frame <- st_as_sf(corona_sf, crs = 4326)
    if (class(country) != "try-error") {
      # if a country was clicked, select the subset of corona data
      if (country != "world") {
        country_df <- st_as_sf(countries[countries$ADMIN == country, ], crs = 4326)
        if (country == "Israel") {
          corona_frame <- corona_frame[corona_frame$`Country/Region` == "Israel", ]
        } else {
          corona_frame <- corona_frame[unlist(st_contains(country_df, corona_frame)), ]
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
      }
    }
    corona_frame$geometry <- NULL
    # group it
    corona_grouped <- corona_frame %>%
      group_by(date) %>%
      summarise(
        confirmed = sum(confirmed),
        deaths = sum(deaths)
      )
    colnames(corona_grouped) <- c("date", "confirmed", "deaths")
    last_day <- corona_grouped[corona_grouped$date == daterange[1] - 1, ]
    corona_grouped <- corona_grouped[
      corona_grouped$date >= daterange[1] &
        corona_grouped$date <= daterange[2],
    ]
    corona_grouped2 <- corona_grouped
    # count the number of cases for each specific day
    if (daterange[1] != daterange[2]) {
      for (i in nrow(corona_grouped2):2) {
        corona_grouped2[i, ]$confirmed <- corona_grouped2[i, ]$confirmed -
          corona_grouped2[i - 1, ]$confirmed
      }
    }
    corona_grouped2$confirmed[1] <- corona_grouped2$confirmed[1] -
      last_day$confirmed
    # update the plot
    plotlyProxy("daily_plot_confirmed", session) %>%
      # remove old traces
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      # add new traces
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped2$date,
        y = corona_grouped2$confirmed,
        name = "Deceased",
        type = "bar",
        marker = list(color = "rgba(52,84,209,0.5)")
      )) %>%
      # set the layout
      plotlyProxyInvoke(
        "relayout",
        showlegend = FALSE,
        yaxis = list(
          title = "New cases",
          fixedrange = TRUE
        ),
        xaxis = list(fixedrange = TRUE, showgrid = FALSE)
      )
  }
})


observeEvent(list(
  input$date,
  input$mymap_click
), {
  if (!is.null(input$date)) {
    # first get the specifiy dataset again
    daterange <- get_date()
    country <- try(get_country(), silent = TRUE)
    corona_frame <- st_as_sf(corona_sf, crs = 4326)
    if (class(country) != "try-error") {
      # if a country was clicked, select the subset of corona data
      if (country != "world") {
        country_df <- st_as_sf(countries[countries$ADMIN == country, ], crs = 4326)
        if (country == "Israel") {
          corona_frame <- corona_frame[corona_frame$`Country/Region` == "Israel", ]
        } else {
          corona_frame <- corona_frame[unlist(st_contains(country_df, corona_frame)), ]
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
      }
    }
    corona_frame$geometry <- NULL
    # group it
    corona_grouped <- corona_frame %>%
      group_by(date) %>%
      summarise(
        confirmed = sum(confirmed),
        deaths = sum(deaths)
      )
    colnames(corona_grouped) <- c("date", "confirmed", "deaths")
    last_day <- corona_grouped[corona_grouped$date == daterange[1] - 1, ]
    corona_grouped <- corona_grouped[
      corona_grouped$date >= daterange[1] &
        corona_grouped$date <= daterange[2],
    ]
    corona_grouped2 <- corona_grouped
    # count the number of cases for each specific day
    if (daterange[1] != daterange[2]) {
      for (i in nrow(corona_grouped2):2) {
        corona_grouped2[i, ]$deaths <- corona_grouped2[i, ]$deaths -
          corona_grouped2[i - 1, ]$deaths
      }
    }
    corona_grouped2$deaths[1] <- corona_grouped2$deaths[1] -
      last_day$deaths
    # update the plot
    plotlyProxy("daily_plot_deaths", session) %>%
      # remove old traces
      plotlyProxyInvoke("deleteTraces", list(0)) %>%
      # add new traces
      plotlyProxyInvoke("addTraces", list(
        x = corona_grouped2$date,
        y = corona_grouped2$deaths,
        name = "Deceased",
        type = "bar",
        marker = list(color = "rgba(237,37,78,0.5")
      )) %>%
      # set the layout
      plotlyProxyInvoke(
        "relayout",
        showlegend = FALSE,
        yaxis = list(
          title = "Deaths",
          fixedrange = TRUE
        ),
        xaxis = list(fixedrange = TRUE, showgrid = FALSE)
      )
  }
})
