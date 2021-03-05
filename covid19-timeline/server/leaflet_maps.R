output$mymap <- renderLeaflet({
  # if no mapbox key exists use cartodb dark matter
  if (key == "") {
    map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(
        "CartoDB.DarkMatter"
      ) %>%
      setView(
        lng = 40,
        lat = 30.45,
        zoom = 3
      )
  } else {
    map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addMapboxGL(style = "mapbox://styles/nicohahn/ckg0iz22c28xx19pqdi54tw53") %>%
      setView(
        lng = 40,
        lat = 30.45,
        zoom = 3
      )
  }
})

output$map_wuhan <- renderLeaflet({
  wuhan <- data.frame(
    lon = 114.283,
    lat = 30.583
  )
  coords <- st_as_sf(wuhan, coords = c("lon", "lat"), crs = 4326)
  china <- countries[countries$ADMIN == "China", ]
  leaflet(options = leafletOptions(
    zoomControl = FALSE, minZoom = 4, maxZoom = 4, attributionControl = FALSE,
    dragging = FALSE
  )) %>%
    addPolylines(
      data = st_cast(st_as_sf(china), "MULTILINESTRING"),
      color = "#00bb8b",
      weight = 1
    ) %>%
    addPulseMarkers(
      lng = wuhan$lon,
      lat = wuhan$lat,
      icon = makePulseIcon(heartbeat = 0.5, color = "#fb5a19"),
    ) %>%
    setView(
      lat = wuhan$lat + 7,
      lng = wuhan$lon,
      zoom = 1
    )
})


output$map_emergency <- renderLeaflet({
  leaflet(options = leafletOptions(
    zoomControl = FALSE, minZoom = 2, maxZoom = 2,
    dragging = FALSE, attributionControl = FALSE
  )) %>%
    addPolylines(
      data = st_cast(infected, "MULTILINESTRING"),
      color = "#00bb8b",
      weight = 1
    ) %>%
    addPulseMarkers(
      data = coords_inf,
      icon = makePulseIcon(heartbeat = 0.5, color = "#fb5a19", iconSize = 3)
    ) %>%
    setView(
      lat = 40,
      lng = 0,
      zoom = 2
    )
})


output$map_lockdown <- renderLeaflet({
  leaflet(options = leafletOptions(
    zoomControl = FALSE, minZoom = 2, maxZoom = 2,
    dragging = FALSE, attributionControl = FALSE
  )) %>%
    addPolygons(
      data = st_as_sf(locked_countries),
      fillColor = ~ pal(lockdown),
      opacity = 1,
      fillOpacity = 0.25,
      weight = 1,
      color = "#FCFCFC",
      label = locked_countries$ADMIN
    ) %>%
    leaflet::addLegend(
      data = locked_countries,
      pal = pal,
      values = ~lockdown,
      title = "Lockdown status",
      position = "bottomleft"
    ) %>%
    setView(
      lat = 40,
      lng = 0,
      zoom = 2
    )
})
