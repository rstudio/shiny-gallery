import("R6")
import("utils")
import("jsonlite")
import("glue")
import("shiny.blank")
import("leaflet")
import("shiny")
import("dplyr")

export("mapManager")

ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$script(src = "scripts/update_map_style.js"),
    tags$style(id = "updateMapStyles"),
    leafletOutput(ns("mainMap"), height = "100%")
  )
}

updateMarkers <- function(markers, required, name, map, dataManager) {
  # Current markers on the map
  if(is.null(markers)) markers = list()
  current <- ifelse(
    is.data.frame(markers),
    nrow(markers),
    0
  )

  # Update marker list with new required side
  if(length(markers) == 0) {
    markers <- sample_n(dataManager$getCities(), required)[c("lat", "lng")]
  }
  if(required < current) {
    markers <- sample_n(markers, required)[c("lat", "lng")]
  }
  if(required > current) {
    markers <- rbind(
      markers,
      sample_n(dataManager$getCities(), required - current)[c("lat", "lng")]
    )
  }

  # Add new markers to the map
  if (length(markers) > 0 ) {
    map <- map %>%
    addMarkers(data = markers, lng = ~lng, lat = ~lat,
    icon = list(
      iconUrl = paste0("assets/map/", name, ".png"),
      iconSize = c(30, 30),
      className = paste0("marker-", name)
     ))
  }

  return(markers)
}

server <- function(input, output, session, stateManager, dataManager) {
  ns <- session$ns

  output$mainMap <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        preferCanvas = TRUE,
        zoomControl = FALSE,
        dragging = FALSE,
        doubleClickZoom= FALSE,
        minZoom = 2,
        maxZoom = 2)
      ) %>%
      addProviderTiles("Stamen.Watercolor",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(0, 0, 2)
  })

  observe({
    map <- leafletProxy("map-mainMap") %>%
      clearMarkers()

    current <- reactiveValuesToList(stateManager$state)
    markers <- stateManager$markers

    # Base values for calculating necessary number of markers
    base <- list(
      environment = floor(current$environment/10),
      wealth = floor(current$wealth/10),
      opinion = floor(current$opinion/10)
    )

    # Marker categories for stats indicators
    categories <- list()
    # Environment indicators
    # Trees start at zero and grow with the current environment
    categories$tree <- base$environment * 5
    categories$tree_large <- base$environment
    categories$tree_small <- base$environment * 2
    # Fires appear at 40 environment an increase numbers as it gets lower
    categories$fire <- ifelse(current$environment <= 40, (5 - base$environment), 0)
    # Cold appear at 40 environment an increase numbers as it gets lower
    categories$cold <- ifelse(current$environment <= 40, (5 - base$environment), 0)
    # Sick appear at 40 environment an increase numbers as it gets lower
    categories$sick <- ifelse(current$environment <= 40, (5 - base$environment), 0)
    # Tornados appear at 25 environment an increase numbers as it gets lower
    categories$tornado <- ifelse(current$environment <= 25, (3 - base$environment), 0)
    # Thunder appear at 25 environment an increase numbers as it gets lower
    categories$thunder <- ifelse(current$environment <= 25, (3 - base$environment), 0)

    # Wealth Indicators
    # Broken houses start apearing at 50 wealth and increase numbers as it gets lower
    categories$house_broken <- ifelse(current$wealth <= 50, (6 - base$wealth), 0)
    # Mormal houses grow up to 50 wealth and start decreasing after that
    if (current$wealth >= 50) categories$house <- (11 - base$wealth)
    else categories$house <- base$wealth
    # Office buildings apearing at 50 wealth and increase numbers as it gets higher
    categories$office <- ifelse( current$wealth >= 50, (base$wealth - 4), 0)

    # Opinion Indicators
    # Mad people start apearing at 50 opinion and increase numbers as it gets lower
    categories$mad <- ifelse(current$opinion <= 50, (6 - base$opinion), 0)
    # Smily people grow up to 50 opinion and start decreasing after that
    if (current$opinion >= 50) categories$smile <- (11 - base$opinion)
    else categories$smile <- base$wealth
    # Super happy people apearing at 50 opinion and increase numbers as it gets higher
    categories$stareyes <- ifelse(current$opinion >= 50, (base$opinion - 4), 0)

    # Updates markers for all categories
    for(category in names(categories)) {
      stateManager$markers[[category]] <- updateMarkers(
        markers = markers[[category]],
        required = categories[[category]],
        name = category,
        map = map,
        dataManager = dataManager
      )
    }
  })
}

mapManager <- R6Class("mapManager",
  private = list(
    server = server,
    stateManager = NULL,
    dataManager = NULL
  ),

  public = list(
    ui = ui,
    init_server = function(id) {
      callModule(private$server, id, private$stateManager, private$dataManager)
    },

    updateState = function(session) {
      state <- private$stateManager$state

      session$sendCustomMessage(
        "updateMapStyle",
        reactiveValuesToList(state)
      )
    },

    initialize = function(stateManager, dataManager) {
      private$stateManager <- stateManager
      private$dataManager <- dataManager
    }
  )
)
