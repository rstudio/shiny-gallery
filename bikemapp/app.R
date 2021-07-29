# Great packages to develop our app
library(dplyr); # data wrangling
library(shiny);library(shinyMobile);library(shinyWidgets);library(waiter); # UI/server app development
library(sf);library(sp);library(rgeos); # geo data-objects
library(leaflet);library(leaflet.extras); # map

# load data and custom UI functions====
source("www/app_helpers.R",encoding = "UTF-8")

# set our UI====
ui = 
  f7Page(
    favicon = "icono.png",
    icon = "icono.png",
    manifest = "manifest.json",
    use_waiter(),

    includeCSS("www/my_switch.css"),

    title = "Mapa del Ciclista", 
    
    init = f7Init(skin = "auto", color = "black",theme = "light"),
    
    f7TabLayout(
      navbar = 
        myf7NavBar(
          bigger = F,
          title = 
            tagList(
              tags$div(style="text-align: left;",
                       tags$h4("Mapa del Ciclista",
                               tags$i(class="fa fa-bicycle")
                       ) 
              )
            ),
          hairline = TRUE,
          shadow = TRUE,
          left_panel = TRUE,
          right_panel = TRUE
        ),
      panels =  
        tagList(
          # left-hand side bar ====
          f7Panel(
            theme = "light",
            side = "left",
            title =
              tags$div(
                style="text-align: center;display: inline-block; ",
                tags$h2("Calles",
                        tags$i(class="glyphicon glyphicon-road")
                ) 
              ),
            effect = "cover",
            resizable = T, 
            tagList(
              myswitch(inputId = "bs",  label = "Mala superficie" ,tipo="bs") ,
              br(),br(),
              myswitch(inputId = "ciclo",  label = "Ciclovias",tipo="ciclo" ) ,
            )
          ),
          # right-hand side bar ====
          f7Panel(
            theme = "light",
            side = "right",
            title = tags$div(
              style="text-align: center;display: inline-block; ",
              tags$h2("Lugares",
                      tags$i(class="glyphicon glyphicon-map-marker")
              ) 
            ),
            effect = "cover",
            resizable = T, 
            tagList(
              br(),
              iconbutton(fill = F,
                             inputId = "agua", shape = "round",
                             label = "Agua", 
                             value = F,
                             status = "info",
                             icon = icon("tint"), 
                             plain = T,
                             animation = "smooth",bigger = T,
                             outline = F
              ),br(),br(),
              iconbutton(fill = F,
                             inputId = "aire", shape = "round",
                             label = "Aire", 
                             value = F,
                             status = "warning",
                             # icon = icon(name = "gas-pump"), 
                             plain = T,
                             animation = "smooth",bigger = T,
                             outline = F
              ),br(),br(),
              iconbutton(fill = F,
                             inputId = "estacionamiento", shape = "round",
                             label = "Estacionamiento", 
                             value = F,
                             status = "primary",
                             # icon = icon(name = "parking"), 
                             plain = T,
                             animation = "smooth",bigger = T,
                             outline = F
              ), br(),br(),
              iconbutton(fill = F,
                             inputId = "taller", shape = "round",
                             label = "Taller", 
                             value = F,
                             status = "default",
                             icon = icon("wrench"), 
                             plain = T,
                             animation = "smooth",bigger = T,
                             outline = F
              )
             
            )
          )
        )
      ,
      # set 2 tabs
      f7Tabs(
        swipeable = F,
        animated = F,
        # 1. Create tje map
        f7Tab(
          tabName = "Mapa",
          icon = f7Icon("map_pin_ellipse"),
          active = T,
          tagList(leafletOutput("mapa",width = "100%",height = "100%"))
        ),
        
        # 2. Create an "About this app" tab 
        f7Tab(
          active = F,
          tabName = "Ayuda",
          icon = f7Icon("question_circle"),
          br(),
          f7Shadow(
            intensity = 5,
            hover = F,
            f7Card(title ="Instrucciones",
                   help_text,
                   footer =
                     tagList(
                       f7Link(
                         external = T,
                         icon = f7Icon("logo_twitter"),
                         label = "",
                         src = "https://twitter.com/2exp3/"
                       ),
                       f7Link(
                         external = T,
                         icon = f7Icon("logo_github"),
                         label = "",
                         src = "https://github.com/2exp3/bikemapp"
                       )
                     )
            )
          )
        )
      )
    )
  )

server = function(input, output, session) {
  # set a custom waiter screen
  wtr0=Waiter$new(html=loading_screen, color= "#44B5A0")
  
  # toggle layers
  hide_show=function(estado,grupo){
   if(estado==T){
      leafletProxy("mapa") %>%
        showGroup(grupo)
    }else{
      leafletProxy("mapa") %>%
        hideGroup(grupo)
    }
  }
 
# render the basemap
  output$mapa = renderLeaflet({
    
    leaflet(options = leafletOptions(preferCanvas = TRUE) ) %>% 
      # centered in beautiful CABA
      setView(lng = -58.44,lat = -34.62, zoom = 12) %>%
      # use gov official map
      addWMSTiles(
        "https://servicios.usig.buenosaires.gob.ar/mapcache/tms/1.0.0/amba_con_transporte_3857@GoogleMapsCompatible/{z}/{x}/{-y}.png",
        options = WMSTileOptions(
          format = "image/png", 
          transparent = TRUE,
          updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
          updateWhenIdle = FALSE
        ),
        layers = "nexrad-n0r-900913",
        attribution = 
          HTML("<a href='#' onclick='window.open('https://usig.buenosaires.gob.ar/');return false;'>USIG</a> (GCBA).
        <a href='http://openstreetmap.org' target='_blank'>OpenStreetMap</a> contrisbutors,
        <a href='http://creativecommons.org/licenses/by-sa/2.0/' target='_blank'>CC-BY-SA.</a>")
      ) %>% 
      # # add ruler
      addMeasure(
        primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers",
        primaryAreaUnit = "sqmeters",secondaryAreaUnit = "sqmeters",
        position = "bottomleft",
        activeColor = "#465ED9",completedColor = "#7eabe6",
        localization = "es"
      ) %>% 
      # # add view reset
      addEasyButton(
        easyButton(
          position = "topleft",
          icon = "ion-arrow-shrink",
          title = "Resetear ubicacion",
          onClick = JS("function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }")
        )
      ) %>% 
      htmlwidgets::onRender(
        JS("function(el, x){ var map = this; map._initialCenter = map.getCenter(); map._initialZoom = map.getZoom();}")
      ) %>% 
     
      # add locate me
      addEasyButton(
        easyButton(
          position = "bottomright",
          icon = "fa-crosshairs",
          title = "Mi ubicacion",
          onClick = JS("
                       function(btn, map){ 
                              map.on('locationfound',
                                    function onLocationFound(e) {
                                       var radius = e.accuracy;
                                      if (typeof mymarker !== 'undefined') {
                                          map.removeLayer(mymarker);
                                    };
                                    mymarker= L.marker(e.latlng).addTo(map)
                                    .bindPopup('Estás en un radio de ' + Math.round(radius) + ' metros alrededor de este punto').openPopup();
                                          });
                       map.locate({flyTo:true, setView:true, enableHighAccuracy: true });}" )
        )
      )%>% 
      # add search
      leaflet.extras::addSearchOSM(
        options =
          searchOptions(
            autoCollapse = T,textCancel = "Cancelar",
            moveToLocation = T,textPlaceholder = "Ingresa direccion...",
            casesensitive = F,collapsed =T, position="topright"
          )) 
  })
  
  # to show waiter while rendering the heavy stuff
  observeEvent(input$shinyInfo,{
    wtr0$show()
    
    leafletProxy("mapa",deferUntilFlush = F) %>% 
      
      # Bad surface
      addPolylines(
        options = pathOptions(lineCap = "round",lineJoin = "round"),
        data=malas,
        weight = 2,
        color="#fe4365",
        opacity = .85,
        noClip = T,
        smoothFactor = 4,
        group="bs"
      ) %>%
      addPolygons(
        data=sf_malaspolydf,
        options =  pathOptions(lineCap = "round",lineJoin = "round"),
        stroke = T,
        fill = T,
        fillColor = "#fe4365",
        fillOpacity = .85,
        weight = 0,
        group="bs",
        popup = ~pop
        ) %>%
      
      # cicleways
      addPolylines(
        data=ciclovias,
        weight = 2,
        dashArray= "10",
        color="#8d6cab",
        opacity = .85,
        noClip = T,
        smoothFactor = 4,
        group = "ciclovia"
      ) %>%
      addPolygons(
        data=sf_ciclopolydf,
        options =  pathOptions(lineCap = "round",lineJoin = "round"),
        stroke = T,
        fill = T,
        
        fillColor = "#8d6cab",
        fillOpacity = .85,
        weight = 0,
        group="ciclovia",
        popup = ~pop
      ) %>%
      hideGroup(group = "ciclovia") %>% 
      
      #water
      addAwesomeMarkers(
        lng = ~long, lat=~lat,
        clusterOptions = markerClusterOptions(),
        data= lugares %>% filter(tipo=="Agua"),
        icon = ~IconSet$Agua,
        group = "Agua",
        popup = ~pop
      ) %>% 
      hideGroup(
        group = "Agua"
      ) %>% 
      
      # air pump
      addAwesomeMarkers(
        lng = ~long, lat=~lat,
        clusterOptions = markerClusterOptions(),
        data= lugares %>% filter(tipo=="Aire"),
        icon = ~IconSet$Aire,
        group = "Aire",
        popup = ~pop
      ) %>% 
      hideGroup(
        group = "Aire"
      ) %>% 
      
      # parking
      addAwesomeMarkers(
        lng = ~long, lat=~lat,
        clusterOptions = markerClusterOptions(),
        data= lugares %>% filter(tipo=="Estacionamiento"),
        icon = ~IconSet$Estacionamiento,
        group = "Estacionamiento",
        popup = ~pop
      ) %>% 
      hideGroup(
        group = "Estacionamiento"
      ) %>% 
      
      # bikeshop
      addAwesomeMarkers(
        lng = ~long, lat=~lat,
        clusterOptions = markerClusterOptions(),
        data= lugares %>% filter(tipo=="Taller"),
        icon = ~IconSet$Taller,
        group = "Taller",
        popup = ~pop
      ) %>% 
      hideGroup(
        group = "Taller"
      ) 
    
    wtr0$hide()
  })

  
  # Mala superficie
  observeEvent(input$bs,ignoreInit = T,{
    hide_show(estado = input$bs,grupo = "bs")
  })
  
  # Ciclovias
  observeEvent(input$ciclo,ignoreInit = T,{
    hide_show(estado = input$ciclo,grupo = "ciclovia")
  })
  
  # Agua
  observeEvent(input$agua,ignoreInit = T,{
    hide_show(estado = input$agua,grupo = "Agua")
  })
  
  # Aire
  observeEvent(input$aire,ignoreInit = T,{
    hide_show(estado = input$aire,grupo = "Aire")
  })
  
  # Estacionamiento
  observeEvent(input$estacionamiento,ignoreInit = T,{
    hide_show(estado = input$estacionamiento,grupo = "Estacionamiento")
  })
  
  # Taller
  observeEvent(input$taller,ignoreInit = T,{
    hide_show(estado = input$taller,grupo = "Taller")
  })
}

shinyApp(ui, server)

