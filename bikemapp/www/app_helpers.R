#  load data ====
load(file="www/data_ciclista.Rda")

# transform polys to sf for better performance ====-
malaspolydf=SpatialPolygonsDataFrame(Sr =  sp::SpatialPolygons(malaspoly),
                                     data = malas@data,
                                     match.ID = T )
sf_malaspolydf=st_as_sf(malaspolydf)

#
ciclopolydf=SpatialPolygonsDataFrame(Sr = sp::SpatialPolygons(ciclopoly),
                                     data = ciclovias@data,
                                     match.ID = T )
sf_ciclopolydf=st_as_sf(ciclopolydf)



# a bike gif  for loading screen====
# (https://designmodo.com/animate-flat-design-bicycle/)
gif <- "https://designmodo.com/wp-content/uploads/2015/08/bicycle-GIF.gif"

loading_screen <- tagList(
  img(src = gif, height = "150px"),
  h2("Cargando mapa...", style = "color:white;")
)


# text for help tab ====
ayuda=readLines(con = "www/ayuda.txt",encoding = "UTF-8")
help_text = HTML(ayuda)


# custom switch button ====
mybutton=function(css){
  return(
    str_replace_all(paste(css,"<div class='onoffswitch'>
  <input type='checkbox' name='onoffswitch' class='onoffswitch-checkbox' id='myonoffswitch' tabindex='0'>
  <label class='onoffswitch-label' for='myonoffswitch'>
  <span class='onoffswitch-inner'></span>
  <span class='onoffswitch-switch'></span>
  </label>
  </div>", collapse=''),
  "[\r\n]" , ""  )
  )
}


myswitch = function(inputId, label, tipo="bs") {
  # bs o ciclo
  if (tipo=="bs"){
    boton=tagList(
      tags$div(class = "form-group shiny-input-container",
               tags$div(class = tipo,
                        tags$label(label, class = "control-label"),
                        tags$div(class = "onoffswitch",
                                 tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox",
                                            id = inputId, checked = "" # only bs is checked
                                 ),
                                 tags$label(class = "onoffswitch-label", `for` = inputId,
                                            tags$span(class = "onoffswitch-inner"),
                                            tags$span(class = "onoffswitch-switch")
                                 )
                        )
               )
      )
    )
  }else{
    boton=tagList(
      tags$div(class = "form-group shiny-input-container",
               tags$div(class = tipo,
                        tags$label(label, class = "control-label"),
                        tags$div(class = "onoffswitch",
                                 tags$input(type = "checkbox", name = "onoffswitch", class = "onoffswitch-checkbox",
                                            id = inputId
                                 ),
                                 tags$label(class = "onoffswitch-label", `for` = inputId,
                                            tags$span(class = "onoffswitch-inner"),
                                            tags$span(class = "onoffswitch-switch")
                                 )
                        )
               )
      )
    )
  }
  return(boton)
}


iconbutton=function (inputId, label, value = FALSE, status = "default", 
                     shape = c("square", "curve", "round"), 
                     outline = FALSE, fill = FALSE, thick = FALSE, animation = NULL, 
                     icon = NULL, plain = FALSE, bigger = FALSE, inline = FALSE, 
                     width = NULL) 
{
  value <- shiny::restoreInput(id = inputId, default = value)
  status <- match.arg(status, c("default", "primary", 
                                "success", "info", "danger", "warning"))
  shape <- match.arg(shape)
  if (!is.null(icon)) {
    icon <- shinyWidgets:::validateIcon(icon)
    icon$attribs$class <- paste("icon", icon$attribs$class)
  }
  if (!is.null(animation)) 
    animation <- match.arg(animation, c("smooth", "jelly", 
                                        "tada", "rotate", "pulse"))
  inputTag <- tags$input(id = inputId, type = "checkbox")
  if (!is.null(value) && value) 
    inputTag$attribs$checked <- "checked"
  checkTag <- tags$div(class = "form-group shiny-input-container", 
                       style= "font-size: large;display: inline-block; margin-right: 10px;padding-left: 20px;",
                       tags$div(class = "pretty", inputTag, class = if (is.null(icon)) 
                         "p-default", class = if (plain) 
                           "p-plain", class = if (bigger) 
                             "p-bigger", class = if (shape != "square") 
                               paste0("p-", shape), class = if (fill) 
                                 "p-fill", class = if (thick) 
                                   "p-thick", class = if (!is.null(animation)) 
                                     paste0("p-", animation), class = if (!is.null(icon)) 
                                       "p-icon", tags$div(class = "state", class = if (status != 
                                                                                       "default") 
                                         paste0("p-", status, if (outline) 
                                           "-o"), if (!is.null(icon)) 
                                             icon, tags$label(tags$span(label)))))
  shinyWidgets:::attachShinyWidgetsDep(checkTag, "pretty")
}

# customize navbar a bit (icons and size) ====
myf7NavBar=function (..., subNavbar = NULL, title = NULL, subtitle = NULL, 
          hairline = TRUE, shadow = TRUE, bigger = FALSE, transparent = FALSE, 
          left_panel = FALSE, right_panel = FALSE) 
{
  navbarClass <- "navbar"

  if (!hairline) 
    navbarClass <- paste0(navbarClass, " no-hairline")
  if (!shadow) 
    navbarClass <- paste0(navbarClass, " no-shadow")
  leftNav <- if (left_panel) {
    shiny::tags$div(class = "left", shiny::tags$a(class = "link icon-only panel-open", 
                                                  `data-panel` = "left", icon(lib="glyphicon","road")))
  }
  rightNav <- if (right_panel) {
    shiny::tags$div(class = "right", shiny::tags$a(class = "link icon-only panel-open", 
                                                   `data-panel` = "right", icon(lib="glyphicon",name="map-marker")))
  }
  innerCl <- "navbar-inner sliding"
  
  shiny::tags$div(class = navbarClass, shiny::tags$div(class = "navbar-bg"), 
                  shiny::tags$div(class = innerCl, leftNav, if (bigger) {
                    shiny::tagList(shiny::tags$div(class = "title", 
                                                   title, style = "color: white;"), rightNav, 
                                   shiny::tags$div(class = "title-large", 
                                                   shiny::tags$div(class = "title-large-text", 
                                                                   title)))
                  }
                  else {
                    shiny::tagList(shiny::tags$div(class = "title", 
                                                   title, if (!is.null(subtitle)) 
                                                     shiny::tags$span(class = "subtitle", 
                                                                      subtitle)), rightNav)
                  }, ..., subNavbar)
                  )
}

#  create json manifest for PWA compat ====
# create_manifest(
#   path = getwd(),
#   name = "Mapa del Ciclista",
#   shortName = "Mapa del Ciclista",
#   description = "Un mapa para andar en bici por el AMBA.",
#   lang = "es-AR",
#   startUrl = "https://2exp3.shinyapps.io/mapa-ciclista/",
#   display = "standalone",
#   background_color = "#eaeffa",
#   theme_color = "#3367D6",
#   icon = data.frame(
#     src = "icons/128x128.png",
#     sizes = "128x128", 10,
#     types = "image/png"
#   )
# )
# 


# shinyapp log ====
#  for debugging
# rsconnect::showLogs(appPath = getwd(), appFile = NULL, appName = NULL,
#                     account = NULL, entries = 100, streaming = FALSE)

