
server <- function(input, output, session) {
  
  #sever(html = disconnected,bg_image = "https://cdn.pixabay.com/photo/2019/11/09/23/48/palace-of-fine-arts-4614673_1280.jpg", bg_color = "#000")
  
  output$monto_aprob <- renderEcharts4r({
    if (input$data == "Todas las alcaldías"){
      datos |> 
        group_by(nombre_alcaldia) |> 
        summarise(monto_asig = sum(monto_aprobado)) |> 
        arrange(desc(monto_asig)) |> 
        e_charts(nombre_alcaldia, dispose = FALSE) |> 
        e_bar(monto_asig, name = "Monto por alcaldía") |> 
        e_title("Monto asignado para cada alcaldía",
                textStyle = list(
                  color = "gray"
                )) |> 
        e_tooltip(confine = TRUE,
                  formatter = e_tooltip_pointer_formatter("currency"),
                  trigger = "axis") |> 
        e_theme("auritus") |> 
        e_color(color = "#616161") |> 
        e_y_axis(show = FALSE) |> 
        e_legend(FALSE) 
    }else{
      datos |> 
        filter(nombre_alcaldia ==input$data) |> 
        group_by(desc_finalidad) |> 
        summarise(monto_asig = sum(monto_aprobado)) |> 
        e_charts(desc_finalidad, dispose = FALSE) |> 
        e_bar(monto_asig, name = "Monto asignado") |> 
        e_tooltip(confine = TRUE,
                  formatter = e_tooltip_pointer_formatter("currency"),
                  trigger = "axis") |> 
        e_theme("auritus") |> 
        e_legend(FALSE) |> 
        e_color(color = "#616161") |> 
        e_y_axis(show = FALSE) |> 
        e_title(paste0("Alcaldía ", input$data),
                "Monto asignado por rubros",
                textStyle = list(
                  color = "gray"
                )  ) 
    }
  })
  
  output$monto_ejer <- renderEcharts4r({
    if (input$data == "Todas las alcaldías"){
      datos |> 
        group_by(nombre_alcaldia) |> 
        summarise(monto_asig = sum(monto_ejercido)) |> 
        arrange(desc(monto_asig)) |> 
        e_charts(nombre_alcaldia, dispose = FALSE) |> 
        e_bar(monto_asig, name = "Monto por alcaldía") |> 
        e_color(color = "#616161") |> 
        e_title("Monto aprobado para cada alcaldía",
                textStyle = list(
                  color = "gray"
                ) ) |> 
        e_tooltip(confine = TRUE,
                  formatter = e_tooltip_pointer_formatter("currency"),
                  trigger = "axis") |> 
        e_theme("auritus") |> 
        e_y_axis(show = FALSE) |> 
        e_legend(FALSE) 
    }else{
      datos |> 
        filter(nombre_alcaldia ==input$data) |> 
        group_by(desc_finalidad) |> 
        summarise(monto_asig = sum(monto_ejercido)) |> 
        e_charts(desc_finalidad, dispose = FALSE) |> 
        e_bar(monto_asig, name = "Monto usado") |> 
        e_tooltip(confine = TRUE,
                  formatter = e_tooltip_pointer_formatter("currency"),
                  trigger = "axis") |> 
        e_color(color = "#616161") |> 
        e_theme("auritus") |> 
        e_legend(FALSE) |> 
        e_y_axis(show = FALSE) |> 
        e_title(paste0("Alcaldía ", input$data),
                textStyle = list(
                  color = "gray"
                )   ) 
    }
  })
  
  
  data <- reactive({
    
    if (input$data == "Todas las alcaldías"){
      espaciales
    }else{
      espaciales |> 
        filter(Delegación == input$data)
    }
    
  })
  
  
  texto <- reactive({
    paste0(
      "Delegación: ", data()$Delegación,"<br/>", 
      "Monto total: ", "$ ", scales::comma(data()$Ingresos), "<br/>") |> 
      lapply(htmltools::HTML)
  })
  
  
  bandas <- reactive({
    
    if (input$data == "Todas las alcaldías"){
      espaciales |> 
        #filter(Delegación == input$data) |> 
        sf::st_bbox(espaciales$geometry)  |> 
        as.vector()
    }else{
      espaciales |> 
        filter(Delegación == input$data) |> 
        sf::st_bbox(espaciales$geometry)  |> 
        as.vector()
    }
    
  })
  
  output$mapa <- renderLeaflet({
    
    
    
    leaflet(espaciales,
            options = leafletOptions(zoomControl = FALSE,
                                     #minZoom = 10.3, maxZoom = 15.3,
                                     doubleClickZoom = FALSE,
                                     attributionControl=FALSE)) |> 
      setView(-99.12, 19.31, 1, zoom = 10.3) |> 
      addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
               attribution = paste(
                 "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                 "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>")) |> 
      addPolygons(
        weight = 1,
        fillColor = ~pal(espaciales$Ingresos),
        fillOpacity = 0.3,
        color = "white",
        highlightOptions = highlightOptions(
          
          bringToFront = TRUE
        ),
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto")
      ) 
    
    
    
  })
  
  observe({
    
    leafletProxy("mapa", data = data()) |> 
      #setView(-99.12, 19.31, 1, zoom = 10.3) |> 
      clearShapes() |> 
      clearControls() |> 
      addPolygons(
        smoothFactor = 0.5,
        weight = 1,
        fillColor = ~pal(espaciales$Ingresos),
        fillOpacity = 0.3,
        color = "white",
        highlightOptions = highlightOptions(
          
          bringToFront = TRUE
        ),
        label = texto(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto")
      ) |> 
      flyToBounds(bandas()[1], bandas()[2], bandas()[3], bandas()[4])  
    
    
    
  })
  
  
  filt_final <- reactive({
    
    est_sociales |> 
      group_by(.data[[input$sexoedad]]) |> 
      summarise(n()) |> 
      setNames(c("v1", "v2")) |> 
      select(v1)
    
  })
  
  observe({
    req(filt_final())
    
    updateSelectInput(session, "filtfinal", choices = c(filt_final()$v1 ))
  })
  
  output$graf_estr <- renderEcharts4r({
    
    if (input$sexoedad == "edad" & input$deleg_est == "Todas las alcaldías" ){
      
      est_sociales |> 
        filter(estratos == input$estrato  & .data[[input$sexoedad]] == input$filtfinal) |> 
        group_by(nomgeo, sexo) |> 
        summarise(total = sum(total)) |> 
        #arrange(`sum(total)`) |> 
        rename("var" = sexo) |> 
        #arrange(var) |> 
        e_charts(var, dispose = TRUE) |> 
        e_bar(total) |> 
        e_title(paste0("Cantidad de personas en situación de '",input$estrato,"'" ), 
                paste0("Por edad (",input$filtfinal, " años)"  ),
                textStyle = list(
                  color = "gray"
                ), left = "center"  ) |> 
        e_legend(FALSE) |> 
        e_tooltip(trigger = "axis",
                  formatter = e_tooltip_pointer_formatter("decimal")
        ) |> 
        e_color(color = RColorBrewer::brewer.pal(9, "YlOrRd")) |> 
        e_theme("auritus") |> 
        e_y_axis(show = FALSE)
      
    }else if(input$sexoedad == "sexo" & input$deleg_est == "Todas las alcaldías" ){
      
      est_sociales |> 
        filter(estratos == input$estrato  & .data[[input$sexoedad]] == input$filtfinal) |> 
        group_by(nomgeo, edad) |> 
        summarise(total = sum(total)) |> 
        #arrange(`sum(total)`) |> 
        rename("var" = edad) |> 
        #arrange(var) |> 
        e_charts(var, dispose = TRUE) |> 
        e_bar(total) |> 
        e_legend(FALSE) |> 
        e_title(paste0("Cantidad de personas en situación de '",input$estrato,"'" ), 
                paste0("Por sexo (",input$filtfinal, ")"  ),
                textStyle = list(
                  color = "gray"
                ), left = "center"  ) |> 
        e_tooltip(trigger = "axis",
                  formatter = e_tooltip_pointer_formatter("decimal")
        ) |> 
        e_color(color = RColorBrewer::brewer.pal(9, "YlOrRd")) |> 
        e_theme("auritus") |> 
        e_y_axis(show = FALSE)
      
    }else if(input$sexoedad == "edad" & input$deleg_est != "Todas las alcaldías"){
      
      est_sociales |> 
        filter(estratos == input$estrato  & .data[[input$sexoedad]] == input$filtfinal & nomgeo == input$deleg_est) |> 
        group_by(nomgeo, sexo) |> 
        summarise(sum(total)) |> 
        #arrange(`sum(total)`) |> 
        e_charts(sexo, dispose = TRUE) |> 
        e_title(paste0("Cantidad de personas en situación de '",input$estrato,"'" ), 
                paste0("Por edad (",input$filtfinal, " años), para la delegación ", input$deleg_est ),
                textStyle = list(
                  color = "gray"
                ), left = "center"  ) |> 
        e_bar(`sum(total)`) |> 
        e_legend(FALSE) |> 
        e_tooltip(trigger = "axis",
                  formatter = e_tooltip_pointer_formatter("decimal")
        ) |> 
        e_color(color = tail(RColorBrewer::brewer.pal(9, "YlOrRd"))) |> 
        e_theme("auritus") |> 
        e_y_axis(show = FALSE)
      
    }else{
      
      est_sociales |> 
        filter(estratos == input$estrato  & .data[[input$sexoedad]] == input$filtfinal & nomgeo == input$deleg_est) |> 
        group_by(nomgeo, edad) |> 
        summarise(sum(total)) |> 
        #arrange(`sum(total)`) |> 
        e_charts(edad, dispose = TRUE) |> 
        e_bar(`sum(total)`) |> 
        e_legend(FALSE) |> 
        e_title(paste0("Cantidad de personas en situación de '",input$estrato,"'" ), 
                paste0("Por sexo (",input$filtfinal, "), para la delegación ", input$deleg_est ),
                textStyle = list(
                  color = "gray"
                ), left = "center"  ) |> 
        e_tooltip(trigger = "axis",
                  formatter = e_tooltip_pointer_formatter("decimal")
        ) |> 
        e_color(color = tail(RColorBrewer::brewer.pal(9, "YlOrRd"))) |> 
        e_theme("auritus") |> 
        e_y_axis(show = FALSE)
      
    }
    
  })
  
  
  data_mapa_est <- reactive({
    est_sociales |> 
      filter(estratos == input$estrato  & .data[[input$sexoedad]] == input$filtfinal ) |> 
      group_by(nomgeo) |> 
      summarise(total = sum(total))  
  })
  
  
  
  output$mapa_estratos <- renderLeaflet({
    
    
    
    
    espaciales_estsociales <- espaciales_estsociales |> 
      left_join(data_mapa_est(), by = c("Delegación" = "nomgeo")) |> 
      arrange()
    
    mytext_est <- paste0(
      "Delegación: ", espaciales_estsociales$Delegación,"<br/>", 
      "Monto total: ", "$ ", scales::comma(espaciales_estsociales$total), "<br/>") |> 
      lapply(htmltools::HTML)
    
    pal_est <- colorBin("YlOrRd", domain = espaciales_estsociales$total, bins = 5)
    
    leaflet(espaciales_estsociales,
            options = leafletOptions(zoomControl = FALSE,
                                     #minZoom = 10.3, maxZoom = 15.3,
                                     doubleClickZoom = FALSE,
                                     attributionControl=FALSE)) |> 
      setView(-99.12, 19.31, 1, zoom = 10.3) |> 
      addProviderTiles(providers$Stamen.TonerBackground,
                       options = providerTileOptions(noWrap = TRUE)
      ) |> 
      addPolygons(
        weight = 1,
        fillColor = ~pal_est(espaciales_estsociales$total),
        fillOpacity = 0.8,
        color = "black",
        highlightOptions = highlightOptions(
          
          bringToFront = TRUE
        ),
        label = mytext_est,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto")
      ) 
    
    
  })
  
  
  bandas_est <- reactive({
    
    
    if (input$deleg_est == "Todas las alcaldías"){
      espaciales_estsociales |> 
        #filter(Delegación == input$data) |> 
        sf::st_bbox(espaciales_estsociales$geometry)  |> 
        as.vector()
    }else{
      espaciales_estsociales |> 
        filter(Delegación == input$deleg_est) |> 
        sf::st_bbox(espaciales_estsociales$geometry)  |> 
        as.vector()
    }
    
  })
  
  texto_est <- reactive({
    
    if ( input$deleg_est == "Todas las alcaldías"){
      
      infotex <- est_sociales |> 
        filter(estratos == input$estrato  & .data[[input$sexoedad]] == input$filtfinal ) |> 
        group_by(nomgeo) |> 
        summarise(total = sum(total))
      
      paste0(
        "Delegación: ", infotex$nomgeo,"<br/>", 
        "Total: ", scales::comma(infotex$total), "<br/>") |> 
        lapply(htmltools::HTML)
    }else{
      
      infotex <- est_sociales |> 
        filter(estratos == input$estrato  & .data[[input$sexoedad]] == input$filtfinal ) |> 
        group_by(nomgeo) |> 
        summarise(total = sum(total)) |> 
        filter(nomgeo == input$deleg_est)
      
      paste0(
        "Delegación: ", infotex$nomgeo,"<br/>", 
        "Total: ", scales::comma(infotex$total), "<br/>") |> 
        lapply(htmltools::HTML)
      
      
    }
    
  })
  
  observe({
    espaciales_estsociales <- espaciales_estsociales |> 
      left_join(data_mapa_est(), by = c("Delegación" = "nomgeo"))
    
    
    if (input$deleg_est == "Todas las alcaldías"){
      espaciales_estsociales <- espaciales_estsociales
    }else{
      
      espaciales_estsociales <- espaciales_estsociales |> 
        filter(Delegación == input$deleg_est)
      
    }
  
    
    
    pal_est <- colorBin("YlOrRd", domain = espaciales_estsociales$total, bins = 5)
      
      
      
      
    
    leafletProxy("mapa_estratos", data = espaciales_estsociales ) |> 
      #setView(-99.12, 19.31, 1, zoom = 10.3) |> 
      clearShapes() |> 
      clearControls() |> 
      addPolygons(
        smoothFactor = 0.5,
        weight = 1,
        fillColor = ~pal_est(data_mapa_est()$total),
        fillOpacity = 0.92,
        color = "white",
        highlightOptions = highlightOptions(
          
          bringToFront = TRUE
        ),
        label = texto_est(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto")
      ) |> 
      flyToBounds(bandas_est()[1], bandas_est()[2], bandas_est()[3], bandas_est()[4])  
    
    
    
  })
  
  output$comp_final <- renderEcharts4r({
    
    relacion |> 
      arrange(desc(total)) |> 
      e_charts(nombre_alcaldia) |> 
      e_bar(total_asig, name = "Monto total asignado") |> 
      e_bar(total, y_index = 1, name = "Personas en situación de pobreza") |> 
      e_tooltip(trigger = "axis") |> 
      e_y_axis(show = FALSE) |> 
      e_y_axis(show = FALSE, index = 1) |> 
      e_theme("auritus") |> 
      e_color(color = c("#8f4343", "#5c5c5c")) |> 
      e_legend(FALSE) |> 
      e_title("Monto asignado vs Personas en situación de pobreza", "¿A quien se prioriza en el presupuesto?",
              left = "center",
              textStyle = list(
                color = "gray")
      )
    
  })
  
  output$pie_final <- renderEcharts4r({
    
    
    porcentaje_fin |> 
      e_charts(nombre_alcaldia) |> 
      e_pie(porcentaje,radius = c("50%", "70%"),
            label = list(show = FALSE)
            ) |> 
      e_legend(FALSE) |> 
      e_tooltip(formatter = htmlwidgets::JS("function(params)
  {return('<strong>' + params.name + 
  '</strong><br />Valor: ' + params.value + 
                  '<br />Porcentaje: ' +  params.percent)  +'%' }"
      )) |> 
      e_theme("auritus") |> 
      e_color(color = RColorBrewer::brewer.pal(7, "Greys"))  |> 
      e_title("Proporción de los montos asignados por alcaldía", "¿Quién obtiene el monto más significativo?",
              left = "center",
              textStyle = list(
                color = "gray")
      ) 
    
    
  })
  
  
  output$rel_final <- renderEcharts4r({
    
    relacion |> 
      e_charts(x = total) |> 
      e_scatter(total_asig, name = "Monto asignado total") |> 
      e_lm(total_asig ~ total, name = "Tendencia") |> 
      e_title(
        text = "Relación lineal entre las variables",
        subtext = "Monto asignado vs Personas en situación de pobreza",
        left = "center",
        textStyle = list(
          color = "gray")
      ) |> 
      e_legend(FALSE) |> 
      e_color(color = c("#de1f1f", "#787878")) |> 
      e_tooltip(trigger = "axis") |> 
      e_y_axis(
        show = FALSE
      ) |> 
      e_theme("auritus")
    
  })
  
  
  
  
  output$comp_final_suelo <- renderEcharts4r({
    
    data_suelo |> 
      arrange(desc(total)) |> 
      e_charts(Delegacion) |> 
      e_bar(total,name =  "Monto total asignado") |> 
      e_bar(Ingresos, y_index = 1, name = "Valor del suelo") |> 
      e_tooltip(trigger = "axis") |> 
      e_y_axis(show = FALSE) |> 
      e_y_axis(show = FALSE, index = 1) |> 
      e_theme("auritus") |> 
      e_color(color = c("#8f4343", "#5c5c5c")) |> 
      e_legend(FALSE) |> 
      e_title("Monto asignado vs valor del suelo en la CDMX", "¿A quién se prioriza en el presupuesto?",
              left = "center",
              textStyle = list(
                color = "gray")
      )
    
    
  })
  
  output$hist_final <- renderEcharts4r({
    
    
    data_suelo |> 
      e_charts() |> 
      e_histogram(total, breaks = "sturges", name = "Frecuencia") |> 
      e_tooltip(trigger = "axis")  |> 
      e_y_axis(show = FALSE) |> 
      e_tooltip(trigger = "axis") |> 
      e_y_axis(show = FALSE, index = 1) |> 
      e_theme("auritus") |> 
      e_color(color = c("#8f4343", "#5c5c5c")) |> 
      e_legend(FALSE) |> 
      e_title("Histograma del costo de suelo", "¿Cómo se distribuye el costo del suelo en la CDMX?",
              left = "center",
              textStyle = list(
                color = "gray")
      )
    
    
  })
  
  
  output$rel_final_suelo <- renderEcharts4r({
    
    data_suelo |> 
      e_charts(x = Ingresos_mill) |> 
      e_scatter(total,name =  "Valor del suelo") |> 
      e_lm(total ~ Ingresos_mill, name = "Tendencia (en millones)") |> 
      e_tooltip(trigger = "axis") |> 
      e_x_axis(min = 3000) |> 
      e_y_axis(show = FALSE) |> 
      e_y_axis(show = FALSE, index = 1) |> 
      e_theme("auritus") |> 
      e_color(color = c("#de1f1f", "#787878")) |> 
      e_legend(FALSE) |> 
      e_title("Relación lineal entre las variables", "Monto asignado vs costo del suelo en la CDMX",
              left = "center",
              textStyle = list(
                color = "gray")
      )
    
  })

  
}
