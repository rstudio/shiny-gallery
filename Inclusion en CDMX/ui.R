
ui <- htmlTemplate(filename = "www/index.html",
                   
                   
                   
                   pagina = fluidPage(
                     #useSever(),
                     column(width = 4,
                            selectInput("data", "Selecciona una delegación:", choices = c("Todas las alcaldías", delegaciones)),
                            echarts4rOutput("monto_aprob", height = 350),
                            echarts4rOutput("monto_ejer", height = 350)
                     ),
                     column(width = 8,
                            leafletOutput("mapa", width = 800, height = 800)
                     )
                   ),
                   estratos_part = fluidPage(
                     fluidPage(width = 3,
                               div(style="display: inline-block;vertical-align:top; width: 240px;",selectInput("estrato", "Escoge un estrato social:", 
                                        choices = estrato_select)),
                               div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")),
                               div(style="display: inline-block;vertical-align:top; width: 240px;",selectInput("deleg_est", "Escoge una delegación:", 
                                        choices = c("Todas las alcaldías", delegaciones))),
                               div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")),
                            div(style="display: inline-block;vertical-align:top; width: 240px;",selectInput("filtfinal", "Selecciona el dato:", 
                                        choices = NULL)),
                            div(style="display: inline-block;vertical-align:top; width: 25px;",HTML("<br>")),
                            div(style="display: inline-block;vertical-align:top; width: 240px;",awesomeRadio(
                              inputId = "sexoedad",
                              label = "Visualizar por:", 
                              choices = c("sexo", "edad"),
                              selected = "sexo",
                              inline = FALSE, 
                              status = "success"
                            ))
                     ),
                     column(width = 6,
                            echarts4rOutput("graf_estr")
                            
                            
                     ),
                     column(
                       width = 6,
                       leafletOutput("mapa_estratos", width = 500, height = 500),
                       br(),br(),br()
                     )
                   ),
                   
                   est_finales = fluidPage(
                     
                     column(width = 12,
                            align=" center",
                            
                            echarts4rOutput("comp_final", height = 250, width = 1100)
                            
                            ),
                     column(width = 6,
                            align="center",
                            
                            echarts4rOutput("pie_final", width = 600, height = 350),
                            ),
                     column(width = 6,
                            align="center",
                            
                            echarts4rOutput("rel_final", width = 600, height = 350),
                     )
                     
                     
                   ),
                   
                   
                   est_finales_suelo = fluidPage(
                     
                     column(width = 12,
                            align=" center",
                            
                            echarts4rOutput("comp_final_suelo", height = 250, width = 1100)
                            
                     ),
                     column(width = 6,
                            align="center",
                            
                            echarts4rOutput("hist_final", width = 600, height = 350),
                     ),
                     column(width = 6,
                            align="center",
                            
                            echarts4rOutput("rel_final_suelo", width = 600, height = 350),
                     )
                     
                     
                   )
                   
                )
                   


