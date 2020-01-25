
## load
source('share_load.R')

## 000 user input setup. Please pay close attention and change -----
## un comtrade max year, you can find it on https://comtrade.un.org/data/da
tmp_un_comtrade_max_year <- year(Sys.time()) - 2 # 2 years of lag

## build server.R
server <- 
   function(input, output, session) {
      ## I. Main dashboard -----------------------------
      i_prog <- 1
      tot_step <- 25
      
      # 1. Value boxes  ---------------------------------------------------------
      ## try add progress bars
      withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
         # Increment the progress bar, and update the detail text.
         incProgress( i_prog/tot_step, detail = NULL)
         ##Sys.sleep(0.1)
         
      })
      i_prog <- i_prog + 1
      
      tmp_ex_g <-
         dtf_shiny_full %>%
         filter( Country == 'World',
                 Year == max(Year),
                 Type_ie == 'Exports',
                 Type_gs == 'Goods'
         ) %>%
         group_by( Year ) %>%
         summarise( Value = round(sum(Value/10^6),0) ) %>%
         dplyr::ungroup() %>%
         dplyr::select(Value) %>%
         as.numeric

      ###
      tmp_ex_s <-
         dtf_shiny_full %>%
         filter( Country == 'World',
                 Year == max(Year),
                 Type_ie == 'Exports',
                 Type_gs == 'Services'
         ) %>%
         group_by( Year ) %>%
         summarise( Value = round(sum(Value/10^6),0) ) %>%
         dplyr::ungroup() %>%
         dplyr::select(Value) %>%
         as.numeric

      ###
      tmp_ex_tot <- tmp_ex_g + tmp_ex_s

      ###
      tmp_im_g <-
         dtf_shiny_full %>%
         filter( Country == 'World',
                 Year == max(Year),
                 Type_ie == 'Imports',
                 Type_gs == 'Goods'
         ) %>%
         group_by( Year ) %>%
         summarise( Value = round(sum(Value/10^6),0) ) %>%
         dplyr::ungroup() %>%
         dplyr::select(Value) %>%
         as.numeric

      ###
      tmp_im_s <-
         dtf_shiny_full %>%
         filter( Country == 'World',
                 Year == max(Year),
                 Type_ie == 'Imports',
                 Type_gs == 'Services'
         ) %>%
         group_by( Year ) %>%
         summarise( Value = round(sum(Value/10^6),0) ) %>%
         dplyr::ungroup() %>%
         dplyr::select(Value) %>%
         as.numeric

      ###
      tmp_im_tot <- tmp_im_g + tmp_im_s

      ###
      tmp_balance_g <- tmp_ex_g - tmp_im_g
      tmp_balance_s <- tmp_ex_s - tmp_im_s
      tmp_balance_tot <- tmp_balance_g + tmp_balance_s
      
      ## build GODDS value boxes
      output$ExGBox <- renderValueBox({
         valueBox(
            VB_style( paste0( '$',format(tmp_ex_g,big.mark=','), " m" ),  "font-size: 60%;"  ),
            VB_style( paste0("Goods exports (", round(tmp_ex_g/tmp_ex_tot*100,0) ,"%)")  ), 
            icon = icon('export', lib = 'glyphicon'), #icon("sign-in"),
            color = "green"
         )
      })
      
      ###
      output$ImGBox <- renderValueBox({
         valueBox(
            VB_style( paste0( '$', format(tmp_im_g, big.mark = ','), " m"),  "font-size: 60%;"  ),
            paste0("Goods imports (", round(tmp_im_g/tmp_im_tot*100,0) ,"%)"), 
            icon = icon('import', lib = 'glyphicon'),# icon("sign-out"),
            color = "red"
         )
      })
      
      ###
      output$BlGBox <- renderValueBox({
         valueBox(
            VB_style( paste0( ifelse( tmp_balance_g>0, '+', '-' ), '$', format(abs(tmp_balance_g),big.mark=','), " m"),  "font-size: 60%;"  ),
            "Goods balance", 
            icon = icon("balance-scale"),
            color = ifelse( tmp_balance_g>0, 'green', 'red' )
         )
      })
      
      ## build Services value boxes
      output$ExSBox <- renderValueBox({
         valueBox(
            VB_style( paste0( '$', format(tmp_ex_s,big.mark=','), " m"), "font-size: 60%;"  ),
            paste0("Services exports (", round(tmp_ex_s/tmp_ex_tot*100,0) ,"%)"), 
            icon = icon('export', lib = 'glyphicon'),#icon("sign-in"),
            color = "green"
         )
      })
      
      ###
      output$ImSBox <- renderValueBox({
         valueBox(
            VB_style( paste0( '$',format(tmp_im_s, big.mark = ','), " m"),"font-size: 60%;"  ),
            paste0("Services imports (", round(tmp_im_s/tmp_im_tot*100,0) ,"%)"), 
            icon = icon('import', lib = 'glyphicon'), #icon("sign-out"),
            color = "red"
         )
      })
      
      ###
      output$BlSBox <- renderValueBox({
         valueBox(
            VB_style( paste0( ifelse( tmp_balance_s>0, '+', '-' ),'$',format(abs(tmp_balance_s),big.mark=','), " m"), "font-size: 60%;"  ),
            "Services balance", 
            icon = icon("balance-scale"),
            color = ifelse( tmp_balance_s>0, 'green', 'red' )
         )
      })
      
      ## build Total trade value boxes
      output$ExTotBox <- renderValueBox({
         valueBox(
            VB_style( paste0( '$',format(tmp_ex_tot,big.mark=','), " m"), "font-size: 60%;"  ),
            "Total exports", 
            icon = icon('export', lib = 'glyphicon'), # icon("sign-in"),
            color = "green"
         )
      })
      
      ###
      output$ImTotBox <- renderValueBox({
         valueBox(
            VB_style( paste0( '$',format(tmp_im_tot, big.mark = ','), " m"),"font-size: 60%;"  ),
            "Total imports", 
            icon = icon('import', lib = 'glyphicon'), #icon("sign-out"),
            color = "red"
         )
      })
      
      ###
      output$BlTotBox <- renderValueBox({
         valueBox(
            VB_style( paste0( ifelse( tmp_balance_tot>0, '+', '-' ),'$', format(abs(tmp_balance_tot),big.mark=','), " m"),"font-size: 60%;"  ),
            "Trade balance", 
            icon = icon("balance-scale"),
            color = ifelse( tmp_balance_tot>0, 'green', 'red' )
         )
      })
      
      
      # 2. Total Trade a line chart  -----------------------------------------------------------------
      withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
         # Increment the progress bar, and update the detail text.
         incProgress( i_prog/tot_step, detail = NULL)
         ##Sys.sleep(0.1)
         
      })
      i_prog <- i_prog + 1
      
      tmp_dtf <-
         dtf_shiny_full %>%
         filter( Country == 'World',
                 #Type_ie == 'Imports',
                 Year >= (max(Year) - 20) ) %>%
         mutate( Value = round(Value/10^6) )

      output$IEGSLineHc <-renderHighchart({
         highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'Goods exports', data =tmp_dtf$Value[tmp_dtf$Type_gs=='Goods'&tmp_dtf$Type_ie=='Exports'], color='green', marker = list(symbol = 'circle') ),
                       list(name = 'Services exports', data =tmp_dtf$Value[tmp_dtf$Type_gs=='Services'&tmp_dtf$Type_ie=='Exports'], color = 'green', dashStyle = 'shortDot', marker = list(symbol = 'triangle') ),
                       list(name = 'Goods imports', data =tmp_dtf$Value[tmp_dtf$Type_gs=='Goods'&tmp_dtf$Type_ie=='Imports'], color = 'red', marker = list(symbol = 'circle') ),
                       list(name = 'Services imports', data =tmp_dtf$Value[tmp_dtf$Type_gs=='Services'&tmp_dtf$Type_ie=='Imports'], color = 'red', dashStyle = 'shortDot', marker = list(symbol = 'triangle')  )
            )%>%
            hc_xAxis( categories = unique(tmp_dtf$Year) ) %>%
            hc_yAxis( title = list(text = "$ million, NZD"),
                      labels = list( format = "${value:,.0f} m")  ) %>%
            hc_plotOptions(column = list(
               dataLabels = list(enabled = F),
               #stacking = "normal",
               enableMouseTracking = T ) 
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: ${point.y} m"),
                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
      })
      
      # 2.1 Total Trade balance a line chart  -----------------------------------------------------------------
      withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
         # Increment the progress bar, and update the detail text.
         incProgress( i_prog/tot_step, detail = NULL)
         ##Sys.sleep(0.1)
         
      })
      i_prog <- i_prog + 1
      
      tmp_dtf_balance <-
         dtf_shiny_full %>%
         filter( Country == 'World',
                 #Type_ie == 'Imports',
                 Year >= (max(Year) - 20) ) %>%
         group_by( Year, Country, Type_gs ) %>%
         mutate( Value = Value[ Type_ie == 'Exports'] - Value[ Type_ie == 'Imports']  ) %>%
         ungroup %>%
         filter( Type_ie == 'Exports' ) %>%
         mutate(  Type_gs = paste0(Type_gs, ' balance') )
      
      tmp_dtf_balance_tot <-
         tmp_dtf_balance %>%
         group_by( Year, Country, Type_ie ) %>%
         summarise( Value = sum(Value, na.rm=T) ) %>%
         ungroup %>%
         mutate( Type_gs = 'Trade balance' )
      
      tmp_dtf_balance %<>%
         bind_rows( tmp_dtf_balance_tot  ) %>%
         mutate( Value = round(Value/10^6) )
      
      output$GSTotalBalanceLineHc <-renderHighchart({
         highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_chart(type = 'line') %>%
            hc_series( list(name = 'Trade balance', data =tmp_dtf_balance$Value[tmp_dtf_balance$Type_gs=='Trade balance'], color='brown' , marker = list(enabled = F), lineWidth = 3 ),
                       list(name = 'Goods balance', data =tmp_dtf_balance$Value[tmp_dtf_balance$Type_gs=='Goods balance'], color = 'darkgreen', dashStyle = 'shortDot', marker = list(symbol = 'circle') ),
                       list(name = 'Services balance', data =tmp_dtf_balance$Value[tmp_dtf_balance$Type_gs=='Services balance'], color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') )
            )%>%
            hc_xAxis( categories = unique(tmp_dtf_balance$Year) ) %>%
            hc_yAxis( title = list(text = "$ million, NZD"),
                      labels = list( format = "${value:,.0f} m"),
                      plotLines = list(
                         list(#label = list(text = "This is a plotLine"),
                            color = "#ff0000",
                            #dashStyle = 'shortDot',
                            width = 2,
                            value = 0 ) )
                      ) %>%
            hc_plotOptions(column = list(
               dataLabels = list(enabled = F),
               #stacking = "normal",
               enableMouseTracking = T ) 
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: ${point.y} m"),
                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
      })


      # 3. Growth prospective ---------------------------------------------------
      withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
         # Increment the progress bar, and update the detail text.
         incProgress( i_prog/tot_step, detail = NULL)
         ##Sys.sleep(0.1)
         
      })
      i_prog <- i_prog + 1
      
      tmp_tot <-
         tmp_dtf %>%
         group_by( Year, Type_ie ) %>%
         summarise( Value = sum(Value,na.rm=T) ) %>%
         ungroup( ) %>%
         mutate( Name = paste0('Total', ' ', tolower(Type_ie)) )

      tmp_tab <-
         tmp_dtf %>%
         mutate( Name = paste0( Type_gs,' ', tolower(Type_ie) ) ) %>%
         bind_rows( tmp_tot ) %>%
         group_by( Name) %>%
         mutate( CAGR1 = CAGR( Value[Year == max(Year)]/
                                  Value[Year == (max(Year)-1)], 1)/100,
                 CAGR5 = CAGR( Value[Year == max(Year)]/
                                  Value[Year == (max(Year)-5)], 5)/100,
                 CAGR10 = CAGR( Value[Year == max(Year)]/
                                   Value[Year == (max(Year)-10)], 10)/100,
                 CAGR20 = CAGR( Value[Year == max(Year)]/
                                   Value[Year == (max(Year)-20)], 20)/100
         ) %>%
         ungroup %>%
         filter( Year == max(Year) ) %>%
         dplyr::select( Name, Value, CAGR1, CAGR5, CAGR10, CAGR20 ) %>%
         mutate( Name = factor(Name, levels = c("Total exports",
                                       'Goods exports',
                                       'Services exports',
                                       'Total imports',
                                       'Goods imports',
                                       'Services imports')) ) %>%
         arrange( Name )


      output$GrowthTab <- renderDataTable({
         datatable( tmp_tab,
                    rownames = F,
                    extensions = 'Buttons',
                    options = list(dom = 'Bt', 
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   scrollX = TRUE) ,
                    colnames=c(" ", 'Value ($m)', 'CAGR 1', 'CAGR 5', 'CAGR 10', 'CAGR 20')
                   ) %>%
            formatStyle(columns = 'Name',
                        target = 'row',
                        fontWeight = styleEqual(c('Total imports','Total exports'), c('bold','bold')),
                        backgroundColor = styleEqual(c('Total imports','Total exports'), c('lightgrey','lightgrey'))
                       ) %>%
            formatStyle(
               c('CAGR1', 'CAGR5', 'CAGR10', 'CAGR20'),
               background = styleColorBar( c(0,max(c(tmp_tab$CAGR1,tmp_tab$CAGR5, tmp_tab$CAGR10, tmp_tab$CAGR20))*2) , 'lightblue'),
               backgroundSize = '100% 90%',
               backgroundRepeat = 'no-repeat',
               backgroundPosition = 'center'
            ) %>%
            formatPercentage( c('CAGR1', 'CAGR5', 'CAGR10', 'CAGR20'),digit = 1 ) %>%
            formatStyle( columns = c('Name','Value','CAGR1', 'CAGR5', 'CAGR10', 'CAGR20'), `font-size`= '115%' ) %>%
            formatCurrency( columns = c('Value'), mark = " ", digits = 0)
      })


      ## remove the waiting message -- 
      removeUI( selector = '#main_wait_message' )
      
      # 7.10  Show more button --------------------
      observeEvent( input$btn_show_more,
                    {
                       
                       ## disable the buttone ---
                       shinyjs::disable("btn_show_more")
                       ## --- hide message to show more -----
                       shinyjs::hide(id = 'message_to_show_more')
                       ## --- show loading message ---
                       shinyjs::show( id = "load_more_message" )
                       
                       # 4. Treemap key export commodity and services ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       ## commodity concordance is download from http://tariffdata.wto.org/ReportersAndProducts.aspx
                       tmp_tm_ex <- treemap( dtf_shiny_commodity_service_ex %>%
                                                filter(Year==max(Year)) %>%
                                                mutate( Value = Value/10^6) ,
                                             index = c("Type_gs", "SNZ_commodity"),
                                             vSize = "Value",
                                             vColor = "CAGR5",
                                             type = 'value',
                                             #aspRatio = 1.618,
                                             overlap.labels = 1,
                                             fun.aggregate = "weighted.mean",
                                             #palette = "RdYlGn",
                                             draw = FALSE)
                       
                       ## modify Goods and Service CAGR 5 change
                       tmp_tm_ex$tm$vColor[tmp_tm_ex$tm$vSize == tmp_ex_g] <-
                          tmp_tm_ex$tm$vColorValue[tmp_tm_ex$tm$vSize == tmp_ex_g] <- tmp_tab$CAGR5[tmp_tab$Name =='Goods exports']*100
                       
                       tmp_tm_ex$tm$vColor[tmp_tm_ex$tm$vSize == tmp_ex_s] <-
                          tmp_tm_ex$tm$vColorValue[tmp_tm_ex$tm$vSize == tmp_ex_s] <- tmp_tab$CAGR5[tmp_tab$Name =='Services exports']*100
                       
                       ## not highlight confidential data
                       #tmp_tm$tm$vColor[tmp_tm$tm$SNZ_commodity == 'Confidential data'] <-
                       #   tmp_tm$tm$vColorValue[tmp_tm$tm$SNZ_commodity == 'Confidential data'] <-
                       #   tmp_tm$tm$color[tmp_tm$tm$SNZ_commodity == 'Confidential data'] <- NA
                       
                       output$KeyExTM <- renderHighchart({
                          highchart() %>%
                             hc_add_series_treemap2( tmp_tm_ex , #hctreemap
                                                     allowDrillToNode = TRUE,
                                                     layoutAlgorithm = "squarified",
                                                     levelIsConstant = FALSE,
                                                     levels = list(list(level = 1,
                                                                        dataLabels = list(enabled = TRUE,
                                                                                          style = list(fontSize = '20px', color = 'white',
                                                                                                       fontWeight = 'normal'),
                                                                                          backgroundColor = 'lightgrey',
                                                                                          align = 'left', verticalAlign = 'top'),
                                                                        borderColor = "#555",
                                                                        borderWidth = 2 ),
                                                                   list(level = 2,
                                                                        dataLabels = list(enabled = TRUE,
                                                                                          style = list(fontSize = '9px',
                                                                                                       fontWeight = 'normal')
                                                                        )
                                                                   )
                                                     )
                             ) %>%
                             hc_chart(backgroundColor = NULL, plotBorderColor = "#555", plotBorderWidth = 2) %>%
                             hc_title(text = "key commodities and services EXPORTS") %>%
                             hc_subtitle(text = "Coloured by compound annual growth rate (CAGR) for the past 5 years (%)") %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                                        Export value: ${point.value:,.0f} m <br>
                                        CAGR 5: {point.colorValue:,.1f}%") %>% 
                             hc_colorAxis(minColor = tmp_tm_ex$tm$color[which.min(tmp_tm_ex$tm$vColorValue)],
                                          maxColor = tmp_tm_ex$tm$color[which.max(tmp_tm_ex$tm$vColorValue)] ,
                                          labels = list(format = "{value}%", useHTML = TRUE), reversed = FALSE
                             ) %>%
                             hc_legend(align = "right", layout = "vertical", verticalAlign = "top",
                                       reversed = TRUE , y = 70, symbolHeight = 250, itemMarginTop = 10)
                       })
                       
                       
                       # 4.2 Treemap key import commodity and services ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       ## commodity concordance is download from http://tariffdata.wto.org/ReportersAndProducts.aspx
                       tmp_tm_im <- treemap( dtf_shiny_commodity_service_im %>%
                                                filter(Year==max(Year)) %>%
                                                mutate( Value = Value/10^6 ) ,
                                             index = c("Type_gs", "SNZ_commodity"),
                                             vSize = "Value",
                                             vColor = "CAGR5",
                                             type = 'value',
                                             #aspRatio = 1.618,
                                             overlap.labels = 1,
                                             fun.aggregate = "weighted.mean",
                                             #palette = "RdYlGn",
                                             draw = FALSE)
                       
                       ## modify Goods and Service CAGR 5 change
                       tmp_tm_im$tm$vColor[tmp_tm_im$tm$vSize == tmp_im_g] <-
                          tmp_tm_im$tm$vColorValue[tmp_tm_im$tm$vSize == tmp_im_g] <- tmp_tab$CAGR5[tmp_tab$Name =='Goods imports']*100
                       
                       tmp_tm_im$tm$vColor[tmp_tm_im$tm$vSize == tmp_im_s] <-
                          tmp_tm_im$tm$vColorValue[tmp_tm_im$tm$vSize == tmp_im_s] <- tmp_tab$CAGR5[tmp_tab$Name =='Services imports']*100
                       
                       ## not highlight confidential data
                       #tmp_tm$tm$vColor[tmp_tm$tm$SNZ_commodity == 'Confidential data'] <-
                       #   tmp_tm$tm$vColorValue[tmp_tm$tm$SNZ_commodity == 'Confidential data'] <-
                       #   tmp_tm$tm$color[tmp_tm$tm$SNZ_commodity == 'Confidential data'] <- NA
                       
                       output$KeyImTM <- renderHighchart({
                          highchart() %>%
                             hc_add_series_treemap2( tmp_tm_im , #         hctreemap(
                                                     allowDrillToNode = TRUE,
                                                     layoutAlgorithm = "squarified",
                                                     levelIsConstant = FALSE,
                                                     levels = list(list(level = 1,
                                                                        dataLabels = list(enabled = TRUE,
                                                                                          style = list(fontSize = '20px', color = 'white',
                                                                                                       fontWeight = 'normal'),
                                                                                          backgroundColor = 'lightgrey',
                                                                                          align = 'left', verticalAlign = 'top'),
                                                                        borderColor = "#555",
                                                                        borderWidth = 2 ),
                                                                   list(level = 2,
                                                                        dataLabels = list(enabled = TRUE,
                                                                                          style = list(fontSize = '9px',
                                                                                                       fontWeight = 'normal')
                                                                        )
                                                                   )
                                                     )
                             ) %>%
                             hc_chart(backgroundColor = NULL, plotBorderColor = "#555", plotBorderWidth = 2) %>%
                             hc_title(text = "key commodities and services IMPORTS") %>%
                             hc_subtitle(text = "Coloured by compound annual growth rate (CAGR) for the past 5 years (%)") %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                                        Import value: ${point.value:,.0f} m <br>
                                        CAGR 5: {point.colorValue:,.1f}%") %>% 
                             hc_colorAxis(minColor = tmp_tm_im$tm$color[which.min(tmp_tm_im$tm$vColorValue)],
                                          maxColor = tmp_tm_im$tm$color[which.max(tmp_tm_im$tm$vColorValue)] ,
                                          labels = list(format = "{value}%", useHTML = TRUE), reversed = FALSE
                             ) %>%
                             hc_legend(align = "right", layout = "vertical", verticalAlign = "top",
                                       reversed = TRUE , y = 70, symbolHeight = 250, itemMarginTop = 10)
                       })
                       
                       
                       # 5.0 Top key commodities and export over time -----------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_top_g_ex <-
                          dtf_shiny_commodity_service_ex %>%
                          filter( Year == max(Year),
                                  Type_gs == 'Goods',
                                  !SNZ_commodity %in% c('Confidential data', 'Other goods'),
                                  Value >= 10^9) %>% ## 1 bn commodity
                          arrange( -Value ) %>%
                          dplyr::select( SNZ_commodity ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_top_s_ex <-
                          dtf_shiny_commodity_service_ex %>%
                          filter( Year == max(Year),
                                  Type_gs == 'Services',
                                  !SNZ_commodity %in% c('Other business services', 'Other services'),
                                  Value >= (10^9)
                          ) %>%
                          arrange( -Value ) %>%
                          dplyr::select( SNZ_commodity ) %>%
                          as.matrix() %>%
                          as.character
                       
                       ## top 10 commodities and top 5services
                       tmp_top_ex <- c( tmp_top_g_ex, tmp_top_s_ex)
                       
                       tmp_dtf_key_line_ex <- dtf_shiny_commodity_service_ex %>%
                          filter( SNZ_commodity %in% tmp_top_ex,
                                  Year >=2007) %>%
                          mutate( Value = round(Value/10^6),
                                  SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_ex)
                          ) %>%
                          arrange( SNZ_commodity )
                       
                       ### plot
                       output$KeyExLine <- renderHighchart({
                          highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_add_series( data =  tmp_dtf_key_line_ex %>% filter( Type_gs == 'Goods' ) ,
                                            mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                            type = 'line',
                                            marker = list(symbol = 'circle') ,
                                            visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                             ) %>%
                             hc_add_series( data =  tmp_dtf_key_line_ex %>% filter( Type_gs == 'Services' ),
                                            mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                            type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                            visible = c(T,rep(F,length(tmp_top_s_ex)-1))
                             ) %>%
                             hc_xAxis( categories = c( unique( tmp_dtf_key_line_ex$Year) ) ) %>%
                             hc_yAxis( title = list(text = "$ million, NZD"), #"Commodities and services exports over $1 bn"
                                       labels = list( format = "${value:,.0f} m")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: ${point.y} m"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       })
                       
                       # 5.0.1 Top key commodities exports over time -- Percentage -------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_tot_ex <-
                          dtf_shiny_full %>%
                          filter( Country == 'World',
                                  Type_ie == 'Exports',
                                  Year >= 2007 )  %>%
                          mutate( Value = round(Value/10^6) ) %>%
                          group_by( Year, Country, Type_ie ) %>%
                          summarize( Value = sum(Value, na.rm=T) ) %>%
                          ungroup %>%
                          mutate( SNZ_commodity = 'Total exports' )
                       
                       tmp_dtf_percent_line_ex <-
                          tmp_dtf_key_line_ex %>%
                          bind_rows( tmp_tot_ex ) %>%
                          group_by( Year, Country, Type_ie ) %>%
                          mutate( Share = Value/Value[SNZ_commodity=='Total exports'],
                                  Value = Share*100 ) %>%
                          ungroup %>%
                          filter( SNZ_commodity != 'Total exports' ) %>%
                          mutate( SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_ex) ) %>%
                          arrange( SNZ_commodity )
                       
                       ### plot
                       tmp_export_percent_hc <- 
                          highchart() %>%
                          hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                          hc_xAxis( categories = c( unique( tmp_dtf_percent_line_ex$Year) ) ) %>%
                          hc_yAxis( title = list(text = "Percentage (%)"),
                                    labels = list( format = "{value:,.1f} %")  ) %>%
                          hc_plotOptions(line = list(
                             dataLabels = list(enabled = F),
                             #stacking = "normal",
                             enableMouseTracking = T)
                          )%>%
                          hc_tooltip(table = TRUE,
                                     sort = TRUE,
                                     pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                           " {series.name}: {point.y:,.1f} %"),
                                     headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                          ) %>%
                          hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       #hc_legend( enabled = FALSE )
                       
                       ### if any services are selected?
                       if( length(tmp_top_g_ex)>=1&length(tmp_top_s_ex)==0 ) {
                          output$KeyExLinePercent <- 
                             renderHighchart({
                                tmp_export_percent_hc %>%
                                   hc_add_series( data =  tmp_dtf_percent_line_ex %>% filter( Type_gs == 'Goods' ) ,
                                                  mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                  type = 'line',
                                                  marker = list(symbol = 'circle') ,
                                                  visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                   )
                             })
                       }
                       if( length(tmp_top_g_ex)==0 & length(tmp_top_s_ex)>=1 ){
                          output$KeyExLinePercent <- 
                             renderHighchart({
                                tmp_export_percent_hc %>%
                                   hc_add_series( data =  tmp_dtf_percent_line_ex %>% filter( Type_gs == 'Services' ),
                                                  mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                  type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                  visible = c(T,rep(F,length(tmp_top_s_ex)-1))
                                   )
                             })
                       }
                       if( length(tmp_top_g_ex)>=1 & length(tmp_top_s_ex)>=1 ){
                          output$KeyExLinePercent <- 
                             renderHighchart({
                                tmp_export_percent_hc %>%
                                   hc_add_series( data =  tmp_dtf_percent_line_ex %>% filter( Type_gs == 'Goods' ) ,
                                                  mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                  type = 'line',
                                                  marker = list(symbol = 'circle') ,
                                                  visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                   ) %>%
                                   hc_add_series( data =  tmp_dtf_percent_line_ex %>% filter( Type_gs == 'Services' ),
                                                  mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                  type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                  visible = c(T,rep(F,length(tmp_top_s_ex)-1))
                                   )
                             })
                       }
                       
                       
                       # 5.1 Top key commodities and import over time -----------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_top_g_im <-
                          dtf_shiny_commodity_service_im %>%
                          filter( Year == max(Year),
                                  Type_gs == 'Goods',
                                  !SNZ_commodity %in% c('Confidential data', 'Other goods'),
                                  Value >= 10^9) %>% ## 1 bn commodity
                          arrange( -Value ) %>%
                          dplyr::select( SNZ_commodity ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_top_s_im <-
                          dtf_shiny_commodity_service_im %>%
                          filter( Year == max(Year),
                                  Type_gs == 'Services',
                                  !SNZ_commodity %in% c('Other business services', 'Other services'),
                                  Value >= (10^9)
                          ) %>%
                          arrange( -Value ) %>%
                          dplyr::select( SNZ_commodity ) %>%
                          as.matrix() %>%
                          as.character
                       
                       ## top 10 commodities and top 5services
                       tmp_top_im <- c( tmp_top_g_im, tmp_top_s_im)
                       
                       tmp_dtf_key_line_im <- dtf_shiny_commodity_service_im %>%
                          filter( SNZ_commodity %in% tmp_top_im,
                                  Year >=2007) %>%
                          mutate( Value = round(Value/10^6),
                                  SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_im)
                          ) %>%
                          arrange( SNZ_commodity )
                       
                       ### plot
                       output$KeyImLine <- renderHighchart({
                          highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_add_series( data =  tmp_dtf_key_line_im %>% filter( Type_gs == 'Goods' ) ,
                                            mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                            type = 'line',
                                            marker = list(symbol = 'circle') ,
                                            visible = c(T,rep(F,length(tmp_top_g_im)-1))
                             ) %>%
                             hc_add_series( data =  tmp_dtf_key_line_im %>% filter( Type_gs == 'Services' ),
                                            mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                            type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                            visible = c(T,rep(F,length(tmp_top_s_im)-1))
                             ) %>%
                             hc_xAxis( categories = c( unique( tmp_dtf_key_line_im$Year) ) ) %>%
                             hc_yAxis( title = list(text = "$ million, NZD"), # "Commodities and services imports over $1 bn"
                                       labels = list( format = "${value:,.0f} m")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: ${point.y} m"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       })
                       
                       # 5.1.1 Top key commodities and import over time Percent -----------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_tot_im <-
                          dtf_shiny_full %>%
                          filter( Country == 'World',
                                  Type_ie == 'Imports',
                                  Year >= 2007 )  %>%
                          mutate( Value = round(Value/10^6) ) %>%
                          group_by( Year, Country, Type_ie ) %>%
                          summarize( Value = sum(Value, na.rm=T) ) %>%
                          ungroup %>%
                          mutate( SNZ_commodity = 'Total imports' )
                       
                       tmp_dtf_percent_line_im <-
                          tmp_dtf_key_line_im %>%
                          bind_rows( tmp_tot_im ) %>%
                          group_by( Year, Country, Type_ie ) %>%
                          mutate( Share = Value/Value[SNZ_commodity=='Total imports'],
                                  Value = Share*100 ) %>%
                          ungroup %>%
                          filter( SNZ_commodity != 'Total imports' ) %>%
                          mutate( SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_im) ) %>%
                          arrange( SNZ_commodity )
                       
                       ### plot
                       tmp_import_percent_hc <- 
                          highchart() %>%
                          hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                          hc_xAxis( categories = c( unique( tmp_dtf_percent_line_im$Year) ) ) %>%
                          hc_yAxis( title = list(text = "Percentage (%)"),
                                    labels = list( format = "{value:,.1f} %")  ) %>%
                          hc_plotOptions(line = list(
                             dataLabels = list(enabled = F),
                             #stacking = "normal",
                             enableMouseTracking = T)
                          )%>%
                          hc_tooltip(table = TRUE,
                                     sort = TRUE,
                                     pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                           " {series.name}: {point.y:,.1f} %"),
                                     headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                          ) %>%
                          hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       #hc_legend( enabled = FALSE )
                       
                       ### if any services are selected?
                       if( length(tmp_top_g_im)>=1&length(tmp_top_s_im)==0 ) {
                          output$KeyImLinePercent <- 
                             renderHighchart({
                                tmp_import_percent_hc %>%
                                   hc_add_series( data =  tmp_dtf_percent_line_im %>% filter( Type_gs == 'Goods' ) ,
                                                  mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                  type = 'line',
                                                  marker = list(symbol = 'circle') ,
                                                  visible = c(T,rep(F,length(tmp_top_g_im)-1))
                                   )
                             })
                       }
                       if( length(tmp_top_g_im)==0 & length(tmp_top_s_im)>=1 ){
                          output$KeyImLinePercent <- 
                             renderHighchart({
                                tmp_import_percent_hc %>%
                                   hc_add_series( data =  tmp_dtf_percent_line_im %>% filter( Type_gs == 'Services' ),
                                                  mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                  type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                  visible = c(T,rep(F,length(tmp_top_s_im)-1))
                                   )
                             })
                       }
                       if( length(tmp_top_g_im)>=1 & length(tmp_top_s_im)>=1 ){
                          output$KeyImLinePercent <- 
                             renderHighchart({
                                tmp_import_percent_hc %>%
                                   hc_add_series( data =  tmp_dtf_percent_line_im %>% filter( Type_gs == 'Goods' ) ,
                                                  mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                  type = 'line',
                                                  marker = list(symbol = 'circle') ,
                                                  visible = c(T,rep(F,length(tmp_top_g_im)-1))
                                   ) %>%
                                   hc_add_series( data =  tmp_dtf_percent_line_im %>% filter( Type_gs == 'Services' ),
                                                  mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                  type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                  visible = c(T,rep(F,length(tmp_top_s_im)-1))
                                   )
                             })
                       }
                       
                       # 6. Global trading partners glance ---------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       output$TradeMap <- 
                          renderUI({
                             tags$iframe(#srcdoc = paste(readLines("www/Twoway_trade_by_country.html"), 
                                #               collapse = '\n'),
                                src = "Twoway_trade_by_country.html",
                                height="550px", width="100%")
                          })
                       
                       # 7.0 FTA timeline ----------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       ### FTA infomration
                       # dtf_fta <- 
                       #    data.frame(
                       #       id = 1:9,
                       #       content = c("<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-china-free-trade-agreement/' target = '_blank'> NZ-China FTA </a>",
                       #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-australia-closer-economic-relations-cer/' target = '_blank'> NZ-Australia CER </a>",
                       #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/aanzfta-asean-australia-new-zealand-fta/' target = '_blank'> AANZFTA </a>",
                       #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/hong-kong-fta/' target = '_blank'> NZ-Hong Kong, China CEP </a>",
                       #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/malaysia-fta/' target = '_blank'> NZ-Malaysia FTA </a>",
                       #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/singapore/' target = '_blank'> NZ-Singapore CEP </a>",
                       #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/thailand/' target = '_blank'> NZ-Thailand CEP </a>",
                       #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/p4/' target = '_blank'> P4 </a>",
                       #                   "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-korea-free-trade-agreement/' target = '_blank'> NZ-Korea FTA </a>"),
                       #       ## time when FTAs in forece
                       #       start = c("2008-04-07",# cn
                       #                 "1983-01-01",# aus
                       #                 "2010-01-01",# asean
                       #                 "2011-01-01",# hk
                       #                 "2010-08-01", #my
                       #                 "2001-01-01", #sing
                       #                 "2005-07-01", # Thai
                       #                 "2006-01-01", #p4
                       #                 "2015-12-20"
                       #       )
                       #       
                       #    )
                       # 
                       # output$FTATimeLine <- 
                       #    renderTimevis({ timevis(dtf_fta) })
                       
                       ### FTA infomration
                       groups <- 
                          data.frame( id = c('cn', 'aus', 'asean',
                                             'hk', 'my', 'sin',
                                             'thai', 'p4', 'sk'),
                                      content =c("<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-china-free-trade-agreement/' target = '_blank'> NZ-China FTA </a>",
                                                 "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-australia-closer-economic-relations-cer/' target = '_blank'> NZ-Australia CER </a>",
                                                 "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/aanzfta-asean-australia-new-zealand-fta/' target = '_blank'> AANZFTA </a>",
                                                 "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/hong-kong-fta/' target = '_blank'> NZ-Hong Kong, China CEP </a>",
                                                 "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/malaysia-fta/' target = '_blank'> NZ-Malaysia FTA </a>",
                                                 "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/singapore/' target = '_blank'> NZ-Singapore CEP </a>",
                                                 "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/thailand/' target = '_blank'> NZ-Thailand CEP </a>",
                                                 "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/p4/' target = '_blank'> P4 </a>",
                                                 "<a href = 'https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/nz-korea-free-trade-agreement/' target = '_blank'> NZ-Korea FTA </a>")
                          )
                       
                       
                       dtf_fta <- 
                          data.frame(
                             id = 1:9,
                             content = c("5 years",
                                         "3 years",
                                         "5 years",
                                         "10 years",
                                         "5 years",
                                         "1 year and 4 months",
                                         "1 year",
                                         "2 years and 4 months",
                                         "6 years and 6 months"
                             ) ,
                             ## talk started
                             start = c("2003-10-01", # cn
                                       "1979-12-31", #aus
                                       "2005-03-01", #asean
                                       "2001-01-01", #hk
                                       "2005-03-01", # my
                                       "1999-09-01", #singapore
                                       "2004-06-01", ## thia
                                       "2003-09-01", #p4
                                       "2009-06-01" #sk
                             ),
                             ## time when FTAs in forece
                             end = c("2008-10-01",# cn
                                     "1983-01-01",# aus
                                     "2010-01-01",# asean
                                     "2011-01-01",# hk
                                     "2010-08-01", #my
                                     "2001-01-01", #sing
                                     "2005-07-01", # Thai
                                     "2006-01-01", #p4
                                     "2015-12-20" # sk
                             ),
                             group = c('cn', 'aus', 'asean',
                                       'hk', 'my', 'sin',
                                       'thai', 'p4', 'sk') #,
                             #type = 'range'
                             
                          )
                       
                       output$FTATimeLine <-
                          renderTimevis({ timevis(data = dtf_fta, groups = groups, options = list(align = 'left'))  })
                       
                       
                       # 7.1 Key exports market trend line ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_country_ex <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          group_by( Year, Country, Type_ie, Note, ISO2, lat, lon ) %>%
                          summarise( Value = sum(Value, na.rm=T) ) %>%
                          ungroup %>%
                          filter( Type_ie == 'Exports' )
                       
                       ## export markets over $500 million
                       tmp_top_country_ex <-
                          tmp_data_country_ex %>%
                          filter( Year == max(Year),
                                  Value >= (10^9), 
                                  !Country %in% c("World", 
                                                  "Destination Unknown - EU")
                          ) %>% ## 1 bn commodity
                          arrange( -Value ) %>%
                          dplyr::select( Country ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_data_country_ex  %<>%
                          filter( Country %in% tmp_top_country_ex #,
                                  #Year >=2007
                          ) %>%
                          mutate( Value = round(Value/10^6),
                                  Country = factor(Country, levels = tmp_top_country_ex)
                          ) %>%
                          arrange( Country )
                       
                       ### plot
                       output$ExMarketLine <- renderHighchart({
                          highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_add_series( data =  tmp_data_country_ex ,
                                            mapping = hcaes(  x = Year, y = Value, group = Country),
                                            type = 'line',
                                            marker = list(symbol = 'circle') ,
                                            visible = c( rep(T,5), rep(F,length(tmp_top_country_ex)-5) )
                             ) %>%
                             hc_xAxis( categories = c( unique( tmp_data_country_ex$Year) ) ) %>%
                             hc_yAxis( title = list(text = "$ million, NZD"), #"Exports markets over $1bn"
                                       labels = list( format = "${value:,.0f} m")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: ${point.y} m"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       })
                       
                       # 7.1.1 Key exports market trend line Percent ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_tot_ex <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          filter(  Type_ie == 'Exports', Country == 'World' ) %>%
                          group_by( Year, Country, Type_ie, Note ) %>%
                          summarise( Value = sum(Value, na.rm=T) ) %>%
                          ungroup 
                       
                       tmp_data_country_ex_pc <-
                          tmp_data_country_ex %>%
                          bind_rows( tmp_data_tot_ex ) %>%
                          group_by( Year, Type_ie,  Note ) %>%
                          mutate( Share = Value/(Value[Country=='World']/10^6) ) %>%
                          ungroup %>%
                          mutate( Value = Share*100 ) %>%
                          filter( Country != 'World' ) %>%
                          mutate( Country = factor(Country, levels = tmp_top_country_ex) ) %>%
                          arrange( Country, Year )
                       
                       output$ExMarketLinePercent <-
                          renderHighchart({
                             highchart() %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_add_series( data =  tmp_data_country_ex_pc ,
                                               mapping = hcaes(  x = Year, y = Value, group = Country),
                                               type = 'line',
                                               marker = list(symbol = 'circle'),
                                               visible = c( rep(T,5), rep(F,length(tmp_top_country_ex)-5) )
                                ) %>%
                                hc_xAxis( categories = c( unique( tmp_data_country_ex_pc$Year) )   ) %>%
                                hc_yAxis( title = list(text = "Percentage (%)"),
                                          labels = list( format = "{value:,.1f} %")  ) %>%
                                hc_plotOptions(line = list(
                                   dataLabels = list(enabled = F),
                                   #stacking = "normal",
                                   enableMouseTracking = T)
                                )%>%
                                hc_tooltip(table = TRUE,
                                           sort = TRUE,
                                           pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                 " {series.name}: {point.y:,.1f} %"),
                                           headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                ) %>%
                                hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          })
                       
                       # 7.2 Key imports market trend line ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_country_im <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          group_by( Year, Country, Type_ie, Note, ISO2, lat, lon ) %>%
                          summarise( Value = sum(Value, na.rm=T) ) %>%
                          ungroup %>%
                          filter( Type_ie == 'Imports' )
                       
                       ## import markets over $500 million
                       tmp_top_country_im <-
                          tmp_data_country_im %>%
                          filter( Year == max(Year),
                                  Value >= (10^9), 
                                  !Country %in% c("World", 
                                                  "Destination Unknown - EU")
                          ) %>% ## 1 bn commodity
                          arrange( -Value ) %>%
                          dplyr::select( Country ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_data_country_im  %<>%
                          filter( Country %in% tmp_top_country_im #,
                                  #Year >=2007
                          ) %>%
                          mutate( Value = round(Value/10^6),
                                  Country = factor(Country, levels = tmp_top_country_im)
                          ) %>%
                          arrange( Country )
                       
                       ### plot
                       output$ImMarketLine <- renderHighchart({
                          highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_add_series( data =  tmp_data_country_im ,
                                            mapping = hcaes(  x = Year, y = Value, group = Country),
                                            type = 'line',
                                            marker = list(symbol = 'circle') ,
                                            visible = c( rep(T,5), rep(F,length(tmp_top_country_im)-5) )
                             ) %>%
                             hc_xAxis( categories = c( unique( tmp_data_country_im$Year) ) ) %>%
                             hc_yAxis( title = list(text = "$ million, NZD"), # "Imports markets over $1bn"
                                       labels = list( format = "${value:,.0f} m")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: ${point.y} m"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       })
                       
                       # 7.2.1 Key imports market trend line Percent ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_tot_im <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          filter(  Type_ie == 'Imports', Country == 'World' ) %>%
                          group_by( Year, Country, Type_ie, Note ) %>%
                          summarise( Value = sum(Value, na.rm=T) ) %>%
                          ungroup 
                       
                       tmp_data_country_im_pc <-
                          tmp_data_country_im %>%
                          bind_rows( tmp_data_tot_im ) %>%
                          group_by( Year, Type_ie,  Note ) %>%
                          mutate( Share = Value/(Value[Country=='World']/10^6) ) %>%
                          ungroup %>%
                          mutate( Value = Share*100 ) %>%
                          filter( Country != 'World' ) %>%
                          mutate( Country = factor(Country, levels = tmp_top_country_im) ) %>%
                          arrange( Country, Year )
                       
                       output$ImMarketLinePercent <-
                          renderHighchart({
                             highchart() %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_add_series( data =  tmp_data_country_im_pc ,
                                               mapping = hcaes(  x = Year, y = Value, group = Country),
                                               type = 'line',
                                               marker = list(symbol = 'circle'),
                                               visible = c( rep(T,5), rep(F,length(tmp_top_country_im)-5) )
                                ) %>%
                                hc_xAxis( categories = c( unique( tmp_data_country_im_pc$Year) )   ) %>%
                                hc_yAxis( title = list(text = "Percentage (%)"),
                                          labels = list( format = "{value:,.1f} %")  ) %>%
                                hc_plotOptions(line = list(
                                   dataLabels = list(enabled = F),
                                   #stacking = "normal",
                                   enableMouseTracking = T)
                                )%>%
                                hc_tooltip(table = TRUE,
                                           sort = TRUE,
                                           pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                 " {series.name}: {point.y:,.1f} %"),
                                           headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                ) %>%
                                hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          })
                       
                       # 7.3 Key Two way trade market trend line ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_country_twoway <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          group_by( Year, Country, Note, ISO2, lat, lon ) %>%
                          summarise( Value = sum(Value, na.rm=T) ) %>%
                          ungroup 
                       
                       ## import markets over $500 million
                       tmp_top_country_twoway <-
                          tmp_data_country_twoway %>%
                          filter( Year == max(Year),
                                  Value >= (10^9)*2, 
                                  !Country %in% c("World", 
                                                  "Destination Unknown - EU")
                          ) %>% ## 1 bn commodity
                          arrange( -Value ) %>%
                          dplyr::select( Country ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_data_country_twoway  %<>%
                          filter( Country %in% tmp_top_country_twoway #,
                                  #Year >=2007
                          ) %>%
                          mutate( Value = round(Value/10^6),
                                  Country = factor(Country, levels = tmp_top_country_twoway)
                          ) %>%
                          arrange( Country )
                       
                       ### plot
                       output$TwowayMarketLine <- renderHighchart({
                          highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_add_series( data =  tmp_data_country_twoway ,
                                            mapping = hcaes(  x = Year, y = Value, group = Country),
                                            type = 'line',
                                            marker = list(symbol = 'circle') ,
                                            visible = c( rep(T,5), rep(F,length(tmp_top_country_twoway)-5) )
                             ) %>%
                             hc_xAxis( categories = c( unique( tmp_data_country_twoway$Year) ) ) %>%
                             hc_yAxis( title = list(text = "$ million, NZD"), # "Markets with two way trade over $2bn"
                                       labels = list( format = "${value:,.0f} m")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: ${point.y} m"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       })
                       
                       # 7.4 Trade balance market trend line ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_country_balance <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          group_by( Year, Country, Type_ie, Note, ISO2, lat, lon ) %>%
                          summarise( Value = sum(Value, na.rm=T) ) %>%
                          ungroup %>%
                          spread( Type_ie, Value ) %>%
                          mutate( Exports = ifelse( is.na(Exports), 0, Exports ),
                                  Imports = ifelse( is.na(Imports), 0, Imports ) ) %>%
                          mutate( Value = Exports - Imports )
                       
                       ## import markets over $500 million
                       tmp_top_country_balance_positive <-
                          tmp_data_country_balance %>%
                          filter( Year == max(Year),
                                  Value >= (10^9)/2, 
                                  !Country %in% c("World", 
                                                  "Destination Unknown - EU",
                                                  "Ships' Bunkering" ,
                                                  "Passengers' Effects")
                          ) %>% ## 1 bn commodity
                          arrange( -Value ) %>%
                          dplyr::select( Country ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_top_country_balance_negative <-
                          tmp_data_country_balance %>%
                          filter( Year == max(Year),
                                  Value <= -(10^9)/2, 
                                  !Country %in% c("World", 
                                                  "Destination Unknown - EU",
                                                  "Ships' Bunkering" ,
                                                  "Passengers' Effects")
                          ) %>% ## 1 bn commodity
                          arrange( Value ) %>%
                          dplyr::select( Country ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_top_country_balance <- c(tmp_top_country_balance_positive, tmp_top_country_balance_negative)
                       
                       tmp_data_country_balance  %<>%
                          filter( Country %in% tmp_top_country_balance #,
                                  #Year >=2007
                          ) %>%
                          mutate( Value = round(Value/10^6),
                                  Country = factor(Country, levels = tmp_top_country_balance)
                          ) %>%
                          arrange( Country )
                       
                       ### plot
                       output$BalanceMarketLine <- renderHighchart({
                          highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_add_series( data =  tmp_data_country_balance %>% filter( Country %in% tmp_top_country_balance_positive ) ,
                                            mapping = hcaes(  x = Year, y = Value, group = Country ),
                                            type = 'line',
                                            marker = list(symbol = 'circle') ,
                                            visible = c( rep(T,3), rep(F,length(tmp_top_country_balance_positive)-3))
                             ) %>%
                             hc_add_series( data =  tmp_data_country_balance %>% filter( Country %in% tmp_top_country_balance_negative ) ,
                                            mapping = hcaes(  x = Year, y = Value, group = Country ),
                                            type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                            visible = c( rep(T,3),rep(F,length(tmp_top_country_balance_negative)-3))
                             ) %>%
                             hc_xAxis( categories = c( unique( tmp_data_country_balance$Year) ) ) %>%
                             hc_yAxis( title = list(text = "$ million, NZD"), #"Markets with trade balance over $500m and under -$500m"
                                       labels = list( format = "${value:,.0f} m")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: ${point.y} m"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       })
                       
                       #session$allowReconnect(TRUE)
                       # 7.6 Key exports market for goods trend line ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_country_ex_g <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          filter( Type_gs == 'Goods', Type_ie == 'Exports' ) 
                       
                       ## export markets over $500 million
                       tmp_top_country_ex_g <-
                          tmp_data_country_ex_g %>%
                          filter( Year == max(Year),
                                  Value >= (10^9)/2, 
                                  !Country %in% c("World", 
                                                  "Destination Unknown - EU")
                          ) %>% ## 1 bn commodity
                          arrange( -Value ) %>%
                          dplyr::select( Country ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_data_country_ex_g  %<>%
                          filter( Country %in% tmp_top_country_ex_g ) %>%
                          mutate( Value = round(Value/10^6),
                                  Country = factor(Country, levels = tmp_top_country_ex_g)
                          ) %>%
                          arrange( Country, Year )
                       
                       ### plot
                       output$ExGMarketLine <- renderHighchart({
                          highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_add_series( data =  tmp_data_country_ex_g ,
                                            mapping = hcaes(  x = Year, y = Value, group = Country),
                                            type = 'line',
                                            marker = list(symbol = 'circle') ,
                                            visible = c( rep(T,5), rep(F,length(tmp_top_country_ex_g)-5) )
                             ) %>%
                             hc_xAxis( categories = c( unique( tmp_data_country_ex_g$Year) ) ) %>%
                             hc_yAxis( title = list(text = "$ million, NZD" ), #"Exports markets over $500mn for goods"
                                       labels = list( format = "${value:,.0f} m")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: ${point.y} m"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       })
                       
                       # 7.6.1 Key exports market for goods trend line Percent ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_tot_ex_g <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          filter( Type_gs == 'Goods', Type_ie == 'Exports', Country == 'World' ) 
                       
                       tmp_data_country_ex_g_pc <-
                          tmp_data_country_ex_g %>%
                          bind_rows( tmp_data_tot_ex_g ) %>%
                          group_by( Year, Type_ie, Type_gs, Commodity, Note ) %>%
                          mutate( Share = Value/(Value[Country=='World']/10^6) ) %>%
                          ungroup %>%
                          mutate( Value = Share*100 ) %>%
                          filter( Country != 'World' ) %>%
                          mutate( Country = factor(Country, levels = tmp_top_country_ex_g) ) %>%
                          arrange( Country, Year )
                       
                       output$ExGMarketLinePercent <-
                          renderHighchart({
                             highchart() %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_add_series( data =  tmp_data_country_ex_g_pc ,
                                               mapping = hcaes(  x = Year, y = Value, group = Country),
                                               type = 'line',
                                               marker = list(symbol = 'circle'),
                                               visible = c( rep(T,5), rep(F,length(tmp_top_country_ex_g)-5) )
                                ) %>%
                                hc_xAxis( categories = c( unique( tmp_data_country_ex_g_pc$Year) )   ) %>%
                                hc_yAxis( title = list(text = "Percentage (%)"),
                                          labels = list( format = "{value:,.1f} %")  ) %>%
                                hc_plotOptions(line = list(
                                   dataLabels = list(enabled = F),
                                   #stacking = "normal",
                                   enableMouseTracking = T)
                                )%>%
                                hc_tooltip(table = TRUE,
                                           sort = TRUE,
                                           pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                 " {series.name}: {point.y:,.1f} %"),
                                           headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                ) %>%
                                hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          })
                       
                       # 7.7 Key exports market for services trend line ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_country_ex_s <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          filter( Type_gs == 'Services', Type_ie == 'Exports' ) 
                       
                       ## export markets over $500 million
                       tmp_top_country_ex_s <-
                          tmp_data_country_ex_s %>%
                          filter( Year == max(Year),
                                  Value >= (10^9)/2, 
                                  !Country %in% c("World", 
                                                  "Destination Unknown - EU")
                          ) %>% ## 1 bn commodity
                          arrange( -Value ) %>%
                          dplyr::select( Country ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_data_country_ex_s  %<>%
                          filter( Country %in% tmp_top_country_ex_s ) %>%
                          mutate( Value = round(Value/10^6),
                                  Country = factor(Country, levels = tmp_top_country_ex_s)
                          ) %>%
                          arrange( Country, Year )
                       
                       ### plot
                       output$ExSMarketLine <- 
                          renderHighchart({
                             highchart() %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_add_series( data =  tmp_data_country_ex_s ,
                                               mapping = hcaes(  x = Year, y = Value, group = Country),
                                               type = 'line',
                                               marker = list(symbol = 'circle') ,
                                               visible = c( rep(T,5), rep(F,length(tmp_top_country_ex_s)-5) )
                                ) %>%
                                hc_xAxis( categories = c( unique( tmp_data_country_ex_s$Year) ) ) %>%
                                hc_yAxis( title = list(text = "$ million, NZD"), #"Exports markets over $500mn for services"
                                          labels = list( format = "${value:,.0f} m")  ) %>%
                                hc_plotOptions(line = list(
                                   dataLabels = list(enabled = F),
                                   #stacking = "normal",
                                   enableMouseTracking = T)
                                )%>%
                                hc_tooltip(table = TRUE,
                                           sort = TRUE,
                                           pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                 " {series.name}: ${point.y} m"),
                                           headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                ) %>%
                                hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          })
                       
                       # 7.7.1 Key exports market for services trend line Percent ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_tot_ex_s <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          filter( Type_gs == 'Services', Type_ie == 'Exports', Country == 'World' ) 
                       
                       tmp_data_country_ex_s_pc <-
                          tmp_data_country_ex_s %>%
                          bind_rows( tmp_data_tot_ex_s ) %>%
                          group_by( Year, Type_ie, Type_gs, Commodity, Note ) %>%
                          mutate( Share = Value/(Value[Country=='World']/10^6) ) %>%
                          ungroup %>%
                          mutate( Value = Share*100 ) %>%
                          filter( Country != 'World' ) %>%
                          mutate( Country = factor(Country, levels = tmp_top_country_ex_s) ) %>%
                          arrange( Country, Year )
                       
                       output$ExSMarketLinePercent <-
                          renderHighchart({
                             highchart() %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_add_series( data =  tmp_data_country_ex_s_pc ,
                                               mapping = hcaes(  x = Year, y = Value, group = Country),
                                               type = 'line',
                                               marker = list(symbol = 'circle'),
                                               visible = c( rep(T,5), rep(F,length(tmp_top_country_ex_s)-5) )
                                ) %>%
                                hc_xAxis( categories = c( unique( tmp_data_country_ex_s_pc$Year) )   ) %>%
                                hc_yAxis( title = list(text = "Percentage (%)"),
                                          labels = list( format = "{value:,.1f} %")  ) %>%
                                hc_plotOptions(line = list(
                                   dataLabels = list(enabled = F),
                                   #stacking = "normal",
                                   enableMouseTracking = T)
                                )%>%
                                hc_tooltip(table = TRUE,
                                           sort = TRUE,
                                           pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                 " {series.name}: {point.y:,.1f} %"),
                                           headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                ) %>%
                                hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          })
                       
                       # 7.8 Key imports market for goods trend line ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_country_im_g <-
                          dtf_shiny_country_gs %>%
                          filter( Type_gs == 'Goods', Type_ie == 'Imports', Year >=2007 ) 
                       
                       ## export markets over $500 million
                       tmp_top_country_im_g <-
                          tmp_data_country_im_g %>%
                          filter( Year == max(Year),
                                  Value >= (10^9)/2, 
                                  !Country %in% c("World", 
                                                  "Destination Unknown - EU")
                          ) %>% ## 1 bn commodity
                          arrange( -Value ) %>%
                          dplyr::select( Country ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_data_country_im_g  %<>%
                          filter( Country %in% tmp_top_country_im_g ) %>%
                          mutate( Value = round(Value/10^6),
                                  Country = factor(Country, levels = tmp_top_country_im_g)
                          ) %>%
                          arrange( Country, Year )
                       
                       ### plot
                       output$ImGMarketLine <- renderHighchart({
                          highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_add_series( data =  tmp_data_country_im_g ,
                                            mapping = hcaes(  x = Year, y = Value, group = Country),
                                            type = 'line',
                                            marker = list(symbol = 'circle') ,
                                            visible = c( rep(T,5), rep(F,length(tmp_top_country_im_g)-5) )
                             ) %>%
                             hc_xAxis( categories = c( unique( tmp_data_country_im_g$Year) ) ) %>%
                             hc_yAxis( title = list(text = "$ million, NZD"), #"Imports markets over $500mn for goods"
                                       labels = list( format = "${value:,.0f} m")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: ${point.y} m"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                       })
                       
                       # 7.8.1 Key imports market for goods trend line Percent ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_tot_im_g <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          filter( Type_gs == 'Goods', Type_ie == 'Imports', Country == 'World' ) 
                       
                       tmp_data_country_im_g_pc <-
                          tmp_data_country_im_g %>%
                          bind_rows( tmp_data_tot_im_g ) %>%
                          group_by( Year, Type_ie, Type_gs, Commodity, Note ) %>%
                          mutate( Share = Value/(Value[Country=='World']/10^6) ) %>%
                          ungroup %>%
                          mutate( Value = Share*100 ) %>%
                          filter( Country != 'World' ) %>%
                          mutate( Country = factor(Country, levels = tmp_top_country_im_g) ) %>%
                          arrange( Country, Year )
                       
                       output$ImGMarketLinePercent <-
                          renderHighchart({
                             highchart() %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_add_series( data =  tmp_data_country_im_g_pc ,
                                               mapping = hcaes(  x = Year, y = Value, group = Country),
                                               type = 'line',
                                               marker = list(symbol = 'circle'),
                                               visible = c( rep(T,5), rep(F,length(tmp_top_country_im_g)-5) )
                                ) %>%
                                hc_xAxis( categories = c( unique( tmp_data_country_im_g_pc$Year) )   ) %>%
                                hc_yAxis( title = list(text = "Percentage (%)"),
                                          labels = list( format = "{value:,.1f} %")  ) %>%
                                hc_plotOptions(line = list(
                                   dataLabels = list(enabled = F),
                                   #stacking = "normal",
                                   enableMouseTracking = T)
                                )%>%
                                hc_tooltip(table = TRUE,
                                           sort = TRUE,
                                           pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                 " {series.name}: {point.y:,.1f} %"),
                                           headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                ) %>%
                                hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          })
                       
                       # 7.9 Key imports market for services trend line ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_country_im_s <-
                          dtf_shiny_country_gs %>%
                          filter( Type_gs == 'Services', Type_ie == 'Imports', Year >=2007 ) 
                       
                       ## export markets over $500 million
                       tmp_top_country_im_s <-
                          tmp_data_country_im_s %>%
                          filter( Year == max(Year),
                                  Value >= (10^9)/2, 
                                  !Country %in% c("World", 
                                                  "Destination Unknown - EU")
                          ) %>% ## 1 bn commodity
                          arrange( -Value ) %>%
                          dplyr::select( Country ) %>%
                          as.matrix() %>%
                          as.character
                       
                       tmp_data_country_im_s  %<>%
                          filter( Country %in% tmp_top_country_im_s ) %>%
                          mutate( Value = round(Value/10^6),
                                  Country = factor(Country, levels = tmp_top_country_im_s)
                          ) %>%
                          arrange( Country, Year )
                       
                       ### plot
                       output$ImSMarketLine <- 
                          renderHighchart({
                             highchart() %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_add_series( data =  tmp_data_country_im_s ,
                                               mapping = hcaes(  x = Year, y = Value, group = Country),
                                               type = 'line',
                                               marker = list(symbol = 'circle') ,
                                               visible = c( rep(T,5), rep(F,length(tmp_top_country_im_s)-5) )
                                ) %>%
                                hc_xAxis( categories = c( unique( tmp_data_country_im_s$Year) ) ) %>%
                                hc_yAxis( title = list(text = "$ million, NZD"), #"Imports markets over $500mn for services"
                                          labels = list( format = "${value:,.0f} m")  ) %>%
                                hc_plotOptions(line = list(
                                   dataLabels = list(enabled = F),
                                   #stacking = "normal",
                                   enableMouseTracking = T)
                                )%>%
                                hc_tooltip(table = TRUE,
                                           sort = TRUE,
                                           pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                 " {series.name}: ${point.y} m"),
                                           headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                ) %>%
                                hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          })
                       
                       # 7.9.1 Key imports market for services trend line Percent ------------------------------------
                       withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
                          # Increment the progress bar, and update the detail text.
                          incProgress( i_prog/tot_step, detail = NULL)
                          ##Sys.sleep(0.1)
                          
                       })
                       i_prog <- i_prog + 1
                       
                       tmp_data_tot_im_s <-
                          dtf_shiny_country_gs %>%
                          filter( Year>=2007 ) %>%
                          filter( Type_gs == 'Services', Type_ie == 'Imports', Country == 'World' ) 
                       
                       tmp_data_country_im_s_pc <-
                          tmp_data_country_im_s %>%
                          bind_rows( tmp_data_tot_im_s ) %>%
                          group_by( Year, Type_ie, Type_gs, Commodity, Note ) %>%
                          mutate( Share = Value/(Value[Country=='World']/10^6) ) %>%
                          ungroup %>%
                          mutate( Value = Share*100 ) %>%
                          filter( Country != 'World' ) %>%
                          mutate( Country = factor(Country, levels = tmp_top_country_im_s) ) %>%
                          arrange( Country, Year )
                       
                       output$ImSMarketLinePercent <-
                          renderHighchart({
                             highchart() %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_add_series( data =  tmp_data_country_im_s_pc ,
                                               mapping = hcaes(  x = Year, y = Value, group = Country),
                                               type = 'line',
                                               marker = list(symbol = 'circle'),
                                               visible = c( rep(T,5), rep(F,length(tmp_top_country_im_s)-5) )
                                ) %>%
                                hc_xAxis( categories = c( unique( tmp_data_country_im_s_pc$Year) )   ) %>%
                                hc_yAxis( title = list(text = "Percentage (%)"),
                                          labels = list( format = "{value:,.1f} %")  ) %>%
                                hc_plotOptions(line = list(
                                   dataLabels = list(enabled = F),
                                   #stacking = "normal",
                                   enableMouseTracking = T)
                                )%>%
                                hc_tooltip(table = TRUE,
                                           sort = TRUE,
                                           pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                 " {series.name}: {point.y:,.1f} %"),
                                           headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                ) %>%
                                hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          })
                       
                       ## insert all UIs ----------
                       insertUI(
                          selector = '#show_more_detail',
                          ui = div( id = 'conents_for_more_detail',
                                    ## 1.4 Treemap on Commodities and services --------------------
                                    h2(paste0('Key commodities and services')),
                                    tags$a(href = 'http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports.aspx', "Key commodities and services are defined by Stats NZ", target = "_blank"),
                                    fluidRow( highchartOutput('KeyExTM')  ) %>% withSpinner(type=4) ,
                                    fluidRow( highchartOutput('KeyImTM')  ),
                                    
                                    
                                    ## 1.5 Line chart key commodities and services exports that are over $1bn the most recent years -------------
                                    h2(paste0('Trends of key commodities and services')),
                                    p("Click on the commodity or service names in the legend area to show their trends"),
                                    fluidRow( h3("key commodities and services EXPORTS over $1bn", align = 'center'),
                                              column( width = 6, h4("Export values"), highchartOutput('KeyExLine')  ),
                                              column( width = 6, h4("As a percentage of total exports"), highchartOutput('KeyExLinePercent')  ) ),
                                    fluidRow( h3("key commodities and services IMPORTS over $1bn", align = 'center'),
                                              column( width = 6, h4("Import values"), highchartOutput('KeyImLine') ),
                                              column( width = 6, h4("As a percentage of total imports"), highchartOutput('KeyImLinePercent')  ) ),
                                    
                                    ## 1.6 World map on total exports by country -----------------
                                    h2(paste0('Global trading partners at a glance')),
                                    HTML( "<p> The map shows New Zealand's trading partners. The size of bubble area represents the magnitude of two way trade.
                                          <span style='color:green'> Green </span> and <span style='color:red'> red </span> color indicate whether is trade
                                          <span style='color:green'> surplus </span> or <span style='color:red'> deficit. </span> </p>" ),
                                    fluidRow( uiOutput('TradeMap') ),
                                    
                                    ## 1.6.1 FTA time line  -----------------
                                    h2(paste0('Free trade agreements in force')),
                                    tags$p( "The timeline below shows when the FTAs negotiation started and then put into force. 
                                            Click on each FTA's name for more information.",
                                            tags$b( "In addition, you can select the 'FTA in force' market group under the Market Intelligence panel to get more insights." )
                                    ),
                                    tags$p("FTA stands for free trade agreement; CER stands for closer economic relations; CEP stands for closer economic partnership, and P4
                                           is short for the Trans-pacific Strategic Economic Partnership."),
                                    fluidRow( timevisOutput("FTATimeLine")   ),
                                    
                                    ## 1.7 Trend of key export markets --------------------
                                    h2(paste0('Trends of key trading partners')),
                                    p("Only top 5 markets are shown. Click on the country names in the legend area to show/hide their trends"),
                                    
                                    ## Two way trade and trade balance
                                    fluidRow( h3("Two-way trade and trade surplus/deficit", align = 'center'),
                                              column( width = 6, h4("key markets with two-way trade over $2b"), highchartOutput("TwowayMarketLine") ),
                                              column( width = 6, h4("key markets with trade surplus/deficit over $500m"), highchartOutput("BalanceMarketLine") )
                                    ),
                                    
                                    ## total Exports
                                    fluidRow( h3("Total exports, goods exports and services exports" ,align = 'center'),
                                              h4(tags$b("Total exports: key EXPORTS markets over $1b")),
                                              column( width = 6, h4("Export values"), highchartOutput("ExMarketLine") ),
                                              column( width = 6, h4("As a percentage of total exports"), highchartOutput("ExMarketLinePercent") )
                                              #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
                                    ),
                                    
                                    fluidRow( h4(tags$b("Goods exports: key EXPORTS markets for GOODS over $500m")),
                                              column( width = 6, h4("Export values"), highchartOutput("ExGMarketLine") ),
                                              column( width = 6, h4("As a percentage of goods exports"), highchartOutput("ExGMarketLinePercent") )
                                              #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
                                    ),
                                    
                                    fluidRow( h4(tags$b("Services exports: key EXPORTS markets for SERVICES over $500m")),
                                              column( width = 6, h4("Export values"), highchartOutput("ExSMarketLine") ),
                                              column( width = 6, h4("As a percentage of services exports"), highchartOutput("ExSMarketLinePercent") )
                                              #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
                                    ),
                                    
                                    ## total Imports
                                    fluidRow( h3("Total imports, goods imports and services imports" ,align = 'center'),
                                              h4(tags$b("Total imports: key IMPORTS markets over $1b")),
                                              column( width = 6, h4("Import values"), highchartOutput("ImMarketLine") ),
                                              column( width = 6, h4("As a percentage of total imports"), highchartOutput("ImMarketLinePercent") )
                                              #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
                                    ),
                                    
                                    fluidRow( h4(tags$b("Goods imports: key IMPORTS markets for GOODS over $500m")),
                                              column( width = 6, h4("Import values"), highchartOutput("ImGMarketLine") ),
                                              column( width = 6, h4("As a percentage of goods imports"), highchartOutput("ImGMarketLinePercent") )
                                              #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
                                    ),
                                    
                                    fluidRow(h4(tags$b("Services imports: key IMPORTS markets for SERVICES over $500m")),
                                             column( width = 6, h4("Import values"), highchartOutput("ImSMarketLine") ),
                                             column( width = 6, h4("As a percentage of services imports"), highchartOutput("ImSMarketLinePercent") )
                                             #column( width = 6, h4("key IMPORTS markets over $1b"), highchartOutput("ImMarketLine") )
                                    )
                                    )
                       )
                       
                       ## hide load more message ---
                       ## --- show loading message ---
                       shinyjs::hide( id = "load_more_message" )
                    })
      
      
      ## II. Commodity intelligence ----------------------
      ### 1.2 Exports ------ when press the Build Report button ------------------
      observeEvent( input$btn_build_commodity_report_ex,
                    {

                       ## 1.1 check the inputs are correct ---------------
                       tmp_execution_pre_define <- tmp_execution_self_define <- FALSE
                       
                       ## 1.2 work on Pre-deinfed Warning if no pre-defined commodity is selected ------------------------
                       if(input$rbtn_prebuilt_diy_ex=='Pre-defined' & is.null(input$select_comodity_ex)) {
                          showModal(modalDialog(
                             title = "Warning",
                             tags$b("Please select one or multiple pre-defined commodities!"),
                             size = 's'
                          ))
                       }
                       
                       ### if test pass
                       if(input$rbtn_prebuilt_diy_ex=='Pre-defined' & !is.null(input$select_comodity_ex)) {
                          tmp_execution_pre_define <- TRUE
                       }
                       
                       ## 1.2.1 Build graphs pre-defined commodity --------------
                       if(tmp_execution_pre_define){
                          ## --- hide howto -----
                          shinyjs::hide(id = 'ci_howto_ex')
                          ## show waite message ----
                          shinyjs::show( id = 'wait_message_ci_ex' )
                          ## disable the buttone ---
                          shinyjs::disable("btn_build_commodity_report_ex")
                          ## disable the selection  ---
                          shinyjs::disable("select_comodity_ex")
                          shinyjs::disable("rbtn_prebuilt_diy_ex")
                          
                          
                          ### work on Data noW!!!!!!!
                          tmp_selected_service <- setdiff( input$select_comodity_ex , list_snz_commodity_ex[['Goods']] )

                          snz_hs <- concord_snz_eg$HS_codes[concord_snz_eg$SNZ_commodity %in% input$select_comodity_ex ]
                          
                          if( length(tmp_selected_service) >=1 ){
                             hs_group <-
                                concord_snz_eg %>%
                                filter( HS_codes %in% snz_hs ) %>%
                                bind_rows( data.frame(HS_codes = tmp_selected_service,
                                                      SNZ_commodity = tmp_selected_service) )
                          }else{
                             hs_group <-
                                concord_snz_eg %>%
                                filter( HS_codes %in% snz_hs )
                          }
                          
                          colnames(hs_group) <- c("HS_code", "HS_group")

                          ## 2.1 Build export value line chart -------------------- 
                          tmp_top_g_ex <-
                             dtf_shiny_commodity_service_ex %>%
                             filter( SNZ_commodity %in% input$select_comodity_ex ,
                                     !SNZ_commodity %in% tmp_selected_service,
                                     SNZ_commodity != 'Confidential data') %>%
                             filter( Year == max(Year)) %>% 
                             arrange( -Value ) %>%
                             dplyr::select( SNZ_commodity ) %>%
                             as.matrix() %>%
                             as.character
                          
                          tmp_top_s_ex <-
                             dtf_shiny_commodity_service_ex %>%
                             filter( SNZ_commodity %in% tmp_selected_service ) %>%
                             filter( Year == max(Year) ) %>%
                             arrange( -Value ) %>%
                             dplyr::select( SNZ_commodity ) %>%
                             as.matrix() %>%
                             as.character
                          
                          ## top selected commodities and top 5services
                          tmp_top_ex <- c( tmp_top_g_ex, tmp_top_s_ex)
                          
                          ## data frame to plot
                          tmp_dtf_key_line_ex <- 
                             dtf_shiny_commodity_service_ex %>%
                             filter( SNZ_commodity %in% tmp_top_ex,
                                     Year >=2007) %>%
                             mutate( Value = round(Value/10^6),
                                     SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_ex)
                             ) %>%
                             arrange( SNZ_commodity )
                          
                          ### plot
                          tmp_export_hc <- 
                             highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_xAxis( categories = c( unique( tmp_dtf_key_line_ex$Year) ) ) %>%
                             hc_yAxis( title = list(text = "$ million, NZD"),
                                       labels = list( format = "${value:,.0f} m")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T #,
                                #series = list(events = list(legendItemClick = sharelegend)) ,
                                #showInLegend = T
                                )
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: ${point.y} m"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          
                          ### if any services are selected?
                          if( length(tmp_top_g_ex)>=1&length(tmp_top_s_ex)==0 ) {
                             output$CIExportValueLine <- 
                                renderHighchart({
                                   tmp_export_hc %>%
                                      hc_add_series( data =  tmp_dtf_key_line_ex %>% filter( Type_gs == 'Goods' ) ,
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line',
                                                     marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                      )
                                })
                          }
                          if( length(tmp_top_g_ex)==0 & length(tmp_top_s_ex)>=1 ){
                             output$CIExportValueLine <- 
                                renderHighchart(
                                   tmp_export_hc %>%
                                      hc_add_series( data =  tmp_dtf_key_line_ex %>% filter( Type_gs == 'Services' ),
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_s_ex)-1))
                                      )
                                )
                          }
                          if( length(tmp_top_g_ex)>=1 & length(tmp_top_s_ex)>=1 ){
                             output$CIExportValueLine <- 
                                renderHighchart(
                                   tmp_export_hc %>%
                                      hc_add_series( data =  tmp_dtf_key_line_ex %>% filter( Type_gs == 'Goods' ) ,
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line',
                                                     marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                      ) %>%
                                      hc_add_series( data =  tmp_dtf_key_line_ex %>% filter( Type_gs == 'Services' ),
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_s_ex)-1))
                                      )
                                )
                          }
                          
                          ## 2.2 build export as a percent of total export line chart -----------------------
                          tmp_tot_ex <-
                             dtf_shiny_full %>%
                             filter( Country == 'World',
                                     Type_ie == 'Exports',
                                     Year >= 2007 )  %>%
                             mutate( Value = round(Value/10^6) ) %>%
                             group_by( Year, Country, Type_ie ) %>%
                             summarize( Value = sum(Value, na.rm=T) ) %>%
                             ungroup %>%
                             mutate( SNZ_commodity = 'Total exports' )
                          
                          tmp_dtf_percent_line_ex <-
                             tmp_dtf_key_line_ex %>%
                             bind_rows( tmp_tot_ex ) %>%
                             group_by( Year, Country, Type_ie ) %>%
                             mutate( Share = Value/Value[SNZ_commodity=='Total exports'],
                                     Value = Share*100 ) %>%
                             ungroup %>%
                             filter( SNZ_commodity != 'Total exports' ) %>%
                             mutate( SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_ex) ) %>%
                             arrange( SNZ_commodity )
                          
                          ### plot
                          tmp_export_percent_hc <- 
                             highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_xAxis( categories = c( unique( tmp_dtf_percent_line_ex$Year) ) ) %>%
                             hc_yAxis( title = list(text = "Percentage (%)"),
                                       labels = list( format = "{value:,.1f} %")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: {point.y:,.1f} %"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                             #hc_legend( enabled = FALSE )
                          
                          ### if any services are selected?
                          if( length(tmp_top_g_ex)>=1&length(tmp_top_s_ex)==0 ) {
                             output$CIExportPercentLine <- 
                                renderHighchart(
                                   tmp_export_percent_hc %>%
                                      hc_add_series( data =  tmp_dtf_percent_line_ex %>% filter( Type_gs == 'Goods' ) ,
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line',
                                                     marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                      )
                                )
                          }
                          if( length(tmp_top_g_ex)==0 & length(tmp_top_s_ex)>=1 ){
                             output$CIExportPercentLine <- 
                                renderHighchart(
                                   tmp_export_percent_hc %>%
                                      hc_add_series( data =  tmp_dtf_percent_line_ex %>% filter( Type_gs == 'Services' ),
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_s_ex)-1))
                                      )
                                )
                          }
                          if( length(tmp_top_g_ex)>=1 & length(tmp_top_s_ex)>=1 ){
                             output$CIExportPercentLine <- 
                                renderHighchart(
                                   tmp_export_percent_hc %>%
                                      hc_add_series( data =  tmp_dtf_percent_line_ex %>% filter( Type_gs == 'Goods' ) ,
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line',
                                                     marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                      ) %>%
                                      hc_add_series( data =  tmp_dtf_percent_line_ex %>% filter( Type_gs == 'Services' ),
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_s_ex)-1))
                                      )
                                )
                          }
                          
                          ## !!!!! try UI insert ----------- 
                          insertUI(
                             selector = '#body_ex',
                             ui =   div( id = 'body_ex_line_value_percent',
                                         fluidRow( h1("Exports for selected commodities/services"),
                                                   p("Click on the commodity or service names in the legend area to show their trends"),
                                                   column(6, div(id = "body_value_ex", h4("Export values"), highchartOutput('CIExportValueLine') ) ),
                                                   column(6, div(id = "body_percent_ex", h4("As a percent of total exports"), highchartOutput('CIExportPercentLine') ) ))
                                       )
                          )
                          ## end Try UI insert --------##

                          ## 2.3 build export value change table ----------------
                          ## data frame to plot
                          tmp_dtf_key_tab_ex <- 
                             dtf_shiny_commodity_service_ex %>%
                             filter( SNZ_commodity %in% tmp_top_ex) %>%
                             mutate( SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_ex) ) %>%
                             arrange( SNZ_commodity )
                          
                          tmp_tab <-
                             tmp_dtf_key_tab_ex %>%
                             mutate( Name =  SNZ_commodity ) %>%
                             group_by( Name) %>%
                             mutate( CAGR1 = CAGR( Value[Year == max(Year)]/
                                                      Value[Year == (max(Year)-1)], 1)/100,
                                     CAGR5 = CAGR( Value[Year == max(Year)]/
                                                      Value[Year == (max(Year)-5)], 5)/100,
                                     CAGR10 = CAGR( Value[Year == max(Year)]/
                                                       Value[Year == (max(Year)-10)], 10)/100,
                                     ABS5 = Value[Year == max(Year)] - Value[Year == (max(Year)-5)],
                                     ABS10 = Value[Year == max(Year)] - Value[Year == (max(Year)-10)]
                             ) %>%
                             ungroup %>%
                             filter( Year == max(Year) ) %>%
                             left_join(tmp_dtf_percent_line_ex %>% dplyr::select(-CAGR5, -Value) ) %>%
                             dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10) %>%
                             mutate( Name = factor(Name, levels = tmp_top_ex),
                                     Value = Value/10^6,
                                     ABS5 = ABS5/10^6,
                                     ABS10 = ABS10/10^6) %>%
                             mutate( Name = factor(Name, levels = tmp_top_ex) ) %>%
                             arrange( Name )
                          
                          ### join back to hs code
                          hs_group_flat <- 
                             hs_group %>%
                             group_by( HS_group ) %>%
                             summarise( HS_code = paste0(HS_code, collapse = '; ') ) %>%
                             ungroup
                             
                          tmp_tab %<>%
                             left_join( hs_group_flat, by = c("Name"= 'HS_group') ) %>%
                             dplyr::select( HS_code, Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10 )
                          

                          ## build table
                          output$GrowthTabSelectedEx <- renderDataTable(
                             datatable( tmp_tab,
                                        rownames = F,
                                        filter = c("top"),
                                        extensions = c('Buttons'
                                                       #, 'FixedColumns'
                                                       ),
                                        options = list(dom = 'Bfltp',# 'Bt', 
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1, 
                                                       ,scrollX = TRUE
                                                       #,fixedColumns = list(leftColumns = 2) 
                                                       ,autoWidth = T
                                                       ,pageLength = 10
                                                       ,lengthMenu = list(c(10,  -1), list('10', 'All')) ,
                                                       searchHighlight = TRUE,
                                                       search = list(regex = TRUE, caseInsensitive = FALSE )
                                                       ) ,
                                        colnames = c("HS codes", "Classification","Value ($m)", "Share of total exports", 'CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10')
                             ) %>%
                              formatStyle(
                                    c('CAGR1', 'CAGR5', 'CAGR10'),
                                    background = styleColorBar( c(0, max(c(tmp_tab$CAGR1,tmp_tab$CAGR5, tmp_tab$CAGR10))*2, na.rm=T) , 'lightblue'),
                                    backgroundSize = '100% 90%',
                                    backgroundRepeat = 'no-repeat',
                                    backgroundPosition = 'center'
                                 ) %>%
                                 formatStyle(c('CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'),
                                             color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>%
                                 formatPercentage( c('Share','CAGR1', 'CAGR5', 'CAGR10'),digit = 1 ) %>%
                                 formatStyle( columns = c('Name','Value', 'Share', 'CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'), `font-size`= '115%' ) %>%
                                 formatCurrency( columns = c('Value', 'ABS5', 'ABS10'), mark = ' ', digits = 1)
                          )
                          
                          ## !!!!! try UI insert ----------- 
                          insertUI(
                             selector = '#body_growth_ex',
                             ui =   div( id = 'body_ex_growth_tab',
                                         fluidRow( h1("Short, medium, and long term growth for selected commodities/services"),
                                                   p("Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years."),
                                                   dataTableOutput('GrowthTabSelectedEx')
                                         )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          ## 2.4 Build export by country output groups -------------------
                          ## create a selector for each selected commodity
                          output$CIEXSelectorByMarkets <- renderUI({
                             selectizeInput("select_comodity_ex_for_market_analysis",
                                            tags$p("Please select or search a commodity for its market analysis"), 
                                            choices =  c('Please select a commodity' = "" , 
                                                         tmp_tab$Name[input$GrowthTabSelectedEx_rows_all]
                                                         ), #input$select_comodity_ex,
                                            selected = NULL,  width = "500px",
                                            multiple = F #,
                                            # options = list(
                                            #    placeholder = 'Please select a commodity',
                                            #    onInitialize = I('function() { this.setValue(" "); }')
                                            #             ) 
                                            )
                          })
                          
                          ### build data for market analysis -- these has to be reactive values
                          ## The name of the selected commodity
                          tmp_selected_ex <- 
                             reactive({
                                input$select_comodity_ex_for_market_analysis
                                })
                          
                          ## The HS codes of the selected commodity
                          tmp_hs_ex <- 
                             reactive({
                                hs_group$HS_code[hs_group$HS_group == tmp_selected_ex()]
                                })
                          
                          ## The data from of the selected commodity by markets
                          tmp_dtf_market_ex <- 
                             reactive({
                                dtf_shiny_full %>%
                                   filter( Commodity %in% tmp_hs_ex(), 
                                           Year >= 2007,
                                           Type_ie == 'Exports') %>%
                                   left_join( concord_country_iso_latlon_raw, by = 'Country' ) %>%
                                   group_by( Year, Country, Type_ie, Type_gs, Note, ISO2, lat, lon ) %>%
                                   summarize( Value = sum(Value, na.rm=T) ) %>%
                                   ungroup %>%
                                   mutate( Commodity = as.character( tmp_selected_ex() ) )
                             })
                          
                          ### selcted commodity and service outputs
                          output$SelectedEx <- 
                             renderText({
                                tmp_selected_ex()
                             })
                          
                          ## !!!!! try UI insert ----------- 
                          insertUI(
                             selector = '#body_ci_markets_ex',
                             ui =   div( id = 'body_ci_markets_ex_selector',
                                         fluidRow(h1("Export markets analysis for selected commodity/service"),
                                                  uiOutput("CIEXSelectorByMarkets") ),
                                         fluidRow( shiny::span(h1( HTML(paste0(textOutput("SelectedEx"))), align = "center" ), style = "color:darkblue" ) )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          ## --- show loading message ------------------
                          observe({
                             if( any(input$select_comodity_ex_for_market_analysis %in% tmp_tab$Name)   ){
                                shinyjs::show( id = "body_ci_market_loading_message" )
                             }
                          })
                          ## finish
                          
                          ### 2.4.1 build highchart map  ---------------------------
                          print("--------- Building highchart map -------------")
                          
                          tmp_dtf_market_ex_map <- 
                             reactive({
                                tmp_dtf_market_ex() %>%
                                   filter( Year == max(Year),
                                           !is.na(lat) ) %>%
                                   mutate( Value = Value/10^6,
                                           z= Value,
                                           name = Country)
                             })
                          
                          ## plot map
                          output$MapEXMarket <- 
                             renderHighchart({
                                if( input$select_comodity_ex_for_market_analysis == "" ) 
                                   return(NULL)
                                
                                hcmap( data = tmp_dtf_market_ex_map() ,
                                       value = 'Value',
                                       joinBy = c('iso-a2','ISO2'), 
                                       name="Exports value",
                                       borderWidth = 1,
                                       borderColor = "#fafafa",
                                       nullColor = "lightgrey",
                                       tooltip = list( table = TRUE,
                                                       sort = TRUE,
                                                       headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                       pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
                                ) %>%
                                hc_add_series(data =  tmp_dtf_market_ex_map(),
                                                 type = "mapbubble",
                                                 color  = hex_to_rgba("#f1c40f", 0.9),
                                                 minSize = 0,
                                                 name="Exports value",
                                                 maxSize = 30,
                                                 tooltip = list(table = TRUE,
                                                                 sort = TRUE,
                                                                 headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                                 pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
                                   ) %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_legend( enabled=FALSE ) %>% 
                                hc_mapNavigation(enabled = TRUE) 
                             })
                          
                          ## !!!!! try UI insert ----------- 
                          output$H2_map_of_export_title <-
                             renderText({
                                if( input$select_comodity_ex_for_market_analysis == "" ) 
                                   return(NULL)
                                paste0("Map of export values")
                             })
                          
                          output$H2_map_of_export_title_note <-
                             renderText({
                                if( input$select_comodity_ex_for_market_analysis == "" ) 
                                   return(NULL)
                                paste0("The size of bubble area and color both represent the value of exports.")
                             })
                           
                          ## inserter UI here  
                          insertUI(
                             selector = '#body_ci_markets_ex',
                             ui =   div( id = 'body_ci_markets_ex_map',
                                         fluidRow(h2( HTML(paste0(textOutput("H2_map_of_export_title"))) ) ,
                                                  p( HTML(paste0(textOutput("H2_map_of_export_title_note")))  ),
                                                  highchartOutput('MapEXMarket') )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          ### 2.4.2 Top markets for selected commodity line chart ----------------
                          print("--------- Building Top market line chart -------------")
                          tmp_top_country_selected_ex <- 
                             reactive({
                                tmp_dtf_market_ex() %>%
                                   filter( Year == max(Year),
                                           Value > 0 , 
                                           !Country %in% c("World", 
                                                           "Destination Unknown - EU")
                                   ) %>% ## 1 bn commodity
                                   arrange( -Value ) %>%
                                   dplyr::select( Country ) %>%
                                   as.matrix() %>%
                                   as.character
                             })

                          tmp_top10_country_selected_ex <-
                             reactive({
                                tmp_top_country_selected_ex()[1:min(10,length(tmp_top_country_selected_ex()))]
                             })
                         
                          
                          ### derive datafrom for the line plot
                          tmp_dtf_market_ex_line <- 
                             reactive({
                                tmp_dtf_market_ex() %>%
                                   filter( Country %in%  as.character(tmp_top_country_selected_ex()) ) %>%
                                   mutate( Value = Value/10^6 ,
                                           Country = factor(Country, levels = as.character(tmp_top_country_selected_ex()) )
                                          ) %>%
                                   arrange(Country)
                             })
                          

                          ## line plot
                          output$SelectedExMarketLine <- renderHighchart({
                             if( input$select_comodity_ex_for_market_analysis == "" ) 
                                return(NULL)
                             
                             highchart() %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_add_series( data =  tmp_dtf_market_ex_line() %>%
                                                  filter( Country %in% as.character(tmp_top10_country_selected_ex()) ),
                                               mapping = hcaes(  x = Year, y = Value, group = Country),
                                               type = 'line',
                                               marker = list(symbol = 'circle'), 
                                               visible = c( rep(T,5), rep(F,length( as.character(tmp_top10_country_selected_ex()) )-5) )
                                ) %>%
                                hc_xAxis( categories = c( unique( tmp_dtf_market_ex_line()$Year) ) ) %>%
                                hc_yAxis( title = list(text = "$ million, NZD"),
                                          labels = list( format = "${value:,.0f} m")  ) %>%
                                hc_plotOptions(line = list(
                                   dataLabels = list(enabled = F),
                                   #stacking = "normal",
                                   enableMouseTracking = T)
                                )%>%
                                hc_tooltip(table = TRUE,
                                           sort = TRUE,
                                           pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                 " {series.name}: ${point.y:,.0f} m"),
                                           headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                ) %>%
                                hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          })

                          ### 2.4.3 Top markets for selected commodity percent line chart -------------------
                          print("--------- Building Top market line chart (Percent) -------------")
                          tmp_dtf_market_ex_line_percent <- 
                             reactive({
                                tmp_dtf_market_ex_line() %>%
                                   group_by(Year, Type_ie, Type_gs, Note, Commodity) %>%
                                   mutate( Share = Value/sum(Value, na.rm=T)) %>%
                                   ungroup %>%
                                   mutate( Value = Share*100 ) 
                             })
                          
                          output$SelectedExMarketLinePercent <-
                             renderHighchart({
                                if( input$select_comodity_ex_for_market_analysis == "" ) 
                                   return(NULL)
                                
                                highchart() %>%
                                   hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                   hc_add_series( data =  tmp_dtf_market_ex_line_percent() %>%
                                                     filter( Country %in% as.character(tmp_top10_country_selected_ex()) ),
                                                  mapping = hcaes(  x = Year, y = Value, group = Country),
                                                  type = 'line',
                                                  marker = list(symbol = 'circle'), 
                                                  visible = c( rep(T,5), rep(F,length( as.character(tmp_top10_country_selected_ex()) )-5) )
                                   ) %>%
                                   hc_xAxis( categories = c( unique( tmp_dtf_market_ex_line_percent()$Year) ) ) %>%
                                   hc_yAxis( title = list(text = "Percentage (%)"),
                                             labels = list( format = "{value:,.1f} %")  ) %>%
                                   hc_plotOptions(line = list(
                                      dataLabels = list(enabled = F),
                                      #stacking = "normal",
                                      enableMouseTracking = T)
                                   )%>%
                                   hc_tooltip(table = TRUE,
                                              sort = TRUE,
                                              pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                    " {series.name}: {point.y:,.1f} %"),
                                              headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                   ) %>%
                                   hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                             })
                          
                          ## !!!!! try UI insert ----------- 
                          output$H2_export_market_trend_title <-
                             renderText({
                                if( input$select_comodity_ex_for_market_analysis == "" )
                                   return(NULL)
                                paste0("Top 10 export markets trends")
                             })
                          
                          output$H2_export_market_trend_title_note <-
                             renderText({
                                if( input$select_comodity_ex_for_market_analysis == "" ) 
                                   return(NULL)
                                paste0("Click on the country names in the legend area to show their trends")
                             })
                          
                          output$H4_export_market_trend_value_title <-
                             renderText({
                                if( input$select_comodity_ex_for_market_analysis == "" ) 
                                   return(NULL)
                                paste0("Export values")
                             })
                          
                          output$H4_export_market_trend_percent_title <-
                             renderText({
                                if( input$select_comodity_ex_for_market_analysis == "" ) 
                                   return(NULL)
                                paste0("As a percent of total exports of the selected")
                             })
                          
                          insertUI(
                             selector = '#body_ci_markets_ex',
                             ui =   div( id = 'body_ci_markets_ex_top',
                                         fluidRow( h2( HTML(paste0(textOutput("H2_export_market_trend_title"))) ),
                                                   p( HTML(paste0(textOutput("H2_export_market_trend_title_note"))) ),
                                                   column(6, 
                                                          h4( HTML(paste0(textOutput("H4_export_market_trend_value_title"))) ),
                                                          highchartOutput("SelectedExMarketLine") 
                                                   ),
                                                   column(6,
                                                          h4( HTML(paste0(textOutput("H4_export_market_trend_percent_title"))) ),
                                                          highchartOutput("SelectedExMarketLinePercent")
                                                   )
                                         )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          ### 2.4.4 Growth prospective tab ----------------------
                          print("--------- Building Grwoth prospective table -------------")
                          tmp_tab_ex_growth <-
                             reactive({
                                tmp_dtf_market_ex_line() %>%
                                   #filter( Country %in% as.character(tmp_top10_country_selected_ex()) ) %>%
                                   mutate( Name =  Country ) %>%
                                   group_by( Name) %>%
                                   do( CAGR1 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                        .$Value[.$Year == (max(.$Year)-1)], 1)/100,
                                       CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                        .$Value[.$Year == (max(.$Year)-5)], 5)/100,
                                       CAGR10 =  CAGR( .$Value[.$Year == max(.$Year)]/
                                                          .$Value[.$Year == (max(.$Year)-10)], 10)/100,
                                       ABS5 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)- 5)],
                                       ABS10 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)- 10)]
                                   ) %>%
                                   ungroup %>%
                                   mutate( CAGR1 =  as.numeric(CAGR1),
                                           CAGR5 = as.numeric(CAGR5), 
                                           CAGR10 = as.numeric(CAGR10),
                                           ABS5 = as.numeric(ABS5), 
                                           ABS10 = as.numeric(ABS10) 
                                           ) %>%
                                   #filter( Year == max(Year) ) %>%
                                   left_join( tmp_dtf_market_ex_line() %>% rename(Name = Country) %>% filter( Year == max(Year) )  ) %>%
                                   left_join( tmp_dtf_market_ex_line_percent() %>% dplyr::select( -Value ) %>% rename( Name = Country) %>% filter( Year == max(Year) )  ) %>%
                                   dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10) %>%
                                   mutate( Name = factor(Name, levels = as.character(tmp_top_country_selected_ex()) ) ) %>%
                                   arrange( Name )
                             })
                          
                          output$SelectedExMarketGrowthTab <- renderDataTable({
                             if( input$select_comodity_ex_for_market_analysis == "" ) 
                                return(NULL)
                             
                             datatable( tmp_tab_ex_growth(),
                                        rownames = F,
                                        extensions = 'Buttons',
                                        options = list(dom = 'Bltp',# 'Bt', 
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1
                                                       ,scrollX = TRUE
                                                       ,pageLength = 10
                                                       ,lengthMenu = list(c(10,  -1), list('10', 'All'))
                                                       #,fixedColumns = list(leftColumns = 2) 
                                                       #,autoWidth = T
                                                       ) ,
                                        colnames=c("Markets", "Value ($m)", "Share of world marekt", 'CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10')
                             ) %>%
                                formatStyle(
                                   c('CAGR1', 'CAGR5', 'CAGR10'),
                                   background = styleColorBar( c(0, max(c(tmp_tab_ex_growth()$CAGR1,
                                                                          tmp_tab_ex_growth()$CAGR5,
                                                                          tmp_tab_ex_growth()$CAGR10))*2, na.rm=T) , 'lightblue'),
                                   backgroundSize = '100% 90%',
                                   backgroundRepeat = 'no-repeat',
                                   backgroundPosition = 'center'
                                ) %>%
                                formatStyle(c('CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'),
                                            color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>%
                                formatPercentage( c('Share', 'CAGR1', 'CAGR5', 'CAGR10'),digit = 1 ) %>%
                                formatStyle( columns = c('Name', "Value", "Share" ,'CAGR1', 'CAGR5', 'CAGR10'), `font-size`= '115%' ) %>%
                                formatCurrency( columns = c("Value", 'ABS5', 'ABS10'), mark = ' ', digits = 1)
                          })
                          
                          ## !!!!! try UI insert ----------- 
                          output$H2_market_ex_growth_tab_title <-
                             renderText({
                                if( input$select_comodity_ex_for_market_analysis == "" ) 
                                   return(NULL)
                                paste0("Top export markets growth prospective")
                             })
                          
                          output$H2_market_ex_growth_tab_title_note <-
                             renderText({
                                if( input$select_comodity_ex_for_market_analysis == "" ) 
                                   return(NULL)
                                paste0("Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years.")
                             })
                          
                          ## insert ui here
                          insertUI(
                             selector = '#body_ci_markets_ex',
                             ui =   div( id = 'body_ci_markets_ex_growth',
                                         fluidRow( h2( HTML(paste0(textOutput("H2_market_ex_growth_tab_title"))) ),
                                                   p( HTML(paste0(textOutput("H2_market_ex_growth_tab_title_note"))) ),
                                                   dataTableOutput("SelectedExMarketGrowthTab")
                                         )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          
                          ## 2.5 show HS groupings in appendix -------------------
                          # output$HS_pre_ex <- renderDataTable( hs_group,rownames = FALSE, 
                          #                                      extensions = 'Buttons',
                          #                                      options = list(dom = 'Bltp', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                          #                                                     pageLength = 5,
                          #                                                     lengthMenu = list(c(5,  -1), list('5', 'All')) 
                          #                                      ) )
                          ## !!!!! try UI insert ----------- 
                          # insertUI(
                          #    selector = '#body_ci_markets_ex',
                          #    ui =   div( id = 'body_appendix_hs_ex',
                          #                conditionalPanel("input.rbtn_prebuilt_diy_ex == 'Pre-defined'",
                          #                                 fluidRow( tags$h1("Appendix -- HS grouping selected"),
                          #                                           div(id = 'output_hs_pre_ex', dataTableOutput( ("HS_pre_ex") ) )
                          #                                 )
                          #                ),
                          #                
                          #                conditionalPanel( "input.rbtn_prebuilt_diy_ex == 'Self-defined'",
                          #                                  fluidRow( tags$h1("Appendix -- HS grouping uploaded"),
                          #                                            div(id = 'output_hs_ex', dataTableOutput( ("HS_ex") ) )
                          #                                  )
                          #                                  
                          #                )
                          #    )
                          # )
                          ## end Try UI insert --------##
                          
                          
                          
                          
                          
                          ## Tests ------------------------
                          # output$test_ex <- 
                          #    renderText({
                          #       tmp_hs_ex() 
                          #    })
                          # 
                          # output$test_country_ex <- 
                          #    renderDataTable(
                          #       datatable( tmp_dtf_market_ex() )
                          #    )
                          
                          
                          ## 2.6 Data for global situation from UN comtrade (ONLY for Export analysis) ----------------
                          print("--------- Building Reactive values for global analysis -------------")
                          rv_pre_define_ex <- reactiveValues()
                          
                          ## put reactive values into observe  ------
                          observe({
                             ## get data from un com trade using loop--------------
                             ## create a list first
                             print("----------- Download Uncomtrade trade by country --------------")
                             rv_pre_define_ex$Fail_uncomtrade_country <- 
                             try(
                                rv_pre_define_ex$tmp_global_by_country_raw_list <- 
                                   lapply( tmp_hs_ex() ,
                                           function(i){
                                                m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                                           commod_codes = i,
                                                           start_date = tmp_un_comtrade_max_year,
                                                           end_date = tmp_un_comtrade_max_year ) %>%
                                                 bind_rows(  m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                                                         commod_codes = i,
                                                                         start_date = tmp_un_comtrade_max_year - 5,
                                                                         end_date = tmp_un_comtrade_max_year - 5 ) 
                                                 )
                                           } 
                                   )
                             )
                             
                             ## try get EU data
                             print("----------- Download Uncomtrade trade by EU --------------")
                             rv_pre_define_ex$Fail_uncomtrade_eu <- 
                             try(
                                rv_pre_define_ex$tmp_global_by_eu_raw_list <- 
                                   lapply( tmp_hs_ex() ,
                                           function(i){
                                               m_ct_search( reporters = "EU-28", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                                           commod_codes = i,
                                                           start_date = tmp_un_comtrade_max_year,
                                                           end_date = tmp_un_comtrade_max_year )  %>%
                                                 bind_rows(  m_ct_search( reporters = "EU-28", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                                                         commod_codes = i,
                                                                         start_date = tmp_un_comtrade_max_year - 5,
                                                                         end_date = tmp_un_comtrade_max_year - 5 ) 
                                                 )
                                           } 
                                   )
                             )
                             

                             ## then consolidate the list into dataframe
                             if( class(rv_pre_define_ex$Fail_uncomtrade_country) != 'try-error' ){
                                print("----------- Success: Download Uncomtrade trade by country --------------")
                                ## get list to data frame
                                try(
                                rv_pre_define_ex$tmp_global_by_country_raw1 <- 
                                   do.call( rbind, rv_pre_define_ex$tmp_global_by_country_raw_list )
                                )
                                
                                ## change names
                                try(
                                rv_pre_define_ex$tmp_global_by_country_raw <-
                                   rv_pre_define_ex$tmp_global_by_country_raw1 %>%
                                   dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
                                   rename( Year = year,
                                           `Commodity.Code` = commodity_code ,
                                           `Trade.Flow` = trade_flow,
                                           Reporter = reporter,
                                           `Reporter.ISO` =  reporter_iso,
                                           Partner = partner,
                                           `Qty.Unit` = qty_unit,
                                           `Alt.Qty.Unit` = qty,
                                           `Trade.Value..US..` = trade_value_usd )
                                )
                             }
                             
                             
                             # try( 
                             #    rv_pre_define_ex$tmp_global_by_country_raw1 <- 
                             #       do.call( rbind, rv_pre_define_ex$tmp_global_by_country_raw_list )
                             #    )
                             
                             if( class(rv_pre_define_ex$Fail_uncomtrade_eu) != 'try-error' ){
                                print("----------- Success: Download Uncomtrade trade by EU --------------")
                                ## get list to data frame
                                try(
                                rv_pre_define_ex$tmp_global_by_eu_raw1 <- 
                                   do.call( rbind, rv_pre_define_ex$tmp_global_by_eu_raw_list )
                                )
                                
                                ## change names
                                try(
                                rv_pre_define_ex$tmp_global_by_eu_raw <-
                                   rv_pre_define_ex$tmp_global_by_eu_raw1 %>%
                                   dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
                                   rename( Year = year,
                                           `Commodity.Code` = commodity_code ,
                                           `Trade.Flow` = trade_flow,
                                           Reporter = reporter,
                                           `Reporter.ISO` =  reporter_iso,
                                           Partner = partner,
                                           `Qty.Unit` = qty_unit,
                                           `Alt.Qty.Unit` = qty,
                                           `Trade.Value..US..` = trade_value_usd )
                                )
                             }
                             
                             # try( 
                             #    rv_pre_define_ex$tmp_global_by_eu_raw1 <- 
                             #       do.call( rbind, rv_pre_define_ex$tmp_global_by_eu_raw_list )
                             # )   
                             
                             ### get data from un com trade -----
                             # rv_pre_define_ex$Fail_uncomtrade <- 
                             #    try(
                             #       rv_pre_define_ex$tmp_global_by_country_raw <-
                             #          rv_pre_define_ex$tmp_global_by_country_raw1 %>%
                             #          #get.Comtrade(r="all", p="0", rg = "1,2"  ## 1 means imports; 2 means exports (3 is re-exports excluded here)
                             #           #            , ps = paste0(tmp_un_comtrade_max_year, "," ,tmp_un_comtrade_max_year-5)
                             #            #           , cc = paste0(tmp_hs_ex(), collapse = ','), fmt = 'csv' )$data #%>%
                             #          # dplyr::select( yr, cmdCode, rgDesc, rtTitle, rt3ISO, ptTitle, qtDesc,  TradeQuantity, TradeValue) %>%
                             #          # mutate_all( as.character ) %>%
                             #          # mutate( yr = as.numeric(yr),
                             #          #         TradeQuantity = as.numeric( TradeQuantity ),
                             #          #         TradeValue = as.numeric( TradeValue )
                             #          #         ) %>%
                             #          # rename( Year = yr, `Commodity.Code` = cmdCode ,
                             #          #         `Trade.Flow` = rgDesc,
                             #          #         Reporter = rtTitle,
                             #          #         `Reporter.ISO` = rt3ISO,
                             #          #         Partner = ptTitle,
                             #          #         `Qty.Unit` = qtDesc,
                             #          #         `Alt.Qty.Unit` = TradeQuantity,
                             #          #         `Trade.Value..US..` = TradeValue )
                             #       
                             #       # m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                             #       #              commod_codes = as.character(tmp_hs_ex()),
                             #       #              start_date = tmp_un_comtrade_max_year - 4,
                             #       #              end_date = tmp_un_comtrade_max_year ) %>%
                             #       #    bind_rows( m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                             #       #                            commod_codes = as.character(tmp_hs_ex()),
                             #       #                            start_date = tmp_un_comtrade_max_year - 5,
                             #       #                            end_date = tmp_un_comtrade_max_year - 5 )
                             #       #               ) %>%
                             #          #filter( year >= tmp_un_comtrade_max_year-5 &
                             #           #          year <= tmp_un_comtrade_max_year ) %>%
                             #          dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
                             #          rename( Year = year,
                             #                  `Commodity.Code` = commodity_code ,
                             #                  `Trade.Flow` = trade_flow,
                             #                  Reporter = reporter,
                             #                  `Reporter.ISO` =  reporter_iso,
                             #                  Partner = partner,
                             #                  `Qty.Unit` = qty_unit,
                             #                  `Alt.Qty.Unit` = qty,
                             #                  `Trade.Value..US..` = trade_value_usd )
                             # 
                             #    )
                             
                             ## Eu export to world data
                             # rv_pre_define_ex$Fail_uncomtrade_eu <- 
                             #    try(
                             #       rv_pre_define_ex$tmp_global_by_eu_raw <-
                             #          rv_pre_define_ex$tmp_global_by_eu_raw1 %>%
                             #          dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
                             #          rename( Year = year,
                             #                  `Commodity.Code` = commodity_code ,
                             #                  `Trade.Flow` = trade_flow,
                             #                  Reporter = reporter,
                             #                  `Reporter.ISO` =  reporter_iso,
                             #                  Partner = partner,
                             #                  `Qty.Unit` = qty_unit,
                             #                  `Alt.Qty.Unit` = qty,
                             #                  `Trade.Value..US..` = trade_value_usd )
                             #       
                             #    )
                             
                             ## 
                             if( class(rv_pre_define_ex$Fail_uncomtrade_country) == "try-error" )
                                print(rv_pre_define_ex$Fail_uncomtrade_country)
                             
                             if( class(rv_pre_define_ex$Fail_uncomtrade_eu) == "try-error" )
                                print(rv_pre_define_ex$Fail_uncomtrade_eu)
                             
                             
                             ## when both data downloaded successfully then do -------
                             if( class(rv_pre_define_ex$Fail_uncomtrade_country) != "try-error" & 
                                 class(rv_pre_define_ex$Fail_uncomtrade_eu) != "try-error" &
                                 !is.null(rv_pre_define_ex$tmp_global_by_country_raw)  ){
                                ## 1. format the data -----
                                
                                ## global import and export of A commodity (sum over all HS code under this commodity) by country
                                rv_pre_define_ex$tmp_global_by_country <- 
                                   rv_pre_define_ex$tmp_global_by_country_raw %>%
                                   dplyr::select( Year,`Commodity.Code` , `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`, `Alt.Qty.Unit`, `Trade.Value..US..`) %>%
                                   #group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
                                   group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
                                   summarise( `Alt.Qty.Unit` = sum(`Alt.Qty.Unit`, na.rm=T),
                                              `Trade.Value..US..` = sum(`Trade.Value..US..`, na.rm=T)
                                   ) %>%
                                   ungroup %>%
                                   mutate( Price = `Trade.Value..US..`/ `Alt.Qty.Unit`) 
                                
                                ## EU import and export of A commodity from world
                                rv_pre_define_ex$tmp_eu_trade_extra_raw <- 
                                   rv_pre_define_ex$tmp_global_by_eu_raw %>%
                                   dplyr::select( Year,`Commodity.Code` , `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`, `Alt.Qty.Unit`, `Trade.Value..US..`) %>%
                                   #group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
                                   group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
                                   summarise( `Alt.Qty.Unit` = sum(`Alt.Qty.Unit`, na.rm=T),
                                              `Trade.Value..US..` = sum(`Trade.Value..US..`, na.rm=T)
                                   ) %>%
                                   ungroup #%>%
                                   #mutate( Price = `Trade.Value..US..`/ `Alt.Qty.Unit`) 
                                
                                ## 5 yr change in value and prices % and abs 
                                rv_pre_define_ex$tmp_global_by_country_change <-    
                                   rv_pre_define_ex$tmp_global_by_country %>%
                                   #group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
                                   group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
                                   do( Value_per_change = CAGR(.$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year]/
                                                                  .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ,
                                       Value_abs_change = .$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year] - .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5] ,
                                       Price_per_change = CAGR(.$Price[.$Year==tmp_un_comtrade_max_year]/
                                                                  .$Price[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ) %>%
                                   ungroup %>%
                                   mutate( Value_per_change = as.numeric(Value_per_change ),
                                           Value_abs_change = as.numeric(Value_abs_change ),
                                           Price_per_change = as.numeric(Price_per_change )
                                   )
                                
                                ## data frame for producing highchart tables 
                                rv_pre_define_ex$tmp_global_by_country_all <- 
                                   rv_pre_define_ex$tmp_global_by_country %>%
                                   filter( Year == tmp_un_comtrade_max_year ) %>%
                                   left_join( rv_pre_define_ex$tmp_global_by_country_change ) %>%
                                   group_by( Year, Trade.Flow  ) %>%
                                   mutate( Share = as.numeric(`Trade.Value..US..`)/ sum(as.numeric(`Trade.Value..US..`), na.rm=T ) ) %>%
                                   ungroup %>%
                                   arrange( `Trade.Flow`, -`Trade.Value..US..`) 
                                
                                ## 1.1 formate data -- get Eu28 intra and extra trade for later use in table ------
                                rv_pre_define_ex$tmp_eu_trade_all <- 
                                   rv_pre_define_ex$tmp_global_by_country %>%
                                   filter( Reporter.ISO %in% concord_eu28$ISO3 ) %>%
                                   #group_by( Year , `Trade.Flow`, Partner, `Qty.Unit` ) %>%
                                   group_by( Year , `Trade.Flow`, Partner ) %>%
                                   summarise(  `Alt.Qty.Unit` = sum( as.numeric(`Alt.Qty.Unit`), na.rm=T ),
                                               `Trade.Value..US..` = sum( as.numeric(`Trade.Value..US..`), na.rm=T ) ) %>%
                                   ungroup %>%
                                   mutate( Reporter = "EU-28", Reporter.ISO = 'EU2'   )
                                
                                ## derive EU trade intra
                                rv_pre_define_ex$tmp_eu_trade_intra_raw <-
                                   rv_pre_define_ex$tmp_eu_trade_all %>%
                                   left_join( rv_pre_define_ex$tmp_eu_trade_extra_raw,
                                              #by = c("Year", "Trade.Flow","Reporter", "Reporter.ISO", "Partner","Qty.Unit" )
                                              by = c("Year", "Trade.Flow","Reporter", "Reporter.ISO", "Partner" )
                                   ) %>%
                                   mutate( `Alt.Qty.Unit` = Alt.Qty.Unit.x - Alt.Qty.Unit.y, 
                                           `Trade.Value..US..` =  `Trade.Value..US...x` - `Trade.Value..US...y` ) %>%
                                   dplyr::select( -Alt.Qty.Unit.x, -Alt.Qty.Unit.y, 
                                                  -`Trade.Value..US...x`,  -`Trade.Value..US...y`) #%>%
                                   #mutate( Partner = "EU-28") 
                                
                                ### formate data
                                rv_pre_define_ex$tmp_eu_trade_intra <- 
                                   rv_pre_define_ex$tmp_eu_trade_intra_raw %>%
                                   mutate( Reporter = 'EU-28-Intra', Reporter.ISO = 'EU2-intra' )
                                
                                rv_pre_define_ex$tmp_eu_trade_extra <- 
                                   rv_pre_define_ex$tmp_eu_trade_extra_raw %>%
                                   mutate( Reporter = 'EU-28-Extra', Reporter.ISO = 'EU2-extra' )
                                 
                                ## join EU intra and extra back
                                rv_pre_define_ex$tmp_global_by_country_and_eu <-
                                   rv_pre_define_ex$tmp_global_by_country_raw %>%
                                   filter( !Reporter.ISO %in% concord_eu28$ISO3 ) %>%
                                   dplyr::select( Year,`Commodity.Code` , `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`, `Alt.Qty.Unit`, `Trade.Value..US..`) %>%
                                   #group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
                                   group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
                                   summarise( `Alt.Qty.Unit` = sum(`Alt.Qty.Unit`, na.rm=T),
                                              `Trade.Value..US..` = sum(`Trade.Value..US..`, na.rm=T)
                                   ) %>%
                                   ungroup %>%
                                   bind_rows( rv_pre_define_ex$tmp_eu_trade_intra ) %>%
                                   bind_rows( rv_pre_define_ex$tmp_eu_trade_extra  ) %>%
                                   mutate( Price = `Trade.Value..US..`/ `Alt.Qty.Unit`)
                                
                                ## 5 yr change in value and prices % and abs 
                                rv_pre_define_ex$tmp_global_by_country_and_eu_change <-    
                                   rv_pre_define_ex$tmp_global_by_country_and_eu %>%
                                   #group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
                                   group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
                                   do( Value_per_change = CAGR(.$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year]/
                                                                  .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ,
                                       Value_abs_change = .$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year] - .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5] ,
                                       Price_per_change = CAGR(.$Price[.$Year==tmp_un_comtrade_max_year]/
                                                                  .$Price[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ) %>%
                                   ungroup %>%
                                   mutate( Value_per_change = as.numeric(Value_per_change ),
                                           Value_abs_change = as.numeric(Value_abs_change ),
                                           Price_per_change = as.numeric(Price_per_change ) )
                                
                                ## data frame for producing highchart tables 
                                rv_pre_define_ex$tmp_global_by_country_and_eu_all <- 
                                   rv_pre_define_ex$tmp_global_by_country_and_eu %>%
                                   filter( Year == tmp_un_comtrade_max_year ) %>%
                                   left_join( rv_pre_define_ex$tmp_global_by_country_and_eu_change ) %>%
                                   group_by( Year, Trade.Flow  ) %>%
                                   mutate( Share = as.numeric(`Trade.Value..US..`)/ sum(as.numeric(`Trade.Value..US..`), na.rm=T ) ) %>%
                                   ungroup %>%
                                   arrange( `Trade.Flow`, -`Trade.Value..US..`) 
                                
                                ## 2. calculate values for later use ------------   
                                ## Global market size -- value now
                                rv_pre_define_ex$tmp_global_size_value_now <- 
                                   rv_pre_define_ex$tmp_global_by_country %>%
                                   group_by(Year, `Trade.Flow`,  Partner ) %>%
                                   summarise(`Trade.Value..US..` = sum(as.numeric(`Trade.Value..US..`), na.rm=T) ) %>%
                                   ungroup %>%
                                   filter( Year == tmp_un_comtrade_max_year,
                                           `Trade.Flow` == 'Import') %>%
                                   dplyr::select( `Trade.Value..US..` ) %>%
                                   as.numeric()
                                
                                ## Global market size -- value 5 years ago
                                rv_pre_define_ex$tmp_global_size_value_pre <- 
                                   rv_pre_define_ex$tmp_global_by_country %>%
                                   group_by(Year, `Trade.Flow`,  Partner ) %>%
                                   summarise(`Trade.Value..US..` = sum(as.numeric(`Trade.Value..US..`), na.rm=T) ) %>%
                                   ungroup %>%
                                   filter( Year == tmp_un_comtrade_max_year-5,
                                           `Trade.Flow` == 'Import') %>%
                                   dplyr::select( `Trade.Value..US..` ) %>%
                                   as.numeric()
                                
                                ## Global market size -- value change %
                                rv_pre_define_ex$tmp_global_size_value_change <-
                                   CAGR( rv_pre_define_ex$tmp_global_size_value_now/
                                            rv_pre_define_ex$tmp_global_size_value_pre, 5)/100
                                
                                ## Global market size -- value change abs
                                rv_pre_define_ex$tmp_global_size_value_change_abs <-
                                   rv_pre_define_ex$tmp_global_size_value_now - rv_pre_define_ex$tmp_global_size_value_pre 
                                
                                ## Top 3 importers share
                                rv_pre_define_ex$tmp_top3_importers_share <-
                                   rv_pre_define_ex$tmp_global_by_country_all %>%
                                   filter( `Trade.Flow` == 'Import' ) %>%
                                   arrange( -Share ) %>%
                                   slice(1:3) %>%
                                   group_by(Year) %>%
                                   summarise( Share = sum(Share, na.rm=T) ) %>%
                                   ungroup %>%
                                   dplyr::select(Share) %>%
                                   as.numeric
                                
                                ## Top 10 importers share
                                rv_pre_define_ex$tmp_top10_importers_share <-
                                   rv_pre_define_ex$tmp_global_by_country_all %>%
                                   filter( `Trade.Flow` == 'Import' ) %>%
                                   arrange( -Share ) %>%
                                   slice(1:10) %>%
                                   group_by(Year) %>%
                                   summarise( Share = sum(Share, na.rm=T) ) %>%
                                   ungroup %>%
                                   dplyr::select(Share) %>%
                                   as.numeric
                                
                                ##  of top 20 markets -- number of high growth market
                                rv_pre_define_ex$tmp_number_high_growth_importers <-
                                   nrow(
                                      rv_pre_define_ex$tmp_global_by_country_all %>%
                                         filter( `Trade.Flow` == 'Import' ) %>%
                                         arrange( -Share ) %>%
                                         slice(1:20) %>%
                                         filter( Value_per_change >= 0.1 )
                                   )
                                
                                ## Top 3 exporters share
                                rv_pre_define_ex$tmp_top3_exporters_share <-
                                   rv_pre_define_ex$tmp_global_by_country_all %>%
                                   filter( `Trade.Flow` == 'Export' ) %>%
                                   arrange( -Share ) %>%
                                   slice(1:3) %>%
                                   group_by(Year) %>%
                                   summarise( Share = sum(Share, na.rm=T) ) %>%
                                   ungroup %>%
                                   dplyr::select(Share) %>%
                                   as.numeric
                                
                                ## Top 10 exporters share
                                rv_pre_define_ex$tmp_top10_exporters_share <-
                                   rv_pre_define_ex$tmp_global_by_country_all %>%
                                   filter( `Trade.Flow` == 'Export' ) %>%
                                   arrange( -Share ) %>%
                                   slice(1:10) %>%
                                   group_by(Year) %>%
                                   summarise( Share = sum(Share, na.rm=T) ) %>%
                                   ungroup %>%
                                   dplyr::select(Share) %>%
                                   as.numeric
                                
                                ## NZ's share
                                rv_pre_define_ex$tmp_nz_share <-
                                   rv_pre_define_ex$tmp_global_by_country_all %>%
                                   filter( `Trade.Flow` == 'Export' ) %>%
                                   filter( Reporter == 'New Zealand' ) %>%
                                   dplyr::select(Share) %>%
                                   as.numeric
                                
                                ## 3. build data for importers and exporter maps -------------------
                                rv_pre_define_ex$tmp_un_comtrade_importer_map <- 
                                   rv_pre_define_ex$tmp_global_by_country_all %>%
                                   filter( `Trade.Flow` == "Import" ) %>%
                                   left_join( concord_uncomtrade_country, by = c('Reporter.ISO' = 'ISO3') ) %>%
                                   filter( !is.na(lat) ) %>%
                                   mutate( Value = `Trade.Value..US..`/10^6,
                                           z= Value,
                                           name = Reporter)
                                
                                rv_pre_define_ex$tmp_un_comtrade_exporter_map <- 
                                   rv_pre_define_ex$tmp_global_by_country_all %>%
                                   filter( `Trade.Flow` == "Export" ) %>%
                                   left_join( concord_uncomtrade_country, by = c('Reporter.ISO' = 'ISO3') ) %>%
                                   filter( !is.na(lat) ) %>%
                                   mutate( Value = `Trade.Value..US..`/10^6,
                                           z= Value,
                                           name = Reporter)
                                
                                ## 4. Build data for the summary table -----------------
                                ## import tab
                                rv_pre_define_ex$tmp_un_comtrade_import_summary_tab <- 
                                   rv_pre_define_ex$tmp_global_by_country_and_eu_all %>%
                                   filter( `Trade.Flow` == 'Import' ) %>%
                                   dplyr::select( Reporter, Share, 
                                                  `Trade.Value..US..` ,Value_per_change, Value_abs_change,  
                                                  Price, Price_per_change ) %>%
                                   mutate( `Trade.Value..US..` = `Trade.Value..US..`/10^6,
                                           Value_abs_change = Value_abs_change/10^6)
                                
                                ## export tab
                                rv_pre_define_ex$tmp_un_comtrade_export_summary_tab <- 
                                   rv_pre_define_ex$tmp_global_by_country_and_eu_all %>%
                                   filter( `Trade.Flow` == 'Export' ) %>%
                                   dplyr::select( Reporter, Share, 
                                                  `Trade.Value..US..` ,Value_per_change, Value_abs_change,  
                                                  Price, Price_per_change ) %>%
                                   mutate( `Trade.Value..US..` = `Trade.Value..US..`/10^6,
                                           Value_abs_change = Value_abs_change/10^6)
                             }
                          })
                          
                          ## 2.6.1 IF hourly query reach 100 ------------
                          # print("--------- Building Fail messages if no data -------------")
                          # output$Un_comtrade_fail_msg <- 
                          #    renderUI({
                          #       if( is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                          #          tags$h1( "Global analysis cannot be performed due to reaching usage limit of 100 requests per hour. Please come back in a hour time." )
                          #    })
                          # 
                          # insertUI(selector = '#body_ci_markets_ex',
                          #          ui = div(id = "#body_ci_markets_ex_fail_msg",
                          #                   uiOutput("Un_comtrade_fail_msg")
                          #                   )
                          #          )
   
                          
                          ## 2.7 UN com Trade data analysis starts here Key facts table ----------
                          ## world market size
                          print("--------- Building facts value boxes -------------")
                          output$Un_comtrade_world_market_size_pre_define <-
                             renderInfoBox({
                                if( is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                infoBox( "World market size",
                                         paste0("$", 
                                                format(round(rv_pre_define_ex$tmp_global_size_value_now/10^6), big.mark = ","),
                                                " m"
                                         )
                                         , icon = icon('globe', lib = "glyphicon")
                                         
                                )
                             })
                          
                          ## 5 year growth
                          output$Un_comtrade_world_market_change_pre_define <-
                             renderInfoBox({
                                if( is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                
                                if( is.null(rv_pre_define_ex$tmp_global_size_value_change) )
                                   infoBox( "CAGR (5 years)",
                                            HTML(paste0( "Not available" )), 
                                            icon = icon('minus'))
                                
                                if(rv_pre_define_ex$tmp_global_size_value_change>0 ){
                                   infoBox( "CAGR (5 years)",
                                            HTML(paste0( "<font color='green'> +",
                                                         round(abs(rv_pre_define_ex$tmp_global_size_value_change)*100,1),
                                                         "% </font>"
                                            )), 
                                            icon = icon('arrow-up'), color = 'green')
                                }else{
                                   infoBox( "CAGR (5 years)",
                                            HTML(paste0( "<font color='red'> -",
                                                         round(abs(rv_pre_define_ex$tmp_global_size_value_change)*100,1),
                                                         "% </font>"
                                            )), 
                                            icon = icon('arrow-down'), color = 'red')
                                }
                                
                             })
                          
                          ## 5 yr abs change
                          output$Un_comtrade_world_market_change_abs_pre_define <-
                             renderInfoBox({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw) )
                                   return(NULL)
                                
                                if( is.null(rv_pre_define_ex$tmp_global_size_value_change_abs) )
                                   infoBox( "ABS (5 years)",
                                            HTML(paste0( "Not available" )), 
                                            icon = icon('minus'))
                                
                                if(rv_pre_define_ex$tmp_global_size_value_change_abs>0 ){
                                   infoBox( "ABS (5 years)",
                                            HTML(paste0("<font color='green'> +$", 
                                                        format(round(rv_pre_define_ex$tmp_global_size_value_change_abs/10^6), big.mark = ","),
                                                        " m </font>"
                                            )),
                                            icon = icon('arrow-up'), color = 'green')
                                }else{
                                   infoBox( "ABS (5 years)",
                                            HTML(paste0("<font color='red'> -$", 
                                                        format(round(abs(rv_pre_define_ex$tmp_global_size_value_change_abs)/10^6), big.mark = ","),
                                                        " m </font>"
                                            )),
                                            icon = icon('arrow-down'), color = 'red')
                                }
                             })
                          
                          ## top 3 importer share
                          output$Un_comtrade_top3_importers_share_pre_define <-
                             renderInfoBox({
                                if( is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                infoBox( HTML("Top 3 importers <br> share"),
                                         paste0( 
                                            round(abs(rv_pre_define_ex$tmp_top3_importers_share)*100,1),
                                            "%"
                                         ),
                                         icon = icon('import', lib = "glyphicon"))
                             })
                          
                          ## top 10 importer share
                          output$Un_comtrade_top10_importers_share_pre_define <-
                             renderInfoBox({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                infoBox( HTML("Top 10 importers <br> share"),
                                         paste0( 
                                            round(abs(rv_pre_define_ex$tmp_top10_importers_share)*100,1),
                                            "%"
                                         ),
                                         icon = icon('import', lib = "glyphicon"))
                             })
                          
                          ##  of top 20 markets -- number of high growth market
                          output$Un_comtrade_high_growth_importers_pre_define <-
                             renderInfoBox({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                infoBox( HTML("Top 20 importers <br> with CAGR>10%"),
                                         paste0( rv_pre_define_ex$tmp_number_high_growth_importers) ,
                                         icon = icon('import', lib = "glyphicon"))
                             })
                          
                          
                          ## top 3 exporter share
                          output$Un_comtrade_top3_exporters_share_pre_define <-
                             renderInfoBox({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw) )
                                   return(NULL)
                                infoBox( HTML("Top 3 exporters <br> share"),
                                         paste0( 
                                            round(abs(rv_pre_define_ex$tmp_top3_exporters_share)*100,1),
                                            "%"
                                         ),
                                         icon = icon('export', lib = "glyphicon"))
                             })
                          
                          ## top 10 exporter share
                          output$Un_comtrade_top10_exporters_share_pre_define <-
                             renderInfoBox({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                infoBox( HTML("Top 10 exporters <br> share"),
                                         paste0( 
                                            round(abs(rv_pre_define_ex$tmp_top10_exporters_share)*100,1),
                                            "%"
                                         ),
                                         icon = icon('export', lib = "glyphicon"))
                             })
                          
                          ## new zealand share
                          output$Un_comtrade_nz_share_pre_define <-
                             renderInfoBox({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                if( rv_pre_define_ex$tmp_nz_share < 0.001 ){
                                   infoBox( HTML("New Zealand <br> share"),
                                            paste0( "Less than 0.1%" ),
                                            icon = icon('export', lib = "glyphicon"))
                                }else{
                                   infoBox( HTML("New Zealand <br> share"),
                                            paste0( 
                                               round(abs(rv_pre_define_ex$tmp_nz_share)*100,1),
                                               "%"
                                            ),
                                            icon = icon('export', lib = "glyphicon"))
                                }
                                
                             })
                          
                          
                          ##!!!!! try UI insert: value box for global market facts ----------- 
                          output$H1_title_global_facts_pre_define <-
                             renderText({
                                if( is.null(rv_pre_define_ex$tmp_global_by_country_raw)   )
                                   return(NULL)
                                paste0( "Global market analysis (", tmp_un_comtrade_max_year ,")" )
                             })
                          
                          output$H1_title_global_facts_note_pre_define <-
                             renderText({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                paste0( "All values undner the global market analysis are reported in current US dollar" )
                             })
                          
                          output$H3_title_global_facts_summary_pre_define <-
                             renderText({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)   )
                                   return(NULL)
                                paste0( "Key facts and summary" )
                             })
                          
                          ### insert global market key facts and summary value boxe
                          insertUI(
                             selector = '#body_ci_markets_ex',
                             ui =   div( id = 'body_ci_markets_ex_global_facts',
                                         fluidRow( 
                                            h1( HTML(paste0(textOutput("H1_title_global_facts_pre_define"))) ),
                                            p( HTML(paste0(textOutput("H1_title_global_facts_note_pre_define"))) ),
                                            h3( HTML(paste0(textOutput("H3_title_global_facts_summary_pre_define"))) ),
                                            infoBoxOutput("Un_comtrade_world_market_size_pre_define") ,
                                            infoBoxOutput("Un_comtrade_world_market_change_pre_define" ) ,
                                            infoBoxOutput("Un_comtrade_world_market_change_abs_pre_define" ) 
                                         ),
                                         fluidRow(
                                            infoBoxOutput("Un_comtrade_top3_importers_share_pre_define" ) ,
                                            infoBoxOutput("Un_comtrade_top10_importers_share_pre_define" ) ,
                                            infoBoxOutput("Un_comtrade_high_growth_importers_pre_define" ) 
                                         ),
                                         fluidRow(
                                            infoBoxOutput("Un_comtrade_top3_exporters_share_pre_define" ) ,
                                            infoBoxOutput("Un_comtrade_top10_exporters_share_pre_define" ) ,
                                            infoBoxOutput("Un_comtrade_nz_share_pre_define" ) 
                                         )
                             )
                          )
                          
                          
                          ## 2.8 Quick glance at both importers and exporters map --------
                          print("--------- Building importer and exporter map -------------")
                          output$UN_comtrade_importer_Map_pre_define <- 
                             renderHighchart({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                hcmap( data = rv_pre_define_ex$tmp_un_comtrade_importer_map ,
                                       value = 'Value',
                                       joinBy = c('iso-a2','ISO2'), 
                                       name= paste0( "Import value"),
                                       borderWidth = 1,
                                       borderColor = "#fafafa",
                                       nullColor = "lightgrey",
                                       tooltip = list( table = TRUE,
                                                       sort = TRUE,
                                                       headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                       pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
                                ) %>%
                                   hc_add_series(data =  rv_pre_define_ex$tmp_un_comtrade_importer_map ,
                                                 type = "mapbubble",
                                                 color  = hex_to_rgba("#DF1995", 0.9),
                                                 minSize = 0,
                                                 name= paste0( "Import value"),
                                                 maxSize = 30,
                                                 tooltip = list(table = TRUE,
                                                                sort = TRUE,
                                                                headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                                pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
                                   ) %>%
                                   hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                   hc_legend( enabled=FALSE ) %>% 
                                   hc_mapNavigation(enabled = TRUE) 
                             })
                          
                          ## exporter map
                          output$UN_comtrade_exporter_Map_pre_define <- 
                             renderHighchart({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                hcmap( data = rv_pre_define_ex$tmp_un_comtrade_exporter_map ,
                                       value = 'Value',
                                       joinBy = c('iso-a2','ISO2'), 
                                       name= paste0( "Export value"),
                                       borderWidth = 1,
                                       borderColor = "#fafafa",
                                       nullColor = "lightgrey",
                                       tooltip = list( table = TRUE,
                                                       sort = TRUE,
                                                       headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                       pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
                                ) %>%
                                   hc_add_series(data =  rv_pre_define_ex$tmp_un_comtrade_exporter_map ,
                                                 type = "mapbubble",
                                                 color  = hex_to_rgba("#97D700", 0.9),
                                                 minSize = 0,
                                                 name= paste0( "Export value"),
                                                 maxSize = 30,
                                                 tooltip = list(table = TRUE,
                                                                sort = TRUE,
                                                                headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                                pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
                                   ) %>%
                                   hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                   hc_legend( enabled=FALSE ) %>% 
                                   hc_mapNavigation(enabled = TRUE) 
                             })
                          
                          ## !!!!! try UI insert: map of importer / exporters ----------- 
                          output$H3_title_un_comtrade_map_pre_define <-
                             renderText({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw) )
                                   return(NULL)
                                paste0("Global importers and exporters at a glance")
                             })
                          
                          output$H3_title_un_comtrade_map_note_pre_define <-
                             renderText({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                paste0( "The size of bubble area and color both represent the value of imports or exports" ) 
                             })
                          
                          output$H4_title_un_comtrade_importer_map_pre_define <-
                             renderText({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                paste0("Global IMPORT markets")
                             })
                          
                          output$H4_title_un_comtrade_exporter_map_pre_define <-
                             renderText({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                paste0("Global EXPORT markets")
                             })
                          
                          ## Insert ui here
                          insertUI(
                             selector = '#body_ci_markets_ex',
                             ui =   div( id = 'body_ci_markets_ex_un_comtrade_map',
                                         fluidRow(h3( HTML(paste0(textOutput("H3_title_un_comtrade_map_pre_define"))) ) ,
                                                  p( HTML(paste0(textOutput("H3_title_un_comtrade_map_note_pre_define"))) ),
                                                  column(6, div(id = "body_ci_markets_ex_un_comtrade_map_import", h4( HTML(paste0(textOutput("H4_title_un_comtrade_importer_map_pre_define"))) ), highchartOutput('UN_comtrade_importer_Map_pre_define') ) ),
                                                  column(6, div(id = "body_ci_markets_ex_un_comtrade_map_export", h4( HTML(paste0(textOutput("H4_title_un_comtrade_exporter_map_pre_define"))) ), highchartOutput('UN_comtrade_exporter_Map_pre_define') ) )
                                         )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          
                          ## 2.8.1 Sankey plot for a commodity ---------------
                          print("--------- Building Sankey data -------------")
                          
                          observe({
                             ## check if able to get sankey data
                             rv_pre_define_ex$Fail_sankey_data <-
                                try(
                                   rv_pre_define_ex$sankey_plot_data <-
                                      get_data_sankey_uncomtrade( cc = tmp_hs_ex(), max_year = tmp_un_comtrade_max_year, eu_internal = "No" )
                                )

                             if( class(rv_pre_define_ex$Fail_sankey_data) == 'try-error' )
                                print("--------- FAIL: building Sankey data !!! -------------")
                          })
                          
                          print("--------- Building Sankey plots -------------")
                          output$Sankey_trade <-
                             renderSankeyNetwork({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw) | 
                                     length(tmp_hs_ex())>1 |  
                                     class(rv_pre_define_ex$Fail_sankey_data) == 'try-error' ){
                                   return(NULL)
                                }else{
                                   print("--------- Plotting Sankey plots -------------")
                                   sankey_uncomtrade( cc = tmp_hs_ex(), max_year = tmp_un_comtrade_max_year,eu_internal = as.character(input$btn_eu_internal)  )
                                }
                             })
                          
                          ## !!!!! try UI insert: Sankey plot ----------- 
                          output$H3_title_sankey <-
                             renderText({
                                # if( is.null(rv_pre_define_ex$tmp_global_by_country_raw) | 
                                #     length(tmp_hs_ex())>1 |  
                                #     class(rv_pre_define_ex$Fail_sankey_data) == 'try-error' )
                                #    #return(NULL)
                                # {paste0("Unable to perform global trade flow analyasis due to data query limits. Please wait for a hour.")}else{
                                #    paste0( "Global trade flow analysis" )
                                # }
                                
                                if( class(rv_pre_define_ex$Fail_sankey_data) == 'try-error' & 
                                    input$select_comodity_ex_for_market_analysis != "" &
                                    length(tmp_hs_ex()) == 1 ){
                                   paste0("Unable to perform global trade flow analyasis due to data query limits. Please wait for a hour.")
                                }
                                
                                if( class(rv_pre_define_ex$Fail_sankey_data) == 'try-error' & 
                                    input$select_comodity_ex_for_market_analysis == ""  ){
                                   return(NULL)
                                }
                                
                                if( class(rv_pre_define_ex$Fail_sankey_data) != 'try-error' &
                                    input$select_comodity_ex_for_market_analysis != "" &
                                    length(tmp_hs_ex()) ==  1){
                                   paste0( "Global trade flow analysis" )
                                }
                             })

                          output$H3_title_sankey_note <-
                             renderUI({
                                if( is.null(rv_pre_define_ex$tmp_global_by_country_raw) | 
                                    length(tmp_hs_ex())>1 |  
                                    class(rv_pre_define_ex$Fail_sankey_data) == 'try-error' )
                                   return(NULL)
                                tags$p("This sankey plot shows trade flows of the selected commodity from expoters to importers. The displayed markets coverage is equal to or greater than 90% of global exports. The displayed trade flows are equal to or greater than 0.5% of global exports. Different colors are used to distinguish",
                                   tags$span( "EXPORTERS", style = "color: #97D700; font-weight: bold" ),
                                   ", ",
                                   tags$span( "IMPORTERS", style = "color: #CD5B45; font-weight: bold"),
                                  ", and ",
                                   tags$span( "BOTH", style = "color: #FBE122; font-weight: bold"), "." )

                             })

                          ## button to choose show/hide EU internal trade
                          output$Btn_EU_Internal <-
                             renderUI({
                                if( is.null(rv_pre_define_ex$tmp_global_by_country_raw) |
                                    length(tmp_hs_ex())>1 |
                                    class(rv_pre_define_ex$Fail_sankey_data) == 'try-error' )
                                   return(NULL)
                                radioButtons("btn_eu_internal",
                                             p("Display EU internal trade: " ),
                                             choiceNames = list(icon("check"), icon("times")),
                                             choiceValues = list( "Yes" , "No"),
                                             #c( "Yes" = "Yes", "No" = "No"),
                                             inline=T,
                                             selected="No")
                             })

                          output$Btn_EU_Internal_note <-
                             renderUI({
                                if( is.null(rv_pre_define_ex$tmp_global_by_country_raw) | 
                                    length(tmp_hs_ex())>1 |  
                                    class(rv_pre_define_ex$Fail_sankey_data) == 'try-error')
                                   return(NULL)
                                tags$p( "You may choose to show or hide EU internal trade in the sankey plot by using the buttons below." )
                             })

                          ## Insert ui here
                          insertUI(
                             selector = '#body_ci_markets_ex',
                             ui =   div( id = 'body_ci_markets_ex_un_comtrade_sankey',
                                         fluidRow(h3( HTML(paste0(textOutput("H3_title_sankey"))) ) ,
                                                  #p( HTML(paste0(textOutput("H2_title_sankey_note"))) ),
                                                  uiOutput("H3_title_sankey_note"),
                                                  uiOutput("Btn_EU_Internal_note"),
                                                  uiOutput("Btn_EU_Internal"),
                                                  sankeyNetworkOutput( "Sankey_trade" )
                                         )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          ## 2.9 Generating summary tables for both importers and exporters -------
                          # container of the table -- importers 
                          print("--------- Building importer and exporter tabels -------------")
                          
                          sketch_uncomtrade_im<-  htmltools::withTags(table(
                             class = 'display',
                             thead(
                                tr(
                                   th(rowspan = 2, 'Market'),
                                   th(rowspan = 2, 'Import share'),
                                   th(colspan = 3, 'Import value'),
                                   th(colspan = 2, 'Import price')
                                ),
                                tr( #th('Country'),
                                   lapply(rep(c('Value ($m)', 'CAGR5', 'ABS5'), 1), th, align = 'center'),
                                   lapply(rep(c('$/kg (unit)', 'CAGR5' ), 1), th, align = 'center')
                                )
                             )
                          ))
                          
                          ## table for importers
                          output$UN_com_trade_importer_summary_pre_define <-
                             renderDataTable({
                                if( is.null(rv_pre_define_ex$tmp_global_by_country_raw)   )
                                   return(NULL)
                                datatable( rv_pre_define_ex$tmp_un_comtrade_import_summary_tab,
                                           container = sketch_uncomtrade_im,
                                           rownames = FALSE,
                                           extensions = 'Buttons',
                                           options = list(dom = 'Bltp',
                                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                          scrollX = TRUE,
                                                          pageLength = 10,
                                                          lengthMenu = list(c(10, 30 , -1), list('10','30' ,'All')),
                                                          columnDefs = list(list(className = 'dt-center', 
                                                                                 targets = 0:(ncol(rv_pre_define_ex$tmp_un_comtrade_import_summary_tab)-1) ) )
                                           )
                                ) %>%
                                   formatPercentage( c('Share', 'Value_per_change', 'Price_per_change' ) , digit = 1 ) %>%
                                   formatCurrency( columns = c('Trade.Value..US..','Value_abs_change'), digits = 0 ) %>%
                                   formatCurrency( columns = c('Price'), digits = 2 ) %>%
                                   formatStyle(
                                      c('Value_per_change' ),
                                      background = styleColorBar( c(0,max(rv_pre_define_ex$tmp_un_comtrade_import_summary_tab[1:min(20,nrow(rv_pre_define_ex$tmp_un_comtrade_import_summary_tab)),c('Value_per_change' )],na.rm=T)*2) ,
                                                                  'lightblue'),
                                      backgroundSize = '100% 90%',
                                      backgroundRepeat = 'no-repeat',
                                      backgroundPosition = 'center',
                                      color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                   ) %>%
                                   formatStyle(
                                      c('Price_per_change' ),
                                      background = styleColorBar( c(0,max(rv_pre_define_ex$tmp_un_comtrade_import_summary_tab[1:min(20,nrow(rv_pre_define_ex$tmp_un_comtrade_import_summary_tab)),c('Price_per_change' )],na.rm=T)*2) ,
                                                                  'lightblue'),
                                      backgroundSize = '100% 90%',
                                      backgroundRepeat = 'no-repeat',
                                      backgroundPosition = 'center',
                                      color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                   ) %>%
                                   formatStyle(
                                      c('Value_abs_change' ),
                                      backgroundSize = '100% 90%',
                                      backgroundRepeat = 'no-repeat',
                                      backgroundPosition = 'center',
                                      color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                   ) %>%
                                   formatStyle( 1:ncol(rv_pre_define_ex$tmp_un_comtrade_import_summary_tab), 'vertical-align'='center', 'text-align' = 'center' )
                             })
                          
                          ### build export table
                          # container of the table -- importers 
                          sketch_uncomtrade_ex <-  htmltools::withTags(table(
                             class = 'display',
                             thead(
                                tr(
                                   th(rowspan = 2, 'Market'),
                                   th(rowspan = 2, 'Export share'),
                                   th(colspan = 3, 'Export value'),
                                   th(colspan = 2, 'Export price')
                                ),
                                tr( #th('Country'),
                                   lapply(rep(c('Value ($m)', 'CAGR5', 'ABS5'), 1), th, align = 'center'),
                                   lapply(rep(c('$/kg (unit)', 'CAGR5' ), 1), th, align = 'center')
                                )
                             )
                          ))
                          
                          ## table for importers
                          output$UN_com_trade_exporter_summary_pre_define <-
                             renderDataTable({
                                if( is.null(rv_pre_define_ex$tmp_global_by_country_raw) )
                                   return(NULL)
                                datatable( rv_pre_define_ex$tmp_un_comtrade_export_summary_tab,
                                           container = sketch_uncomtrade_ex,
                                           rownames = FALSE,
                                           extensions = 'Buttons',
                                           options = list(dom = 'Bltp', 
                                                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                          scrollX = TRUE,
                                                          pageLength = 10,
                                                          lengthMenu = list(c(10, 30, -1), list('10', '30' ,'All')),
                                                          columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(rv_pre_define_ex$tmp_un_comtrade_export_summary_tab)-1) ) )
                                           )
                                ) %>%
                                   formatPercentage( c('Share', 'Value_per_change', 'Price_per_change' ) , digit = 1 ) %>%
                                   formatCurrency( columns = c('Trade.Value..US..','Value_abs_change'), digits = 0 ) %>%
                                   formatCurrency( columns = c('Price'), digits = 2 ) %>%
                                   formatStyle(
                                      c('Value_per_change' ),
                                      background = styleColorBar( c(0,max(rv_pre_define_ex$tmp_un_comtrade_export_summary_tab[1:min(20,nrow(rv_pre_define_ex$tmp_un_comtrade_export_summary_tab)),c('Value_per_change' )],na.rm=T)*2) ,
                                                                  'lightblue'),
                                      backgroundSize = '100% 90%',
                                      backgroundRepeat = 'no-repeat',
                                      backgroundPosition = 'center',
                                      color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                   ) %>%
                                   formatStyle(
                                      c('Price_per_change' ),
                                      background = styleColorBar( c(0,max(rv_pre_define_ex$tmp_un_comtrade_export_summary_tab[1:min(20,nrow(rv_pre_define_ex$tmp_un_comtrade_export_summary_tab)),c('Price_per_change' )],na.rm=T)*2) ,
                                                                  'lightblue'),
                                      backgroundSize = '100% 90%',
                                      backgroundRepeat = 'no-repeat',
                                      backgroundPosition = 'center',
                                      color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                   ) %>%
                                   formatStyle(
                                      c('Value_abs_change' ),
                                      backgroundSize = '100% 90%',
                                      backgroundRepeat = 'no-repeat',
                                      backgroundPosition = 'center',
                                      color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                   ) %>%
                                   formatStyle( 1:ncol(rv_pre_define_ex$tmp_un_comtrade_export_summary_tab), 'vertical-align'='center', 'text-align' = 'center' )
                             })
                          
                          ## Insert ui here: summary tables  ----------------
                          output$H3_title_un_comtrade_summary_tab_pre_define <-
                             renderText({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw) )
                                   return(NULL)
                                paste0("Summary tables for importers and exporters")
                             })
                          
                          output$H3_title_un_comtrade_summary_tab_pre_define_note <-
                             renderText({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw) )
                                   return(NULL)
                                paste0("EU-28-Intra means all internal trade among the EU28 countries. EU-28-Extra means trade between EU28 as a whole to the rest of the world. Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years. Import or export prices will be displayed when quantity of the selected commodity is available.")
                             })
                          
                          
                          output$H4_title_un_comtrade_importer_sum_tab_pre_define <-
                             renderText({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                paste0("Global IMPORT markets")
                             })
                          
                          output$H4_title_un_comtrade_exporter_sum_tab_pre_define <-
                             renderText({
                                if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw)  )
                                   return(NULL)
                                paste0("Global EXPORT markets")
                             })
                          
                          insertUI(
                             selector = '#body_ci_markets_ex',
                             ui =   div( id = 'body_ci_markets_ex_un_comtrade_summary_tab',
                                         fluidRow(h3( HTML(paste0(textOutput("H3_title_un_comtrade_summary_tab_pre_define"))) ) ,
                                                  p( HTML(paste0(textOutput("H3_title_un_comtrade_summary_tab_pre_define_note"))) ),
                                                  column(6, div(id = "body_ci_markets_ex_un_comtrade_import_summary_tab", h4( HTML(paste0(textOutput("H4_title_un_comtrade_importer_sum_tab_pre_define"))) ), dataTableOutput('UN_com_trade_importer_summary_pre_define') ) ),
                                                  column(6, div(id = "body_ci_markets_ex_un_comtrade_export_summary_tab", h4( HTML(paste0(textOutput("H4_title_un_comtrade_exporter_sum_tab_pre_define"))) ), dataTableOutput('UN_com_trade_exporter_summary_pre_define') ) )
                                         )
                             )
                          )
                          ## end Try UI insert --------##
                          

                          
                          ## 3.0 Get the leftover quota and reset time ---------
                          output$Un_comtrade_msg <-
                             renderUI({
                                #if(  is.null(rv_pre_define_ex$tmp_global_by_country_raw) )
                                 #  return(NULL)
                                # tags$p(paste0( "Note: ",ct_get_remaining_hourly_queries(), 
                                #                " number of queries are left for the global analysis section from the UN Comtrade. The reset time will be at ", 
                                #                ct_get_reset_time() ,
                                #                ", while the current time is ", format(Sys.time()) , "."
                                #                )
                                #          )
                                
                                if(  input$select_comodity_ex_for_market_analysis == "" ){
                                   return(NULL)
                                }else{
                                   tags$div(
                                      tags$hr(),
                                      tags$p(paste0( "Note: ",ct_get_remaining_hourly_queries(), 
                                                     " number of queries are left for the global analysis section from the UN Comtrade. The reset time will be at ", 
                                                     ct_get_reset_time() ,
                                                     ", while the current time is ", format(Sys.time()) , "."
                                      ))
                                   )
                                }
                                
                             })
                          
                          insertUI( selector = '#body_ci_markets_ex',
                                    ui = div( id = 'body_ci_markets_ex_un_comtrade_msg',
                                              fluidRow( #tags$hr(),
                                                        uiOutput("Un_comtrade_msg") ) 
                                              )
                                    )
                          
                          ## 4.15 Hide generating report message ----------
                          observe({
                             if( any(input$select_comodity_ex_for_market_analysis %in% tmp_tab$Name)   ){
                                shinyjs::hide( id = "body_ci_market_loading_message" )
                             }
                          })
                          
                          ## hide waite message ----
                          shinyjs::hide( id = 'wait_message_ci_ex' )
                       }
                       
                       ## 1.3 work on Self defined warning if no .csv HS code and grouping uploaded ------------------
                       if(input$rbtn_prebuilt_diy_ex=='Self-defined' & 
                          is.null(input$file_comodity_ex) ) {
                          showModal(modalDialog(
                             title = "Warning",
                             tags$b("Please upload an appropriate CSV file with HS codes and groupsings!"),
                             size = 's'
                          ))
                       }
                       
                       ## Now if a csv file is uploaded -- check HS groupings
                       if(input$rbtn_prebuilt_diy_ex=='Self-defined' & 
                          !is.null(input$file_comodity_ex) ){
                          ## warning if not a CSV file
                          if( !grepl(".csv",input$file_comodity_ex$datapath) ){
                             showModal(modalDialog(
                                title = "Warning",
                                tags$b("Only CSV files are accepted!"),
                                size = 's'
                             ))
                          }else{
                             ## read the grouping
                             hs_group <-  read.csv(input$file_comodity_ex$datapath, row.names = NULL) 
                                                   
                             ## check if the first column is HS code
                             tmp_hs_c1 <- gsub("[`]", "", hs_group[,1])
                             if( ncol(hs_group) >2 ){
                                showModal(modalDialog(
                                   title = "Warning",
                                   tags$p("Please check your uploaded HS groupings and make sure", 
                                          tags$b("it contains TWO columns only!")),
                                   size = 's'
                                ))
                             }else if( any( is.na( as.numeric(tmp_hs_c1) )  ) ){
                                showModal(modalDialog(
                                   title = "Warning",
                                   tags$p("Please check your uploaded HS groupings and make sure", 
                                           tags$b("the first column is HS codes!")),
                                   size = 's'
                                ))
                             }else if( any( nchar(tmp_hs_c1) > 6 ) ){
                                showModal(modalDialog(
                                   title = "Warning",
                                   tags$p("Please check your uploaded HS groupings and make sure", 
                                          tags$b("all HS codes are within level 6!") ),
                                   size = 's'
                                ))
                             }else{
                                ### can rn self define
                                tmp_execution_self_define <- TRUE
                             }
                             
                             ## 1.3.1 Build graphs self-defined commodity --------------
                             if(tmp_execution_self_define){
                                ## --- hide howto -----
                                shinyjs::hide(id = 'ci_howto_ex')
                                ## show waite message ----
                                shinyjs::show( id = 'wait_message_ci_ex' )
                                ## disable the buttone ---
                                shinyjs::disable("btn_build_commodity_report_ex")
                                ## disable the upload button ---
                                shinyjs::disable("file_comodity_ex")
                                shinyjs::disable("rbtn_prebuilt_diy_ex")
                                
                                ## now To build report if checks are all good ----------------
                                ## make sure the HS codes become characters and HS 1 has 01 format
                                ## standerdise column names
                                colnames(hs_group) <- c("HS_code", "HS_group")
                                ## make columns characters and make sure HS code has 01, and 0122 etc format
                                hs_group %<>%
                                   mutate_all( funs(as.character) ) %>%
                                   mutate( HS_code = gsub("[`]","",HS_code) ) %>%
                                   mutate( HS_code = if_else(nchar(HS_code)%in%c(1,3,5), paste0("0", HS_code), HS_code  )  )
                                
                                
                                ## 2.0.1 Self-define Build the main data.frame -- all selected commodity by country ------
                                tmp_dtf_shiny_full <-
                                   dtf_shiny_full %>%
                                   filter( Type_ie == 'Exports', 
                                           Commodity %in% hs_group$HS_code ) %>%
                                   left_join( hs_group, by = c('Commodity' = 'HS_code') ) %>%
                                   left_join( concord_country_iso_latlon_raw, by = 'Country' ) %>%
                                   group_by( Year, Country, Type_ie, Type_gs, HS_group, ISO2, lat, lon, Note ) %>%
                                   summarise( Value = sum(Value, na.rm=T) ) %>%
                                   ungroup
                                
                                #output$test_full_shiny <- renderDataTable(tmp_dtf_shiny_full)
                                
                                ## commodity only -- sum all countires
                                tmp_dtf_shiny_full_commodity_only <-
                                   tmp_dtf_shiny_full %>%
                                   group_by( Year,  Type_ie, Type_gs, HS_group, Note ) %>%
                                   summarise( Value = sum(Value, na.rm=T) ) %>%
                                   ungroup %>%
                                   mutate( Country = 'World' )
                                
                                ## 2.1 Self-defined: Build export value line chart -------------------- 
                                tmp_top_g_ex <-
                                   tmp_dtf_shiny_full_commodity_only %>%
                                   filter( Year == max(Year)) %>% 
                                   arrange( -Value ) %>%
                                   dplyr::select( HS_group ) %>%
                                   as.matrix() %>%
                                   as.character
                                
                                
                                ## top selected commodities and top 5services
                                tmp_top_ex <- c( tmp_top_g_ex) #, tmp_top_s_ex)
                                
                                ## data frame to plot
                                tmp_dtf_key_line_ex <- 
                                   tmp_dtf_shiny_full_commodity_only%>%
                                   filter( HS_group %in% tmp_top_ex,
                                           Year >=2007) %>%
                                   mutate( Value = round(Value/10^6),
                                           HS_group = factor(HS_group, levels = tmp_top_ex)
                                   ) %>%
                                   arrange( HS_group )
                                
                                ### plot
                                output$CIExportValueLine <- 
                                   renderHighchart(
                                      highchart() %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_xAxis( categories = c( unique( tmp_dtf_key_line_ex$Year) ) ) %>%
                                         hc_yAxis( title = list(text = "$ million, NZD"),
                                                   labels = list( format = "${value:,.0f} m")  ) %>%
                                         hc_plotOptions(line = list(
                                            dataLabels = list(enabled = F),
                                            #stacking = "normal",
                                            enableMouseTracking = T #,
                                            #series = list(events = list(legendItemClick = sharelegend)) ,
                                            #showInLegend = T
                                         )
                                         )%>%
                                         hc_tooltip(table = TRUE,
                                                    sort = TRUE,
                                                    pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                          " {series.name}: ${point.y} m"),
                                                    headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                         ) %>%
                                         hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                         hc_add_series( data =  tmp_dtf_key_line_ex %>% filter( Type_gs == 'Goods' ) ,
                                                        mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                                                        type = 'line',
                                                        marker = list(symbol = 'circle') #,
                                                        #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                         )
                                   )
                                
                                ## 2.2 Self-defined: build export as a percent of total export line chart -----------------------
                                tmp_tot_ex <-
                                   dtf_shiny_full %>%
                                   filter( Country == 'World',
                                           Type_ie == 'Exports',
                                           Year >= 2007 )  %>%
                                   mutate( Value = round(Value/10^6) ) %>%
                                   group_by( Year, Country, Type_ie ) %>%
                                   summarize( Value = sum(Value, na.rm=T) ) %>%
                                   ungroup %>%
                                   mutate( HS_group = 'Total exports' )
                                
                                tmp_dtf_percent_line_ex <-
                                   tmp_dtf_key_line_ex %>%
                                   bind_rows( tmp_tot_ex ) %>%
                                   group_by( Year, Country, Type_ie ) %>%
                                   mutate( Share = Value/Value[HS_group=='Total exports'],
                                           Value = Share*100 ) %>%
                                   ungroup %>%
                                   filter( HS_group != 'Total exports' ) %>%
                                   mutate( HS_group = factor(HS_group, levels = tmp_top_ex) ) %>%
                                   arrange( HS_group )
                                
                                # ### plot
                                output$CIExportPercentLine <-
                                   renderHighchart(
                                      highchart() %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_xAxis( categories = c( unique( tmp_dtf_percent_line_ex$Year) ) ) %>%
                                         hc_yAxis( title = list(text = "Percentage (%)"),
                                                   labels = list( format = "{value:,.1f} %")  ) %>%
                                         hc_plotOptions(line = list(
                                            dataLabels = list(enabled = F),
                                            #stacking = "normal",
                                            enableMouseTracking = T)
                                         )%>%
                                         hc_tooltip(table = TRUE,
                                                    sort = TRUE,
                                                    pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                          " {series.name}: {point.y:,.1f} %"),
                                                    headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                         ) %>%
                                         hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                         hc_add_series( data =  tmp_dtf_percent_line_ex %>% filter( Type_gs == 'Goods' ) ,
                                                        mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                                                        type = 'line',
                                                        marker = list(symbol = 'circle') #,
                                                        #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                         )
                                   )
                                
                                ## !!!!! try UI insert ONLY when not may commodities selected ----------- 
                                if( length(unique(hs_group$HS_group)) < 111 ){
                                   insertUI(
                                      selector = '#body_ex_self_defined',
                                      ui =   div( id = 'body_ex_line_value_percent_self_defined',
                                                  fluidRow( h1("Exports for selected commodities/services"),
                                                            p("Click on the commodity or service names in the legend area to show their trends"),
                                                            column(6, div(id = "body_value_ex", h4("Export values"), highchartOutput('CIExportValueLine') ) ),
                                                            column(6, div(id = "body_percent_ex", h4("As a percent of total exports"), highchartOutput('CIExportPercentLine') ) ))
                                      )
                                   )
                                }
                                ## end Try UI insert --------##
                                
                                ## 2.3 Self-defined: build export value change table ----------------
                                ## data frame to plot
                                tmp_dtf_key_tab_ex <- 
                                   tmp_dtf_shiny_full_commodity_only %>%
                                   filter( HS_group %in% tmp_top_ex) %>%
                                   mutate( HS_group = factor(HS_group, levels = tmp_top_ex) ) %>%
                                   arrange( HS_group )
                                
                                tmp_tab <-
                                   tmp_dtf_key_tab_ex %>%
                                   mutate( Name =  HS_group ) %>%
                                   group_by( Name) %>%
                                   do(CAGR1 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                       .$Value[.$Year == (max(.$Year)-1)], 1)/100,
                                      CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                       .$Value[.$Year == (max(.$Year)-5)], 5)/100,
                                      CAGR10 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                        .$Value[.$Year == (max(.$Year)-10)], 10)/100 ,
                                      ABS5 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)-5)],
                                      ABS10 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)-10)]
                                      ) %>%
                                   ungroup %>%
                                   mutate( CAGR1 = as.numeric(CAGR1),
                                           CAGR5 = as.numeric(CAGR5),
                                           CAGR10 = as.numeric(CAGR10),
                                           ABS5 = as.numeric(ABS5),
                                           ABS10 = as.numeric(ABS10)
                                           ) %>%
                                   left_join( tmp_dtf_key_tab_ex , 
                                              by =c('Name'='HS_group') ) %>%
                                   left_join( tmp_dtf_percent_line_ex %>% dplyr::select( -Value) %>% rename(Name = HS_group) ) %>%
                                   filter( Year == max(Year) ) %>%
                                   mutate( Value = Value/10^6, ABS5 = ABS5/10^6, ABS10 = ABS10/10^6 ) %>%
                                   dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10) %>%
                                   mutate( Name = factor(Name, levels = tmp_top_ex),
                                           CAGR1 = ifelse(CAGR1 %in% c(Inf,-Inf), NA, CAGR1),
                                           CAGR5 = ifelse(CAGR5 %in% c(Inf,-Inf), NA, CAGR5),
                                           CAGR10 = ifelse(CAGR10 %in% c(Inf,-Inf), NA, CAGR10)
                                   ) %>%
                                   arrange( Name )
                                
                                
                                ### join back to hs code
                                hs_group_flat <- 
                                   hs_group %>%
                                   group_by( HS_group ) %>%
                                   summarise( HS_code = paste0(HS_code, collapse = '; ') ) %>%
                                   ungroup
                                
                                tmp_tab %<>%
                                   left_join( hs_group_flat, by = c("Name"= 'HS_group') ) %>%
                                   dplyr::select( HS_code, Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10 )
                                
                                output$GrowthTabSelectedEx <- renderDataTable(
                                   datatable( tmp_tab,
                                              rownames = F,
                                              filter = c("top"),
                                              extensions = c('Buttons'
                                                             #, 'FixedColumns'
                                              ),
                                              options = list(dom = 'Bfltp', #'Bltp',# 'Bt',
                                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1, 
                                                             ,scrollX = TRUE
                                                             #,fixedColumns = list(leftColumns = 2) 
                                                             ,autoWidth = T
                                                             ,pageLength = 10
                                                             ,lengthMenu = list(c(10,  -1), list('10', 'All')) ,
                                                             searchHighlight = TRUE,
                                                             search = list(regex = TRUE, caseInsensitive = FALSE )
                                                             
                                              ) ,
                                              colnames = c("HS codes", "Classification","Value ($m)", "Share of total exports", 'CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10')
                                   ) %>%
                                      formatStyle(
                                         c('CAGR1', 'CAGR5', 'CAGR10'),
                                         background = styleColorBar( c(0, max(c(tmp_tab$CAGR1,tmp_tab$CAGR5, tmp_tab$CAGR10))*2, na.rm=T) , 'lightblue'),
                                         backgroundSize = '100% 90%',
                                         backgroundRepeat = 'no-repeat',
                                         backgroundPosition = 'center'
                                      ) %>%
                                      formatStyle(c('CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'),
                                                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>%
                                      formatPercentage( c('Share','CAGR1', 'CAGR5', 'CAGR10'),digit = 1 ) %>%
                                      formatStyle( columns = c('Name','Value', 'Share', 'CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'), `font-size`= '115%' ) %>%
                                      formatCurrency( columns = c('Value', 'ABS5', 'ABS10'), mark = ' ', digits = 1)
                                )
                                
                                ## !!!!! try UI insert ----------- 
                                insertUI(
                                   selector = '#body_growth_ex_self_defined',
                                   ui =   div( id = 'body_ex_growth_tab_self_defined',
                                               fluidRow( h1("Short, medium, and long term growth for selected commodities/services"),
                                                         p("Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years."),
                                                         dataTableOutput('GrowthTabSelectedEx')
                                               )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                ## 2.4 Self-defined: Build export by country output groups -------------------
                                ## create a selector for each selected commodity -----------------
                                output$CIEXSelectorByMarkets <- renderUI({
                                   selectizeInput("select_comodity_ex_for_market_analysis",
                                                   tags$p("Please select or search a commodity for its market analysis"), 
                                                  # choices = tmp_tab$Name[input$GrowthTabSelectedEx_rows_all], # tmp_top_ex, 
                                                  # selected = NULL, #tmp_top_ex[1], 
                                                  # width = "500px",
                                                  # multiple = F,
                                                  # options = list(
                                                  #    placeholder = 'Please select a commodity',
                                                  #    onInitialize = I('function() { this.setValue(""); }')
                                                  # ) 
                                                  choices =  c('Please select a commodity' = "" , 
                                                               tmp_tab$Name[input$GrowthTabSelectedEx_rows_all]), #input$select_comodity_ex,
                                                  selected = NULL,  width = "500px",
                                                  multiple = F #,
                                                  # options = list(
                                                  #    placeholder = 'Please select a commodity',
                                                  #    onInitialize = I('function() { this.setValue(" "); }')
                                                  #             ) 
                                                  )
                                })
                                
                                ### build data for market analysis -- these has to be reactive values
                                ## The name of the selected commodity
                                tmp_selected_ex <- 
                                   reactive({
                                      input$select_comodity_ex_for_market_analysis
                                   })
                                
                                ## The HS codes of the selected commodity
                                tmp_hs_ex <- 
                                   reactive({
                                      hs_group$HS_code[hs_group$HS_group == tmp_selected_ex()]
                                   })
                                
                                ## The data from of the selected commodity by markets
                                tmp_dtf_market_ex <- 
                                   reactive({
                                      dtf_shiny_full %>%
                                         filter( Commodity %in% tmp_hs_ex(), 
                                                 Year >= 2007,
                                                 Type_ie == 'Exports') %>%
                                         left_join( concord_country_iso_latlon_raw, by = 'Country' ) %>%
                                         left_join( hs_group, by = c('Commodity' = 'HS_code') ) %>%
                                         group_by( Year, Country, Type_ie, Type_gs, Note, ISO2, lat, lon ) %>%
                                         summarize( Value = sum(Value, na.rm=T) ) %>%
                                         ungroup %>%
                                         mutate( Commodity = as.character( tmp_selected_ex() ) )
                                   })
                                
                                ### selcted commodity and service outputs
                                output$SelectedEx <- 
                                   renderText({
                                      tmp_selected_ex()
                                   })
                                
                                ## !!!!! try UI insert ---------------
                                insertUI(
                                   selector = '#body_ci_markets_ex_self_defined',
                                   ui =   div( id = 'body_ci_markets_ex_selector_self_defined',
                                               fluidRow(h1("Export markets analysis for selected commodity/service"),
                                                        uiOutput("CIEXSelectorByMarkets") ),
                                               fluidRow( shiny::span(h1( HTML(paste0(textOutput("SelectedEx"))), align = "center" ), style = "color:darkblue" ) )
                                   )
                                )
                                ## end Try UI insert -----------## 
                                
                                ## --- show loading message ------------------
                                observe({
                                   if( any(input$select_comodity_ex_for_market_analysis %in% tmp_tab$Name)  ){
                                      shinyjs::show( id = "body_ci_market_loading_message_self_define" )
                                   }
                                })
                                ## finish
                                
                                ### 2.4.0 Value Line and Percentage line for selected commodities ----------------
                                tmp_dtf_line_selected_ex <-
                                   reactive({
                                      tmp_dtf_key_line_ex %>%
                                         filter( HS_group %in% as.character( tmp_selected_ex() ) )
                                   })
                                
                                ### plot
                                output$CISelectedExportValueLine <- 
                                   renderHighchart({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      
                                      highchart() %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_xAxis( categories = c( unique( tmp_dtf_line_selected_ex()$Year) ) ) %>%
                                         hc_yAxis( title = list(text = "$ million, NZD"),
                                                   labels = list( format = "${value:,.0f} m")  ) %>%
                                         hc_plotOptions(line = list(
                                            dataLabels = list(enabled = F),
                                            #stacking = "normal",
                                            enableMouseTracking = T #,
                                            #series = list(events = list(legendItemClick = sharelegend)) ,
                                            #showInLegend = T
                                         )
                                         )%>%
                                         hc_tooltip(table = TRUE,
                                                    sort = TRUE,
                                                    pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                          " {series.name}: ${point.y} m"),
                                                    headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                         ) %>%
                                         hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                         hc_add_series( data =  tmp_dtf_line_selected_ex() %>% filter( Type_gs == 'Goods' ) ,
                                                        mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                                                        type = 'line',
                                                        marker = list(symbol = 'circle') #,
                                                        #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                         )
                                   })
                                
                                ## percentage line
                                tmp_dtf_percent_selected_line_ex <-
                                   reactive({
                                      tmp_dtf_percent_line_ex %>%
                                         filter( HS_group %in% as.character( tmp_selected_ex() ) )
                                   })
                                
                                # ### plot
                                output$CISelectedExportPercentLine <-
                                   renderHighchart({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      
                                      highchart() %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_xAxis( categories = c( unique( tmp_dtf_percent_selected_line_ex()$Year) ) ) %>%
                                         hc_yAxis( title = list(text = "Percentage (%)"),
                                                   labels = list( format = "{value:,.1f} %")  ) %>%
                                         hc_plotOptions(line = list(
                                            dataLabels = list(enabled = F),
                                            #stacking = "normal",
                                            enableMouseTracking = T)
                                         )%>%
                                         hc_tooltip(table = TRUE,
                                                    sort = TRUE,
                                                    pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                          " {series.name}: {point.y:,.1f} %"),
                                                    headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                         ) %>%
                                         hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                         hc_add_series( data =  tmp_dtf_percent_selected_line_ex() %>% filter( Type_gs == 'Goods' ) ,
                                                        mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                                                        type = 'line',
                                                        marker = list(symbol = 'circle') #,
                                                        #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                         )
                                   })
                                
                                ## !!!!! try UI insert ----------- 
                                output$H1_export_trend_self_define_title <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("Exports trend")
                                   })
                                
                                output$H1_export_trend_self_define_title_note <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("Click on the commodity or service names in the legend area to show their trends")
                                   })
                                
                                output$H4_export_trend_self_define_value_title <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("Export values")
                                   })
                                
                                output$H4_export_trend_self_define_percent_title <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("As a percent of total exports")
                                   })
                                
                                ## insert ui here
                                insertUI(
                                   selector = '#body_ci_markets_ex_self_defined',
                                   ui =   div( id = 'body_selected_ex_line_value_percent_self_defined',
                                               fluidRow( h1( HTML(paste0(textOutput("H1_export_trend_self_define_title"))) ),
                                                         p( HTML(paste0(textOutput("H1_export_trend_self_define_title_note"))) ),
                                                         column(6, div(id = "body_value_selected_ex", 
                                                                       h4( HTML(paste0(textOutput("H4_export_trend_self_define_value_title"))) ), 
                                                                       highchartOutput('CISelectedExportValueLine') ) ),
                                                         column(6, div(id = "body_percent_selected_ex", 
                                                                       h4( HTML(paste0(textOutput("H4_export_trend_self_define_percent_title"))) ),
                                                                       highchartOutput('CISelectedExportPercentLine') ) ))
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                ### 2.4.1 Self-defined: build highchart map  ---------------------------
                                print("--------- Building highchart map -------------")
                                tmp_dtf_market_ex_map <- 
                                   reactive({
                                      tmp_dtf_market_ex() %>%
                                         filter( Year == max(Year),
                                                 !is.na(lat) ) %>%
                                         mutate( Value = Value/10^6,
                                                 z= Value,
                                                 name = Country)
                                   })
                                
                                ## plot map
                                output$MapEXMarket <- 
                                   renderHighchart({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      
                                      hcmap( data = tmp_dtf_market_ex_map() ,
                                             value = 'Value',
                                             joinBy = c('iso-a2','ISO2'), 
                                             name="Exports value",
                                             borderWidth = 1,
                                             borderColor = "#fafafa",
                                             nullColor = "lightgrey",
                                             tooltip = list( table = TRUE,
                                                             sort = TRUE,
                                                             headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                             pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
                                      ) %>%
                                         hc_add_series(data =  tmp_dtf_market_ex_map(),
                                                       type = "mapbubble",
                                                       color  = hex_to_rgba("#f1c40f", 0.9),
                                                       minSize = 0,
                                                       name="Exports value",
                                                       maxSize = 30,
                                                       tooltip = list(table = TRUE,
                                                                      sort = TRUE,
                                                                      headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                                      pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
                                         ) %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_legend( enabled=FALSE ) %>% 
                                         hc_mapNavigation(enabled = TRUE) 
                                   })
                                
                                ## !!!!! try UI insert ----------- 
                                output$H2_map_of_export_title <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("Map of export values")
                                   })
                                
                                output$H2_map_of_export_title_note <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("The size of bubble area and color both represent the value of exports.")
                                   })
                                
                                ## inserter UI here  
                                insertUI(
                                   selector = '#body_ci_markets_ex_self_defined',
                                   ui =   div( id = 'body_ci_markets_ex_map_self_defined',
                                               fluidRow(h2( HTML(paste0(textOutput("H2_map_of_export_title")))  ) ,
                                                        p( HTML(paste0(textOutput("H2_map_of_export_title_note")))  ),
                                                        highchartOutput('MapEXMarket') )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                ### 2.4.2 Self-defined: Top markets for selected commodity line chart ----------------
                                print("--------- Building Top market line chart -------------")
                                tmp_top_country_selected_ex <- 
                                   reactive({
                                      tmp_dtf_market_ex() %>%
                                         filter( Year == max(Year),
                                                 Value > 0 , 
                                                 !Country %in% c("World", 
                                                                 "Destination Unknown - EU")
                                         ) %>% ## 1 bn commodity
                                         arrange( -Value ) %>%
                                         dplyr::select( Country ) %>%
                                         as.matrix() %>%
                                         as.character
                                   })
                                
                                ### only show top 10 countries 
                                tmp_top10_country_selected_ex <-
                                   reactive({
                                      tmp_top_country_selected_ex()[1:min(10,length(tmp_top_country_selected_ex()))]
                                   })
                                
                                ## test the see top countries
                                # output$test_top_country_ex <- 
                                #    renderText({
                                #       tmp_top_country_selected_ex()
                                #    })
                                ### derive datafrom for the line plot
                                tmp_dtf_market_ex_line <- 
                                   reactive({
                                      tmp_dtf_market_ex() %>%
                                         filter( Country %in%  as.character(tmp_top_country_selected_ex()) ) %>%
                                         mutate( Value = Value/10^6 ,
                                                 Country = factor(Country, levels = as.character(tmp_top_country_selected_ex()) )
                                         ) %>%
                                         arrange(Country)
                                   })
                                
                                ## test the see top countries
                                # output$test_top_country_ex_dtf <- 
                                #    renderDataTable({
                                #       tmp_dtf_market_ex_line()
                                #    })
                                
                                ## line plot
                                output$SelectedExMarketLine <- renderHighchart({
                                   if( input$select_comodity_ex_for_market_analysis == "" ) 
                                      return(NULL)
                                   
                                   highchart() %>%
                                      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                      hc_add_series( data =  tmp_dtf_market_ex_line() %>%
                                                        filter( Country %in% as.character(tmp_top10_country_selected_ex()) ),
                                                     mapping = hcaes(  x = Year, y = Value, group = Country),
                                                     type = 'line',
                                                     marker = list(symbol = 'circle'), 
                                                     visible = c( rep(T,5), rep(F,length( as.character(tmp_top10_country_selected_ex()) )-5) )
                                      ) %>%
                                      hc_xAxis( categories = c( unique( tmp_dtf_market_ex_line()$Year) ) ) %>%
                                      hc_yAxis( title = list(text = "$ million, NZD"),
                                                labels = list( format = "${value:,.0f} m")  ) %>%
                                      hc_plotOptions(line = list(
                                         dataLabels = list(enabled = F),
                                         #stacking = "normal",
                                         enableMouseTracking = T)
                                      )%>%
                                      hc_tooltip(table = TRUE,
                                                 sort = TRUE,
                                                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                       " {series.name}: ${point.y:,.0f} m"),
                                                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                      ) %>%
                                      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                                })
                                
                                
                                ### 2.4.3 Self-defined: Top markets for selected commodity percent line chart -------------------
                                print("--------- Building Top market line chart (Percent) -------------")
                                tmp_dtf_market_ex_line_percent <- 
                                   reactive({
                                      tmp_dtf_market_ex_line() %>%
                                         group_by(Year, Type_ie, Type_gs, Note, Commodity) %>%
                                         mutate( Share = Value/sum(Value, na.rm=T)) %>%
                                         ungroup %>%
                                         mutate( Value = Share*100 ) 
                                   })
                                
                                output$SelectedExMarketLinePercent <-
                                   renderHighchart({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      
                                      highchart() %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_add_series( data =  tmp_dtf_market_ex_line_percent() %>%
                                                           filter( Country %in% as.character(tmp_top10_country_selected_ex()) ),
                                                        mapping = hcaes(  x = Year, y = Value, group = Country),
                                                        type = 'line',
                                                        marker = list(symbol = 'circle'), 
                                                        visible = c( rep(T,5), rep(F,length( as.character(tmp_top10_country_selected_ex()) )-5) )
                                         ) %>%
                                         hc_xAxis( categories = c( unique( tmp_dtf_market_ex_line_percent()$Year) ) ) %>%
                                         hc_yAxis( title = list(text = "Percentage (%)"),
                                                   labels = list( format = "{value:,.1f} %")  ) %>%
                                         hc_plotOptions(line = list(
                                            dataLabels = list(enabled = F),
                                            #stacking = "normal",
                                            enableMouseTracking = T)
                                         )%>%
                                         hc_tooltip(table = TRUE,
                                                    sort = TRUE,
                                                    pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                          " {series.name}: {point.y:,.1f} %"),
                                                    headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                         ) %>%
                                         hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                                   })
                                
                                ## !!!!! try UI insert ----------- 
                                output$H2_export_market_trend_title <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("Top 10 export markets trends")
                                   })
                                
                                output$H2_export_market_trend_title_note <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("Click on the country names in the legend area to show their trends")
                                   })
                                
                                output$H4_export_market_trend_value_title <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("Export values")
                                   })
                                
                                output$H4_export_market_trend_percent_title <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("As a percent of total exports of the selected")
                                   })
                                
                                ## insert ui here
                                insertUI(
                                   selector = '#body_ci_markets_ex_self_defined',
                                   ui =   div( id = 'body_ci_markets_ex_top_self_defined',
                                               fluidRow( h2( HTML(paste0(textOutput("H2_export_market_trend_title"))) ),
                                                         p( HTML(paste0(textOutput("H2_export_market_trend_title_note"))) ),
                                                         column(6, 
                                                                h4( HTML(paste0(textOutput("H4_export_market_trend_value_title"))) ),
                                                                highchartOutput("SelectedExMarketLine") 
                                                         ),
                                                         column(6,
                                                                h4( HTML(paste0(textOutput("H4_export_market_trend_percent_title"))) ),
                                                                highchartOutput("SelectedExMarketLinePercent")
                                                         )
                                               )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                ### 2.4.4 Self-defined: Growth prospective tab ----------------------
                                print("--------- Building Grwoth prospective table -------------")
                                tmp_tab_ex_growth <-
                                   reactive({
                                      tmp_dtf_market_ex_line() %>%
                                         #filter( Country %in% as.character(tmp_top10_country_selected_ex()) ) %>%
                                         mutate( Name =  Country ) %>%
                                         group_by( Name) %>%
                                         do( CAGR1 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                              .$Value[.$Year == (max(.$Year)-1)], 1)/100,
                                             CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                              .$Value[.$Year == (max(.$Year)-5)], 5)/100,
                                             CAGR10 =  CAGR( .$Value[.$Year == max(.$Year)]/
                                                                .$Value[.$Year == (max(.$Year)-10)], 10)/100,
                                             ABS5 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)- 5)],
                                             ABS10 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)- 10)]
                                         ) %>%
                                         ungroup %>%
                                         mutate( CAGR1 = as.numeric(CAGR1), 
                                                 CAGR5 = as.numeric(CAGR5), 
                                                 CAGR10 = as.numeric(CAGR10),
                                                 ABS5 = as.numeric(ABS5),
                                                 ABS10 = as.numeric(ABS10) ) %>%
                                         #filter( Year == max(Year) ) %>%
                                         left_join( tmp_dtf_market_ex_line() %>% rename(Name = Country) %>% filter( Year == max(Year) )  ) %>%
                                         left_join( tmp_dtf_market_ex_line_percent() %>% dplyr::select( -Value ) %>% rename( Name = Country) %>% filter( Year == max(Year) )  ) %>%
                                         dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10) %>%
                                         mutate( Name = factor(Name, levels = as.character(tmp_top_country_selected_ex()) ) ) %>%
                                         arrange( Name )
                                   })
                                
                                output$SelectedExMarketGrowthTab <- renderDataTable({
                                   if( input$select_comodity_ex_for_market_analysis == "" ) 
                                      return(NULL)
                                   
                                   datatable( tmp_tab_ex_growth(),
                                              rownames = F,
                                              extensions = 'Buttons',
                                              options = list(dom = 'Bltp',# 'Bt', 
                                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1
                                                             ,scrollX = TRUE
                                                             ,pageLength = 10
                                                             ,lengthMenu = list(c(10,  -1), list('10', 'All')) 
                                                             #,fixedColumns = list(leftColumns = 2) 
                                                             #,autoWidth = T
                                              ) ,
                                              colnames=c("Markets", "Value ($m)", "Share of world marekt", 'CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10')
                                   ) %>%
                                      formatStyle(
                                         c('CAGR1', 'CAGR5', 'CAGR10'),
                                         background = styleColorBar( c(0, max(c(tmp_tab_ex_growth()$CAGR1,
                                                                                tmp_tab_ex_growth()$CAGR5,
                                                                                tmp_tab_ex_growth()$CAGR10))*2, na.rm=T) , 'lightblue'),
                                         backgroundSize = '100% 90%',
                                         backgroundRepeat = 'no-repeat',
                                         backgroundPosition = 'center'
                                      ) %>%
                                      formatStyle(c('CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'),
                                                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>%
                                      formatPercentage( c('Share', 'CAGR1', 'CAGR5', 'CAGR10'),digit = 1 ) %>%
                                      formatStyle( columns = c('Name', "Value", "Share" ,'CAGR1', 'CAGR5', 'CAGR10'), `font-size`= '115%' ) %>%
                                      formatCurrency( columns = c("Value", 'ABS5', 'ABS10'), mark = ' ', digits = 1)
                                })
                                
                                
                                ## !!!!! try UI insert ----------- 
                                output$H2_market_ex_growth_tab_title <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("Top export markets growth prospective")
                                   })
                                
                                output$H2_market_ex_growth_tab_title_note <-
                                   renderText({
                                      if( input$select_comodity_ex_for_market_analysis == "" ) 
                                         return(NULL)
                                      paste0("Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years.")
                                   })
                                
                                ## insert ui here
                                insertUI(
                                   selector = '#body_ci_markets_ex_self_defined',
                                   ui =   div( id = 'body_ci_markets_ex_growth_self_defined',
                                               fluidRow( h2( HTML(paste0(textOutput("H2_market_ex_growth_tab_title"))) ),
                                                         p( HTML(paste0(textOutput("H2_market_ex_growth_tab_title_note"))) ),
                                                         dataTableOutput("SelectedExMarketGrowthTab")
                                               )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                
                                ## 2.5 Self-defined: show HS groupings in appendix -------------------
                                # output$HS_ex <- renderDataTable( hs_group,rownames = FALSE, 
                                #                                  extensions = 'Buttons',
                                #                                  options = list(dom = 'Bltp', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                #                                                 pageLength = 5,
                                #                                                 lengthMenu = list(c(5,  -1), list('5', 'All')) 
                                #                                  ) 
                                # )
                                ## !!!!! try UI insert ----------- 
                                # insertUI(
                                #    selector = '#body_ci_markets_ex_self_defined',
                                #    ui =   div( id = 'body_appendix_hs_ex_self_defined',
                                #                conditionalPanel("input.rbtn_prebuilt_diy_ex == 'Pre-defined'",
                                #                                 fluidRow( tags$h1("Appendix -- HS grouping selected"),
                                #                                           div(id = 'output_hs_pre_ex', dataTableOutput( ("HS_pre_ex") ) )
                                #                                 )
                                #                ),
                                #                
                                #                conditionalPanel( "input.rbtn_prebuilt_diy_ex == 'Self-defined'",
                                #                                  fluidRow( tags$h1("Appendix -- HS grouping uploaded"),
                                #                                            div(id = 'output_hs_ex', dataTableOutput( ("HS_ex") ) )
                                #                                  )
                                #                                  
                                #                )
                                #    )
                                # )
                                ## end Try UI insert --------##
                                ## 2.6 Data for global situation from UN comtrade (ONLY for Export analysis) ----------------
                                print("--------- Building Reactive values for global analysis -------------")
                                rv_self_define_ex <- reactiveValues()
                                
                                ## put reactive values into observe  ------
                                observe({
                                   ## get data from un com trade using loop
                                   ## create a list first
                                   print("----------- Download Uncomtrade trade by country --------------")
                                   rv_self_define_ex$Fail_uncomtrade_country <- 
                                   try(
                                      rv_self_define_ex$tmp_global_by_country_raw_list <- 
                                         lapply( tmp_hs_ex() ,
                                                 function(i){
                                                    m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                                                 commod_codes = i,
                                                                 start_date = tmp_un_comtrade_max_year ,
                                                                 end_date = tmp_un_comtrade_max_year )  %>%
                                                       bind_rows(  m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                                                               commod_codes = i,
                                                                               start_date = tmp_un_comtrade_max_year - 5,
                                                                               end_date = tmp_un_comtrade_max_year - 5 ) 
                                                       )
                                                 } 
                                         )
                                   )
                                   
                                   ## try get EU data
                                   print("----------- Download Uncomtrade trade by EU --------------")
                                   rv_self_define_ex$Fail_uncomtrade_eu <- 
                                   try(
                                      rv_self_define_ex$tmp_global_by_eu_raw_list <- 
                                         lapply( tmp_hs_ex() ,
                                                 function(i){
                                                     m_ct_search( reporters = "EU-28", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                                                                commod_codes = i,
                                                                                start_date = tmp_un_comtrade_max_year,
                                                                                end_date = tmp_un_comtrade_max_year )  %>%
                                                       bind_rows(  m_ct_search( reporters = "EU-28", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                                                                              commod_codes = i,
                                                                                              start_date = tmp_un_comtrade_max_year - 5,
                                                                                              end_date = tmp_un_comtrade_max_year - 5 ) 
                                                       )
                                                 } 
                                         )
                                   )
                                   
                                   ## then consolidate the list into dataframe
                                   if( class(rv_self_define_ex$Fail_uncomtrade_country) != 'try-error' ){
                                      print("----------- Success: Download Uncomtrade trade by country --------------")
                                      ## get list to data frame
                                      try(
                                      rv_self_define_ex$tmp_global_by_country_raw1 <- 
                                         do.call( rbind, rv_self_define_ex$tmp_global_by_country_raw_list )
                                      )
                                      
                                      ## change names
                                      try(
                                      rv_self_define_ex$tmp_global_by_country_raw <-
                                         rv_self_define_ex$tmp_global_by_country_raw1 %>%
                                         dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
                                         rename( Year = year,
                                                 `Commodity.Code` = commodity_code ,
                                                 `Trade.Flow` = trade_flow,
                                                 Reporter = reporter,
                                                 `Reporter.ISO` =  reporter_iso,
                                                 Partner = partner,
                                                 `Qty.Unit` = qty_unit,
                                                 `Alt.Qty.Unit` = qty,
                                                 `Trade.Value..US..` = trade_value_usd )
                                      )
                                   }
                                   
                                   # try( 
                                   #    rv_self_define_ex$tmp_global_by_country_raw1 <- 
                                   #       do.call( rbind, rv_self_define_ex$tmp_global_by_country_raw_list )
                                   # )
                                   
                                   
                                   if( class(rv_self_define_ex$Fail_uncomtrade_eu) != 'try-error' ){
                                      print("----------- Success: Download Uncomtrade trade by EU --------------")
                                      ## get list to data frame
                                      try(
                                      rv_self_define_ex$tmp_global_by_eu_raw1 <- 
                                         do.call( rbind, rv_self_define_ex$tmp_global_by_eu_raw_list )
                                      )
                                      
                                      ## change names
                                      try(
                                      rv_self_define_ex$tmp_global_by_eu_raw <-
                                         rv_self_define_ex$tmp_global_by_eu_raw1 %>%
                                         dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
                                         rename( Year = year,
                                                 `Commodity.Code` = commodity_code ,
                                                 `Trade.Flow` = trade_flow,
                                                 Reporter = reporter,
                                                 `Reporter.ISO` =  reporter_iso,
                                                 Partner = partner,
                                                 `Qty.Unit` = qty_unit,
                                                 `Alt.Qty.Unit` = qty,
                                                 `Trade.Value..US..` = trade_value_usd )
                                      )
                                   }
                                   
                                   # try( 
                                   #    rv_self_define_ex$tmp_global_by_eu_raw1 <- 
                                   #       do.call( rbind, rv_self_define_ex$tmp_global_by_eu_raw_list )
                                   # )   
                                   
                                   ### get data from un com trade -----
                                   # rv_self_define_ex$Fail_uncomtrade <- 
                                   #    try(
                                   #       rv_self_define_ex$tmp_global_by_country_raw <-
                                   #          rv_self_define_ex$tmp_global_by_country_raw1 %>%
                                   #          #get.Comtrade(r="all", p="0", rg = "1,2"  ## 1 means imports; 2 means exports (3 is re-exports excluded here)
                                   #          #            , ps = paste0(tmp_un_comtrade_max_year, "," ,tmp_un_comtrade_max_year-5)
                                   #          #            , cc = paste0(tmp_hs_ex(), collapse = ','), fmt = 'csv' )$data #%>%
                                   #          # dplyr::select( yr, cmdCode, rgDesc, rtTitle, rt3ISO, ptTitle, qtDesc,  TradeQuantity, TradeValue) %>%
                                   #          # mutate_all( as.character ) %>%
                                   #          # mutate( yr = as.numeric(yr),
                                   #          #         TradeQuantity = as.numeric( TradeQuantity ),
                                   #          #         TradeValue = as.numeric( TradeValue )
                                   #          #         ) %>%
                                   #          # rename( Year = yr, `Commodity.Code` = cmdCode ,
                                   #          #         `Trade.Flow` = rgDesc,
                                   #       #         Reporter = rtTitle,
                                   #       #         `Reporter.ISO` = rt3ISO,
                                   #       #         Partner = ptTitle,
                                   #       #         `Qty.Unit` = qtDesc,
                                   #       #         `Alt.Qty.Unit` = TradeQuantity,
                                   #       #         `Trade.Value..US..` = TradeValue )
                                   #       
                                   #       # m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                   #       #              commod_codes = as.character(tmp_hs_ex()),
                                   #       #              start_date = tmp_un_comtrade_max_year - 4,
                                   #       #              end_date = tmp_un_comtrade_max_year ) %>%
                                   #       #    bind_rows( m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                   #       #                            commod_codes = as.character(tmp_hs_ex()),
                                   #       #                            start_date = tmp_un_comtrade_max_year - 5,
                                   #       #                            end_date = tmp_un_comtrade_max_year - 5 )
                                   #       #               ) %>%
                                   #       #filter( year >= tmp_un_comtrade_max_year-5 &
                                   #       #          year <= tmp_un_comtrade_max_year ) %>%
                                   #       dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
                                   #          rename( Year = year, 
                                   #                  `Commodity.Code` = commodity_code ,
                                   #                  `Trade.Flow` = trade_flow,
                                   #                  Reporter = reporter,
                                   #                  `Reporter.ISO` =  reporter_iso,
                                   #                  Partner = partner,
                                   #                  `Qty.Unit` = qty_unit,
                                   #                  `Alt.Qty.Unit` = qty,
                                   #                  `Trade.Value..US..` = trade_value_usd )
                                   #       
                                   #    )
                                   
                                   
                                   ## Eu export to world data
                                   # rv_self_define_ex$Fail_uncomtrade_eu <- 
                                   #    try(
                                   #       rv_self_define_ex$tmp_global_by_eu_raw <-
                                   #          rv_self_define_ex$tmp_global_by_eu_raw1 %>%
                                   #          dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
                                   #          rename( Year = year,
                                   #                  `Commodity.Code` = commodity_code ,
                                   #                  `Trade.Flow` = trade_flow,
                                   #                  Reporter = reporter,
                                   #                  `Reporter.ISO` =  reporter_iso,
                                   #                  Partner = partner,
                                   #                  `Qty.Unit` = qty_unit,
                                   #                  `Alt.Qty.Unit` = qty,
                                   #                  `Trade.Value..US..` = trade_value_usd )
                                   #       
                                   #    )
                                   
                                   ## 
                                   if( class(rv_self_define_ex$Fail_uncomtrade_country) == "try-error" )
                                      print(rv_self_define_ex$Fail_uncomtrade_country)
                                   
                                   if( class(rv_self_define_ex$Fail_uncomtrade_eu) == "try-error" )
                                      print(rv_self_define_ex$Fail_uncomtrade_eu)
                                   
                                   ## when both data downloaded successfully then do -------
                                   if( class(rv_self_define_ex$Fail_uncomtrade_country) != "try-error" & 
                                       class(rv_self_define_ex$Fail_uncomtrade_eu) != "try-error" & 
                                       !is.null(rv_self_define_ex$tmp_global_by_country_raw)  ){
                                      ## 1. format the data -----
                                      
                                      ## global import and export of A commodity (sum over all HS code under this commodity) by country
                                      rv_self_define_ex$tmp_global_by_country <- 
                                         rv_self_define_ex$tmp_global_by_country_raw %>%
                                         dplyr::select( Year,`Commodity.Code` , `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`, `Alt.Qty.Unit`, `Trade.Value..US..`) %>%
                                         #group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
                                         group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner ) %>%
                                         summarise( `Alt.Qty.Unit` = sum(`Alt.Qty.Unit`, na.rm=T),
                                                    `Trade.Value..US..` = sum(`Trade.Value..US..`, na.rm=T) 
                                         ) %>%
                                         ungroup %>%
                                         mutate( Price = `Trade.Value..US..`/ `Alt.Qty.Unit`) 
                                      
                                      ## EU import and export of A commodity from world
                                      rv_self_define_ex$tmp_eu_trade_extra_raw <- 
                                         rv_self_define_ex$tmp_global_by_eu_raw %>%
                                         dplyr::select( Year,`Commodity.Code` , `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`, `Alt.Qty.Unit`, `Trade.Value..US..`) %>%
                                         #group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
                                         group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner ) %>%
                                         summarise( `Alt.Qty.Unit` = sum(`Alt.Qty.Unit`, na.rm=T),
                                                    `Trade.Value..US..` = sum(`Trade.Value..US..`, na.rm=T)
                                         ) %>%
                                         ungroup 
                                      
                                      ## 5 yr change in value and prices % and abs 
                                      rv_self_define_ex$tmp_global_by_country_change <-    
                                         rv_self_define_ex$tmp_global_by_country %>%
                                         #group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
                                         group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
                                         do( Value_per_change = CAGR(.$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year]/
                                                                        .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ,
                                             Value_abs_change = .$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year] - .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5] ,
                                             Price_per_change = CAGR(.$Price[.$Year==tmp_un_comtrade_max_year]/
                                                                        .$Price[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ) %>%
                                         ungroup %>%
                                         mutate( Value_per_change = as.numeric(Value_per_change ),
                                                 Value_abs_change = as.numeric(Value_abs_change ),
                                                 Price_per_change = as.numeric(Price_per_change )
                                         )
                                      
                                      ## data frame for producing highchart tables 
                                      rv_self_define_ex$tmp_global_by_country_all <- 
                                         rv_self_define_ex$tmp_global_by_country %>%
                                         filter( Year == tmp_un_comtrade_max_year ) %>%
                                         left_join( rv_self_define_ex$tmp_global_by_country_change ) %>%
                                         group_by( Year, Trade.Flow  ) %>%
                                         mutate( Share = as.numeric(`Trade.Value..US..`)/ sum(as.numeric(`Trade.Value..US..`), na.rm=T ) ) %>%
                                         ungroup %>%
                                         arrange( `Trade.Flow`, -`Trade.Value..US..`) 
                                      
                                      ## 1.1 formate data -- get Eu28 intra and extra trade for later use in table ------
                                      rv_self_define_ex$tmp_eu_trade_all <- 
                                         rv_self_define_ex$tmp_global_by_country %>%
                                         filter( Reporter.ISO %in% concord_eu28$ISO3 ) %>%
                                         #group_by( Year , `Trade.Flow`, Partner, `Qty.Unit` ) %>%
                                         group_by( Year , `Trade.Flow`, Partner) %>%
                                         summarise(  `Alt.Qty.Unit` = sum( as.numeric(`Alt.Qty.Unit`), na.rm=T ),
                                                     `Trade.Value..US..` = sum( as.numeric(`Trade.Value..US..`), na.rm=T ) ) %>%
                                         ungroup %>%
                                         mutate( Reporter = "EU-28", Reporter.ISO = 'EU2'   )
                                      
                                      ## derive EU trade intra
                                      rv_self_define_ex$tmp_eu_trade_intra_raw <-
                                         rv_self_define_ex$tmp_eu_trade_all %>%
                                         left_join( rv_self_define_ex$tmp_eu_trade_extra_raw,
                                                    #by = c("Year", "Trade.Flow","Reporter", "Reporter.ISO", "Partner","Qty.Unit" )
                                                    by = c("Year", "Trade.Flow","Reporter", "Reporter.ISO", "Partner" )
                                         ) %>%
                                         mutate( `Alt.Qty.Unit` = Alt.Qty.Unit.x - Alt.Qty.Unit.y, 
                                                 `Trade.Value..US..` =  `Trade.Value..US...x` - `Trade.Value..US...y` ) %>%
                                         dplyr::select( -Alt.Qty.Unit.x, -Alt.Qty.Unit.y, 
                                                        -`Trade.Value..US...x`,  -`Trade.Value..US...y`) #%>%
                                      #mutate( Partner = "EU-28") 
                                      
                                      ### formate data
                                      rv_self_define_ex$tmp_eu_trade_intra <- 
                                         rv_self_define_ex$tmp_eu_trade_intra_raw %>%
                                         mutate( Reporter = 'EU-28-Intra', Reporter.ISO = 'EU2-intra' )
                                      
                                      rv_self_define_ex$tmp_eu_trade_extra <- 
                                         rv_self_define_ex$tmp_eu_trade_extra_raw %>%
                                         mutate( Reporter = 'EU-28-Extra', Reporter.ISO = 'EU2-extra' )
                                      
                                      ## join EU intra and extra back
                                      rv_self_define_ex$tmp_global_by_country_and_eu <-
                                         rv_self_define_ex$tmp_global_by_country_raw %>%
                                         filter( !Reporter.ISO %in% concord_eu28$ISO3 ) %>%
                                         dplyr::select( Year,`Commodity.Code` , `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`, `Alt.Qty.Unit`, `Trade.Value..US..`) %>%
                                         #group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
                                         group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
                                         summarise( `Alt.Qty.Unit` = sum(`Alt.Qty.Unit`, na.rm=T),
                                                    `Trade.Value..US..` = sum(`Trade.Value..US..`, na.rm=T)
                                         ) %>%
                                         ungroup %>%
                                         bind_rows( rv_self_define_ex$tmp_eu_trade_intra ) %>%
                                         bind_rows( rv_self_define_ex$tmp_eu_trade_extra  ) %>%
                                         mutate( Price = `Trade.Value..US..`/ `Alt.Qty.Unit`)
                                      
                                      ## 5 yr change in value and prices % and abs 
                                      rv_self_define_ex$tmp_global_by_country_and_eu_change <-    
                                         rv_self_define_ex$tmp_global_by_country_and_eu %>%
                                         #group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
                                         group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
                                         do( Value_per_change = CAGR(.$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year]/
                                                                        .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ,
                                             Value_abs_change = .$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year] - .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5] ,
                                             Price_per_change = CAGR(.$Price[.$Year==tmp_un_comtrade_max_year]/
                                                                        .$Price[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ) %>%
                                         ungroup %>%
                                         mutate( Value_per_change = as.numeric(Value_per_change ),
                                                 Value_abs_change = as.numeric(Value_abs_change ),
                                                 Price_per_change = as.numeric(Price_per_change ) )
                                      
                                      ## data frame for producing highchart tables 
                                      rv_self_define_ex$tmp_global_by_country_and_eu_all <- 
                                         rv_self_define_ex$tmp_global_by_country_and_eu %>%
                                         filter( Year == tmp_un_comtrade_max_year ) %>%
                                         left_join( rv_self_define_ex$tmp_global_by_country_and_eu_change ) %>%
                                         group_by( Year, Trade.Flow  ) %>%
                                         mutate( Share = as.numeric(`Trade.Value..US..`)/ sum(as.numeric(`Trade.Value..US..`), na.rm=T ) ) %>%
                                         ungroup %>%
                                         arrange( `Trade.Flow`, -`Trade.Value..US..`) 
                                      
                                      
                                      ## 2. calculate values for later use ------------   
                                      ## Global market size -- value now
                                      rv_self_define_ex$tmp_global_size_value_now <- 
                                         rv_self_define_ex$tmp_global_by_country %>%
                                         group_by(Year, `Trade.Flow`,  Partner ) %>%
                                         summarise(`Trade.Value..US..` = sum(as.numeric(`Trade.Value..US..`), na.rm=T) ) %>%
                                         ungroup %>%
                                         filter( Year == tmp_un_comtrade_max_year,
                                                 `Trade.Flow` == 'Import') %>%
                                         dplyr::select( `Trade.Value..US..` ) %>%
                                         as.numeric()
                                      
                                      ## Global market size -- value 5 years ago
                                      rv_self_define_ex$tmp_global_size_value_pre <- 
                                         rv_self_define_ex$tmp_global_by_country %>%
                                         group_by(Year, `Trade.Flow`,  Partner ) %>%
                                         summarise(`Trade.Value..US..` = sum(as.numeric(`Trade.Value..US..`), na.rm=T) ) %>%
                                         ungroup %>%
                                         filter( Year == tmp_un_comtrade_max_year-5,
                                                 `Trade.Flow` == 'Import') %>%
                                         dplyr::select( `Trade.Value..US..` ) %>%
                                         as.numeric()
                                      
                                      ## Global market size -- value change %
                                      rv_self_define_ex$tmp_global_size_value_change <-
                                         CAGR( rv_self_define_ex$tmp_global_size_value_now/
                                                  rv_self_define_ex$tmp_global_size_value_pre, 5)/100
                                      
                                      ## Global market size -- value change abs
                                      rv_self_define_ex$tmp_global_size_value_change_abs <-
                                         rv_self_define_ex$tmp_global_size_value_now - rv_self_define_ex$tmp_global_size_value_pre 
                                      
                                      ## Top 3 importers share
                                      rv_self_define_ex$tmp_top3_importers_share <-
                                         rv_self_define_ex$tmp_global_by_country_all %>%
                                         filter( `Trade.Flow` == 'Import' ) %>%
                                         arrange( -Share ) %>%
                                         slice(1:3) %>%
                                         group_by(Year) %>%
                                         summarise( Share = sum(Share, na.rm=T) ) %>%
                                         ungroup %>%
                                         dplyr::select(Share) %>%
                                         as.numeric
                                      
                                      ## Top 10 importers share
                                      rv_self_define_ex$tmp_top10_importers_share <-
                                         rv_self_define_ex$tmp_global_by_country_all %>%
                                         filter( `Trade.Flow` == 'Import' ) %>%
                                         arrange( -Share ) %>%
                                         slice(1:10) %>%
                                         group_by(Year) %>%
                                         summarise( Share = sum(Share, na.rm=T) ) %>%
                                         ungroup %>%
                                         dplyr::select(Share) %>%
                                         as.numeric
                                      
                                      ##  of top 20 markets -- number of high growth market
                                      rv_self_define_ex$tmp_number_high_growth_importers <-
                                         nrow(
                                            rv_self_define_ex$tmp_global_by_country_all %>%
                                               filter( `Trade.Flow` == 'Import' ) %>%
                                               arrange( -Share ) %>%
                                               slice(1:20) %>%
                                               filter( Value_per_change >= 0.1 )
                                         )
                                      
                                      ## Top 3 exporters share
                                      rv_self_define_ex$tmp_top3_exporters_share <-
                                         rv_self_define_ex$tmp_global_by_country_all %>%
                                         filter( `Trade.Flow` == 'Export' ) %>%
                                         arrange( -Share ) %>%
                                         slice(1:3) %>%
                                         group_by(Year) %>%
                                         summarise( Share = sum(Share, na.rm=T) ) %>%
                                         ungroup %>%
                                         dplyr::select(Share) %>%
                                         as.numeric
                                      
                                      ## Top 10 exporters share
                                      rv_self_define_ex$tmp_top10_exporters_share <-
                                         rv_self_define_ex$tmp_global_by_country_all %>%
                                         filter( `Trade.Flow` == 'Export' ) %>%
                                         arrange( -Share ) %>%
                                         slice(1:10) %>%
                                         group_by(Year) %>%
                                         summarise( Share = sum(Share, na.rm=T) ) %>%
                                         ungroup %>%
                                         dplyr::select(Share) %>%
                                         as.numeric
                                      
                                      ## NZ's share
                                      rv_self_define_ex$tmp_nz_share <-
                                         rv_self_define_ex$tmp_global_by_country_all %>%
                                         filter( `Trade.Flow` == 'Export' ) %>%
                                         filter( Reporter == 'New Zealand' ) %>%
                                         dplyr::select(Share) %>%
                                         as.numeric
                                      
                                      ## 3. build data for importers and exporter maps -------------------
                                      rv_self_define_ex$tmp_un_comtrade_importer_map <- 
                                         rv_self_define_ex$tmp_global_by_country_all %>%
                                         filter( `Trade.Flow` == "Import" ) %>%
                                         left_join( concord_uncomtrade_country, by = c('Reporter.ISO' = 'ISO3') ) %>%
                                         filter( !is.na(lat) ) %>%
                                         mutate( Value = `Trade.Value..US..`/10^6,
                                                 z= Value,
                                                 name = Reporter)
                                      
                                      rv_self_define_ex$tmp_un_comtrade_exporter_map <- 
                                         rv_self_define_ex$tmp_global_by_country_all %>%
                                         filter( `Trade.Flow` == "Export" ) %>%
                                         left_join( concord_uncomtrade_country, by = c('Reporter.ISO' = 'ISO3') ) %>%
                                         filter( !is.na(lat) ) %>%
                                         mutate( Value = `Trade.Value..US..`/10^6,
                                                 z= Value,
                                                 name = Reporter)
                                      
                                      ## 4. Build data for the summary table -----------------
                                      ## import tab
                                      rv_self_define_ex$tmp_un_comtrade_import_summary_tab <- 
                                         rv_self_define_ex$tmp_global_by_country_and_eu_all %>%
                                         filter( `Trade.Flow` == 'Import' ) %>%
                                         dplyr::select( Reporter, Share, 
                                                        `Trade.Value..US..` ,Value_per_change, Value_abs_change,  
                                                        Price, Price_per_change ) %>%
                                         mutate( `Trade.Value..US..` = `Trade.Value..US..`/10^6,
                                                 Value_abs_change = Value_abs_change/10^6)
                                      
                                      ## export tab
                                      rv_self_define_ex$tmp_un_comtrade_export_summary_tab <- 
                                         rv_self_define_ex$tmp_global_by_country_and_eu_all %>%
                                         filter( `Trade.Flow` == 'Export' ) %>%
                                         dplyr::select( Reporter, Share, 
                                                        `Trade.Value..US..` ,Value_per_change, Value_abs_change,  
                                                        Price, Price_per_change ) %>%
                                         mutate( `Trade.Value..US..` = `Trade.Value..US..`/10^6,
                                                 Value_abs_change = Value_abs_change/10^6)
                                   }
                                })
                                
                                ## 2.6.1 IF hourly query reach 100 ------------
                                # output$Un_comtrade_fail_msg_self_define <- 
                                #    renderUI({
                                #       if( is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                #          tags$h1( "Global analysis cannot be performed due to reaching usage limit of 100 requests per hour. Please come back in a hour time." )
                                #    })
                                # 
                                # insertUI(selector = '#body_ci_markets_ex_self_defined',
                                #          ui = div(id = "#body_ci_markets_ex_fail_msg_self_define",
                                #                   uiOutput("Un_comtrade_fail_msg_self_define")
                                #          )
                                # )
                                
                                ## 2.7 UN com Trade data analysis starts here Key facts table ----------
                                ## world market size
                                print("--------- Building facts value boxes -------------")
                                output$Un_comtrade_world_market_size_self_define <-
                                   renderInfoBox({
                                      if( is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      infoBox( "World market size",
                                               paste0("$", 
                                                      format(round(rv_self_define_ex$tmp_global_size_value_now/10^6), big.mark = ","),
                                                      " m"
                                               )
                                               , icon = icon('globe', lib = "glyphicon")
                                               
                                      )
                                   })
                                
                                ## 5 year growth
                                output$Un_comtrade_world_market_change_self_define <-
                                   renderInfoBox({
                                      if( is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      
                                      if( is.null(rv_self_define_ex$tmp_global_size_value_change) )
                                         infoBox( "CAGR (5 years)",
                                                  HTML(paste0( "Not available" )), 
                                                  icon = icon('minus'))
                                      
                                      if(rv_self_define_ex$tmp_global_size_value_change>0 ){
                                         infoBox( "CAGR (5 years)",
                                                  HTML(paste0( "<font color='green'> +",
                                                               round(abs(rv_self_define_ex$tmp_global_size_value_change)*100,1),
                                                               "% </font>"
                                                  )), 
                                                  icon = icon('arrow-up'), color = 'green')
                                      }else{
                                         infoBox( "CAGR (5 years)",
                                                  HTML(paste0( "<font color='red'> -",
                                                               round(abs(rv_self_define_ex$tmp_global_size_value_change)*100,1),
                                                               "% </font>"
                                                  )), 
                                                  icon = icon('arrow-down'), color = 'red')
                                      }
                                      
                                   })
                                
                                ## 5 yr abs change
                                output$Un_comtrade_world_market_change_abs_self_define <-
                                   renderInfoBox({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw) )
                                         return(NULL)
                                      
                                      if( is.null(rv_self_define_ex$tmp_global_size_value_change_abs) )
                                         infoBox( "ABS (5 years)",
                                                  HTML(paste0( "Not available" )), 
                                                  icon = icon('minus'))
                                      
                                      if(rv_self_define_ex$tmp_global_size_value_change_abs>0 ){
                                         infoBox( "ABS (5 years)",
                                                  HTML(paste0("<font color='green'> +$", 
                                                              format(round(rv_self_define_ex$tmp_global_size_value_change_abs/10^6), big.mark = ","),
                                                              " m </font>"
                                                  )),
                                                  icon = icon('arrow-up'), color = 'green')
                                      }else{
                                         infoBox( "ABS (5 years)",
                                                  HTML(paste0("<font color='red'> -$", 
                                                              format(round(abs(rv_self_define_ex$tmp_global_size_value_change_abs)/10^6), big.mark = ","),
                                                              " m </font>"
                                                  )),
                                                  icon = icon('arrow-down'), color = 'red')
                                      }
                                   })
                                
                                ## top 3 importer share
                                output$Un_comtrade_top3_importers_share_self_define <-
                                   renderInfoBox({
                                      if( is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      infoBox( HTML("Top 3 importers <br> share"),
                                               paste0( 
                                                  round(abs(rv_self_define_ex$tmp_top3_importers_share)*100,1),
                                                  "%"
                                               ),
                                               icon = icon('import', lib = "glyphicon"))
                                   })
                                
                                ## top 10 importer share
                                output$Un_comtrade_top10_importers_share_self_define <-
                                   renderInfoBox({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      infoBox( HTML("Top 10 importers <br> share"),
                                               paste0( 
                                                  round(abs(rv_self_define_ex$tmp_top10_importers_share)*100,1),
                                                  "%"
                                               ),
                                               icon = icon('import', lib = "glyphicon"))
                                   })
                                
                                ##  of top 20 markets -- number of high growth market
                                output$Un_comtrade_high_growth_importers_self_define <-
                                   renderInfoBox({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      infoBox( HTML("Top 20 importers <br> with CAGR>10%"),
                                               paste0( rv_self_define_ex$tmp_number_high_growth_importers) ,
                                               icon = icon('import', lib = "glyphicon"))
                                   })
                                
                                
                                ## top 3 exporter share
                                output$Un_comtrade_top3_exporters_share_self_define <-
                                   renderInfoBox({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw) )
                                         return(NULL)
                                      infoBox( HTML("Top 3 exporters <br> share"),
                                               paste0( 
                                                  round(abs(rv_self_define_ex$tmp_top3_exporters_share)*100,1),
                                                  "%"
                                               ),
                                               icon = icon('export', lib = "glyphicon"))
                                   })
                                
                                ## top 10 exporter share
                                output$Un_comtrade_top10_exporters_share_self_define <-
                                   renderInfoBox({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      infoBox( HTML("Top 10 exporters <br> share"),
                                               paste0( 
                                                  round(abs(rv_self_define_ex$tmp_top10_exporters_share)*100,1),
                                                  "%"
                                               ),
                                               icon = icon('export', lib = "glyphicon"))
                                   })
                                
                                ## new zealand share
                                output$Un_comtrade_nz_share_self_define <-
                                   renderInfoBox({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      if( rv_self_define_ex$tmp_nz_share < 0.001 ){
                                         infoBox( HTML("New Zealand <br> share"),
                                                  paste0( "Less than 0.1%" ),
                                                  icon = icon('export', lib = "glyphicon"))
                                      }else{
                                         infoBox( HTML("New Zealand <br> share"),
                                                  paste0( 
                                                     round(abs(rv_self_define_ex$tmp_nz_share)*100,1),
                                                     "%"
                                                  ),
                                                  icon = icon('export', lib = "glyphicon"))
                                      }
                                      
                                   })
                                
                                
                                ##!!!!! try UI insert: value box for global market facts ----------- 
                                output$H1_title_global_facts_self_define <-
                                   renderText({
                                      if( is.null(rv_self_define_ex$tmp_global_by_country_raw)   )
                                         return(NULL)
                                      paste0( "Global market analysis (", tmp_un_comtrade_max_year ,")" )
                                   })
                                
                                output$H1_title_global_facts_note_self_define <-
                                   renderText({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      paste0( "All values undner the global market analysis are reported in current US dollar" )
                                   })
                                
                                output$H3_title_global_facts_summary_self_define <-
                                   renderText({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)   )
                                         return(NULL)
                                      paste0( "Key facts and summary" )
                                   })
                                
                                ### insert global market key facts and summary value boxe
                                insertUI(
                                   selector = '#body_ci_markets_ex_self_defined',
                                   ui =   div( id = 'body_ci_markets_ex_global_facts_self_define',
                                               fluidRow( 
                                                  h1( HTML(paste0(textOutput("H1_title_global_facts_self_define"))) ),
                                                  p( HTML(paste0(textOutput("H1_title_global_facts_note_self_define"))) ),
                                                  h3( HTML(paste0(textOutput("H3_title_global_facts_summary_self_define"))) ),
                                                  infoBoxOutput("Un_comtrade_world_market_size_self_define") ,
                                                  infoBoxOutput("Un_comtrade_world_market_change_self_define" ) ,
                                                  infoBoxOutput("Un_comtrade_world_market_change_abs_self_define" ) 
                                               ),
                                               fluidRow(
                                                  infoBoxOutput("Un_comtrade_top3_importers_share_self_define" ) ,
                                                  infoBoxOutput("Un_comtrade_top10_importers_share_self_define" ) ,
                                                  infoBoxOutput("Un_comtrade_high_growth_importers_self_define" ) 
                                               ),
                                               fluidRow(
                                                  infoBoxOutput("Un_comtrade_top3_exporters_share_self_define" ) ,
                                                  infoBoxOutput("Un_comtrade_top10_exporters_share_self_define" ) ,
                                                  infoBoxOutput("Un_comtrade_nz_share_self_define" ) 
                                               )
                                   )
                                )
                                
                                
                                ## 2.8 Quick glance at both importers and exporters map --------
                                print("--------- Building importer and exporter map -------------")
                                output$UN_comtrade_importer_Map_self_define <- 
                                   renderHighchart({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      hcmap( data = rv_self_define_ex$tmp_un_comtrade_importer_map ,
                                             value = 'Value',
                                             joinBy = c('iso-a2','ISO2'), 
                                             name= paste0( "Import value"),
                                             borderWidth = 1,
                                             borderColor = "#fafafa",
                                             nullColor = "lightgrey",
                                             tooltip = list( table = TRUE,
                                                             sort = TRUE,
                                                             headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                             pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
                                      ) %>%
                                         hc_add_series(data =  rv_self_define_ex$tmp_un_comtrade_importer_map ,
                                                       type = "mapbubble",
                                                       color  = hex_to_rgba("#DF1995", 0.9),
                                                       minSize = 0,
                                                       name= paste0( "Import value"),
                                                       maxSize = 30,
                                                       tooltip = list(table = TRUE,
                                                                      sort = TRUE,
                                                                      headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                                      pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
                                         ) %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_legend( enabled=FALSE ) %>% 
                                         hc_mapNavigation(enabled = TRUE) 
                                   })
                                
                                ## exporter map
                                output$UN_comtrade_exporter_Map_self_define <- 
                                   renderHighchart({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      hcmap( data = rv_self_define_ex$tmp_un_comtrade_exporter_map ,
                                             value = 'Value',
                                             joinBy = c('iso-a2','ISO2'), 
                                             name= paste0( "Export value"),
                                             borderWidth = 1,
                                             borderColor = "#fafafa",
                                             nullColor = "lightgrey",
                                             tooltip = list( table = TRUE,
                                                             sort = TRUE,
                                                             headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                             pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
                                      ) %>%
                                         hc_add_series(data =  rv_self_define_ex$tmp_un_comtrade_exporter_map ,
                                                       type = "mapbubble",
                                                       color  = hex_to_rgba("#97D700", 0.9),
                                                       minSize = 0,
                                                       name= paste0( "Export value"),
                                                       maxSize = 30,
                                                       tooltip = list(table = TRUE,
                                                                      sort = TRUE,
                                                                      headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                                      pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
                                         ) %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_legend( enabled=FALSE ) %>% 
                                         hc_mapNavigation(enabled = TRUE) 
                                   })
                                
                                ## !!!!! try UI insert: map of importer / exporters ----------- 
                                output$H3_title_un_comtrade_map_self_define <-
                                   renderText({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw) )
                                         return(NULL)
                                      paste0("Global importers and exporters at a glance")
                                   })
                                
                                output$H3_title_un_comtrade_map_note_self_define <-
                                   renderText({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      paste0( "The size of bubble area and color both represent the value of imports or exports" ) 
                                   })
                                
                                output$H4_title_un_comtrade_importer_map_self_define <-
                                   renderText({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      paste0("Global IMPORT markets")
                                   })
                                
                                output$H4_title_un_comtrade_exporter_map_self_define <-
                                   renderText({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      paste0("Global EXPORT markets")
                                   })
                                
                                ## Insert ui here
                                insertUI(
                                   selector = '#body_ci_markets_ex_self_defined',
                                   ui =   div( id = 'body_ci_markets_ex_un_comtrade_map_self_define',
                                               fluidRow(h3( HTML(paste0(textOutput("H3_title_un_comtrade_map_self_define"))) ) ,
                                                        p( HTML(paste0(textOutput("H3_title_un_comtrade_map_note_self_define"))) ),
                                                        column(6, div(id = "body_ci_markets_ex_un_comtrade_map_import_self_define", h4( HTML(paste0(textOutput("H4_title_un_comtrade_importer_map_self_define"))) ), highchartOutput('UN_comtrade_importer_Map_self_define') ) ),
                                                        column(6, div(id = "body_ci_markets_ex_un_comtrade_map_export_self_define", h4( HTML(paste0(textOutput("H4_title_un_comtrade_exporter_map_self_define"))) ), highchartOutput('UN_comtrade_exporter_Map_self_define') ) )
                                               )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                
                                ## 2.8.1 Sankey plot for a commodity ---------------
                                print("--------- Building Sankey data -------------")
                                
                                observe({
                                   ## check if able to get sankey data
                                   rv_self_define_ex$Fail_sankey_data <-
                                      try(
                                         rv_self_define_ex$sankey_plot_data <-
                                            get_data_sankey_uncomtrade( cc = tmp_hs_ex(), max_year = tmp_un_comtrade_max_year, eu_internal = "No" )
                                      )
                                   
                                   if( class(rv_self_define_ex$Fail_sankey_data) == 'try-error' )
                                      print("--------- FAIL: building Sankey data !!! -------------")
                                })
                                
                                print("--------- Building Sankey plots -------------")
                                output$Sankey_trade_self_define <-
                                   renderSankeyNetwork({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw) | 
                                           length(tmp_hs_ex())>1 |  
                                           class(rv_self_define_ex$Fail_sankey_data) == 'try-error' ){
                                         return(NULL)
                                      }else{
                                         print("--------- Plotting Sankey plots -------------")
                                         sankey_uncomtrade( cc = tmp_hs_ex(), max_year = tmp_un_comtrade_max_year,eu_internal = as.character(input$btn_eu_internal_self_define)  )
                                      }
                                   })
                                
                                ## !!!!! try UI insert: Sankey plot ----------- 
                                output$H3_title_sankey_self_define <-
                                   renderText({
                                      # if( is.null(rv_self_define_ex$tmp_global_by_country_raw) | 
                                      #     length(tmp_hs_ex())>1 |  
                                      #     class(rv_self_define_ex$Fail_sankey_data) == 'try-error' )
                                      #    #return(NULL)
                                      # {paste0("Unable to perform global trade flow analyasis due to data query limits. Please wait for a hour.")}else{
                                      #    paste0( "Global trade flow analysis" )
                                      # }
                                      
                                      if( class(rv_self_define_ex$Fail_sankey_data) == 'try-error' & 
                                          input$select_comodity_ex_for_market_analysis != "" &
                                          length(tmp_hs_ex()) == 1 ){
                                         paste0("Unable to perform global trade flow analyasis due to data query limits. Please wait for a hour.")
                                      }
                                      
                                      if( class(rv_self_define_ex$Fail_sankey_data) == 'try-error' & 
                                          input$select_comodity_ex_for_market_analysis == ""  ){
                                         return(NULL)
                                      }
                                      
                                      if( class(rv_self_define_ex$Fail_sankey_data) != 'try-error' &
                                          input$select_comodity_ex_for_market_analysis != "" &
                                          length(tmp_hs_ex()) ==  1){
                                         paste0( "Global trade flow analysis" )
                                      }
                                   })

                                output$H3_title_sankey_self_define_note <-
                                   renderUI({
                                      if( is.null(rv_self_define_ex$tmp_global_by_country_raw) | 
                                          length(tmp_hs_ex())>1 |  
                                          class(rv_self_define_ex$Fail_sankey_data) == 'try-error' ) 
                                         return(NULL)
                                      tags$p("This sankey plot shows trade flows of the selected commodity from expoters to importers. The displayed markets coverage is equal to or greater than 90% of global exports. The displayed trade flows are equal to or greater than 0.5% of global exports. Different colors are used to distinguish",
                                             tags$span( "EXPORTERS", style = "color: #97D700; font-weight: bold" ),
                                             ", ",
                                             tags$span( "IMPORTERS", style = "color: #CD5B45; font-weight: bold"),
                                             ", and ",
                                             tags$span( "BOTH", style = "color: #FBE122; font-weight: bold"), "." )

                                   })

                                ## button to choose show/hide EU internal trade
                                output$Btn_EU_Internal_self_define <-
                                   renderUI({
                                      if( is.null(rv_self_define_ex$tmp_global_by_country_raw) | 
                                          length(tmp_hs_ex())>1 |  
                                          class(rv_self_define_ex$Fail_sankey_data) == 'try-error' ) 
                                         return(NULL)
                                      radioButtons("btn_eu_internal_self_define",
                                                   p("Display EU internal trade: " ),
                                                   choiceNames = list(icon("check"), icon("times")),
                                                   choiceValues = list( "Yes" , "No"),
                                                   #c( "Yes" = "Yes", "No" = "No"),
                                                   inline=T,
                                                   selected="No")
                                   })

                                output$Btn_EU_Internal_self_define_note <-
                                   renderUI({
                                      if( is.null(rv_self_define_ex$tmp_global_by_country_raw) | 
                                          length(tmp_hs_ex())>1 |  
                                          class(rv_self_define_ex$Fail_sankey_data) == 'try-error' ) 
                                         return(NULL)
                                      tags$p( "You may choose to show or hide EU internal trade in the sankey plot by using the buttons below." )
                                   })

                                ## Insert ui here
                                insertUI(
                                   selector = '#body_ci_markets_ex_self_defined',
                                   ui =   div( id = 'body_ci_markets_ex_un_comtrade_sankey_self_define',
                                               fluidRow(h3( HTML(paste0(textOutput("H3_title_sankey_self_define"))) ) ,
                                                        #p( HTML(paste0(textOutput("H2_title_sankey_note"))) ),
                                                        uiOutput("H3_title_sankey_self_define_note"),
                                                        uiOutput("Btn_EU_Internal_self_define_note"),
                                                        uiOutput("Btn_EU_Internal_self_define"),
                                                        sankeyNetworkOutput( "Sankey_trade_self_define" )
                                               )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                ## 2.9 Generating summary tables for both importers and exporters -------
                                # container of the table -- importers 
                                print("--------- Building importer and exporter tabels -------------")
                                
                                sketch_uncomtrade_im<-  htmltools::withTags(table(
                                   class = 'display',
                                   thead(
                                      tr(
                                         th(rowspan = 2, 'Market'),
                                         th(rowspan = 2, 'Import share'),
                                         th(colspan = 3, 'Import value'),
                                         th(colspan = 2, 'Import price')
                                      ),
                                      tr( #th('Country'),
                                         lapply(rep(c('Value ($m)', 'CAGR5', 'ABS5'), 1), th, align = 'center'),
                                         lapply(rep(c('$/kg (unit)', 'CAGR5' ), 1), th, align = 'center')
                                      )
                                   )
                                ))
                                
                                ## table for importers
                                output$UN_com_trade_importer_summary_self_define <-
                                   renderDataTable({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw) )
                                         return(NULL)
                                      datatable( rv_self_define_ex$tmp_un_comtrade_import_summary_tab,
                                                 container = sketch_uncomtrade_im,
                                                 rownames = FALSE,
                                                 extensions = 'Buttons',
                                                 options = list(dom = 'Bltp', 
                                                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                                scrollX = TRUE,
                                                                pageLength = 10,
                                                                lengthMenu = list(c(10, 30 , -1), list('10','30' ,'All')),
                                                                columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(rv_self_define_ex$tmp_un_comtrade_import_summary_tab)-1) ) )
                                                 )
                                      ) %>%
                                         formatPercentage( c('Share', 'Value_per_change', 'Price_per_change' ) , digit = 1 ) %>%
                                         formatCurrency( columns = c('Trade.Value..US..','Value_abs_change'), digits = 0 ) %>%
                                         formatCurrency( columns = c('Price'), digits = 2 ) %>%
                                         formatStyle(
                                            c('Value_per_change' ),
                                            background = styleColorBar( c(0,max(rv_self_define_ex$tmp_un_comtrade_import_summary_tab[1:min(20,nrow(rv_self_define_ex$tmp_un_comtrade_import_summary_tab)),c('Value_per_change' )],na.rm=T)*2) ,
                                                                        'lightblue'),
                                            backgroundSize = '100% 90%',
                                            backgroundRepeat = 'no-repeat',
                                            backgroundPosition = 'center',
                                            color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                         ) %>%
                                         formatStyle(
                                            c('Price_per_change' ),
                                            background = styleColorBar( c(0,max(rv_self_define_ex$tmp_un_comtrade_import_summary_tab[1:min(20,nrow(rv_self_define_ex$tmp_un_comtrade_import_summary_tab)),c('Price_per_change' )],na.rm=T)*2) ,
                                                                        'lightblue'),
                                            backgroundSize = '100% 90%',
                                            backgroundRepeat = 'no-repeat',
                                            backgroundPosition = 'center',
                                            color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                         ) %>%
                                         formatStyle(
                                            c('Value_abs_change' ),
                                            backgroundSize = '100% 90%',
                                            backgroundRepeat = 'no-repeat',
                                            backgroundPosition = 'center',
                                            color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                         ) %>%
                                         formatStyle( 1:ncol(rv_self_define_ex$tmp_un_comtrade_import_summary_tab), 'vertical-align'='center', 'text-align' = 'center' )
                                   })
                                
                                ### build export table
                                # container of the table -- importers 
                                sketch_uncomtrade_ex <-  htmltools::withTags(table(
                                   class = 'display',
                                   thead(
                                      tr(
                                         th(rowspan = 2, 'Market'),
                                         th(rowspan = 2, 'Export share'),
                                         th(colspan = 3, 'Export value'),
                                         th(colspan = 2, 'Export price')
                                      ),
                                      tr( #th('Country'),
                                         lapply(rep(c('Value ($m)', 'CAGR5', 'ABS5'), 1), th, align = 'center'),
                                         lapply(rep(c('$/kg (unit)', 'CAGR5' ), 1), th, align = 'center')
                                      )
                                   )
                                ))
                                
                                ## table for importers
                                output$UN_com_trade_exporter_summary_self_define <-
                                   renderDataTable({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw) )
                                         return(NULL)
                                      datatable( rv_self_define_ex$tmp_un_comtrade_export_summary_tab,
                                                 container = sketch_uncomtrade_ex,
                                                 rownames = FALSE,
                                                 extensions = 'Buttons',
                                                 options = list(dom = 'Bltp', 
                                                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                                scrollX = TRUE,
                                                                pageLength = 10,
                                                                lengthMenu = list(c(10, 30, -1), list('10', '30' ,'All')),
                                                                columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(rv_self_define_ex$tmp_un_comtrade_export_summary_tab)-1) ) )
                                                 )
                                      ) %>%
                                         formatPercentage( c('Share', 'Value_per_change', 'Price_per_change' ) , digit = 1 ) %>%
                                         formatCurrency( columns = c('Trade.Value..US..','Value_abs_change'), digits = 0 ) %>%
                                         formatCurrency( columns = c('Price'), digits = 2 ) %>%
                                         formatStyle(
                                            c('Value_per_change' ),
                                            background = styleColorBar( c(0,max(rv_self_define_ex$tmp_un_comtrade_export_summary_tab[1:min(20,nrow(rv_self_define_ex$tmp_un_comtrade_export_summary_tab)),c('Value_per_change' )],na.rm=T)*2) ,
                                                                        'lightblue'),
                                            backgroundSize = '100% 90%',
                                            backgroundRepeat = 'no-repeat',
                                            backgroundPosition = 'center',
                                            color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                         ) %>%
                                         formatStyle(
                                            c('Price_per_change' ),
                                            background = styleColorBar( c(0,max(rv_self_define_ex$tmp_un_comtrade_export_summary_tab[1:min(20,nrow(rv_self_define_ex$tmp_un_comtrade_export_summary_tab)),c('Price_per_change' )],na.rm=T)*2) ,
                                                                        'lightblue'),
                                            backgroundSize = '100% 90%',
                                            backgroundRepeat = 'no-repeat',
                                            backgroundPosition = 'center',
                                            color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                         ) %>%
                                         formatStyle(
                                            c('Value_abs_change' ),
                                            backgroundSize = '100% 90%',
                                            backgroundRepeat = 'no-repeat',
                                            backgroundPosition = 'center',
                                            color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                         ) %>%
                                         formatStyle( 1:ncol(rv_self_define_ex$tmp_un_comtrade_export_summary_tab), 'vertical-align'='center', 'text-align' = 'center' )
                                   })
                                
                                ## Insert ui here: summary tables  ----------------
                                output$H3_title_un_comtrade_summary_tab_self_define <-
                                   renderText({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw) )
                                         return(NULL)
                                      paste0("Summary tables for importers and exporters")
                                   })
                                
                                
                                output$H4_title_un_comtrade_importer_sum_tab_self_define <-
                                   renderText({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      paste0("Global IMPORT markets")
                                   })
                                
                                output$H4_title_un_comtrade_exporter_sum_tab_self_define <-
                                   renderText({
                                      if(  is.null(rv_self_define_ex$tmp_global_by_country_raw)  )
                                         return(NULL)
                                      paste0("Global EXPORT markets")
                                   })
                                
                                insertUI(
                                   selector = '#body_ci_markets_ex_self_defined',
                                   ui =   div( id = 'body_ci_markets_ex_un_comtrade_summary_tab_self_define',
                                               fluidRow(h3( HTML(paste0(textOutput("H3_title_un_comtrade_summary_tab_self_define"))) ) ,
                                                        #p( HTML(paste0(textOutput("H3_title_un_comtrade_map_note"))) ),
                                                        column(6, div(id = "body_ci_markets_ex_un_comtrade_import_summary_tab_self_define", h4( HTML(paste0(textOutput("H4_title_un_comtrade_importer_sum_tab_self_define"))) ), dataTableOutput('UN_com_trade_importer_summary_self_define') ) ),
                                                        column(6, div(id = "body_ci_markets_ex_un_comtrade_export_summary_tab_self_define", h4( HTML(paste0(textOutput("H4_title_un_comtrade_exporter_sum_tab_self_define"))) ), dataTableOutput('UN_com_trade_exporter_summary_self_define') ) )
                                               )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                
                                ## 3.0 Get the leftover quota and reset time ---------
                                output$Un_comtrade_msg_self_define <-
                                   renderUI({
                                      #if(  is.null(rv_self_define_ex$tmp_global_by_country_raw) )
                                       #  return(NULL)
                                      # tags$p(paste0( "Note: ",ct_get_remaining_hourly_queries(), 
                                      #                " number of queries are left for the global analysis section from the UN Comtrade. The reset time will be at ", 
                                      #                ct_get_reset_time() ,
                                      #                ", while the current time is ", format(Sys.time()) , "."
                                      #                )
                                      #        )
                                      
                                      if(  input$select_comodity_ex_for_market_analysis == "" ){
                                         return(NULL)
                                      }else{
                                         tags$div(
                                            tags$hr(),
                                            tags$p(paste0( "Note: ",ct_get_remaining_hourly_queries(), 
                                                           " number of queries are left for the global analysis section from the UN Comtrade. The reset time will be at ", 
                                                           ct_get_reset_time() ,
                                                           ", while the current time is ", format(Sys.time()) , "."
                                            ))
                                         )
                                      }
                                   })
                                
                                insertUI( selector = '#body_ci_markets_ex_self_defined',
                                          ui = div( id = 'body_ci_markets_ex_un_comtrade_msg_self_define',
                                                    fluidRow( #tags$hr(),
                                                              uiOutput("Un_comtrade_msg_self_define") )
                                                    ) 
                                )
                                ## 4.15 Hide generating report message ----------
                                observe({
                                   if( any(input$select_comodity_ex_for_market_analysis %in% tmp_tab$Name)  ){
                                      shinyjs::hide( id = "body_ci_market_loading_message_self_define" )
                                  }
                                })
                                
                                ## hide wait message ------
                                shinyjs::hide( id = 'wait_message_ci_ex' )
                             }
                             
                          }
                       }
                     }
                   )
      
      ### 1.2 Imports--- when press the Build Report button ------------------
      observeEvent( input$btn_build_commodity_report_im,
                    {
                       ## 1.1 check the inputs are correct -------------
                       ## first both checked not passed
                       tmp_execution_pre_define <- tmp_execution_self_define <- FALSE
                       
                       ## 1.2 work on Pre-deinfed Warning if no pre-defined commodity is selected ------------------------
                       if(input$rbtn_prebuilt_diy_im=='Pre-defined' & is.null(input$select_comodity_im)) {
                          showModal(modalDialog(
                             title = "Warning",
                             tags$b("Please select one or multiple pre-defined commodities!"),
                             size = 's'
                          ))
                       }
                       
                       ## if test pass
                       if( input$rbtn_prebuilt_diy_im=='Pre-defined' & !is.null(input$select_comodity_im) ){
                          tmp_execution_pre_define <- TRUE
                       }
                       
                       ## 1.2.1 Build graphs pre-defined commodity --------------
                       if(tmp_execution_pre_define){
                          ## --- hide howto -----
                          shinyjs::hide(id = 'ci_howto_im')
                          ## show waite message ----
                          shinyjs::show( id = 'wait_message_ci_im' )
                          ## disable the buttone ---
                          shinyjs::disable("btn_build_commodity_report_im")
                          ## disable the selection  ---
                          shinyjs::disable("select_comodity_im")
                          shinyjs::disable("rbtn_prebuilt_diy_im")
                          
                          ## first both checked not passed
                          checked_pre_defined_im <- checked_self_defined_im <- TRUE
                          
                          ### work on Data noW!!!!!!!
                          tmp_selected_service <- setdiff( input$select_comodity_im , list_snz_commodity_im[['Goods']] )
                          
                          snz_hs <- concord_snz_ig$HS_codes[concord_snz_ig$SNZ_commodity %in% input$select_comodity_im ]
                          
                          if( length(tmp_selected_service) >=1 ){
                             hs_group <-
                                concord_snz_ig %>%
                                filter( HS_codes %in% snz_hs ) %>%
                                bind_rows( data.frame(HS_codes = tmp_selected_service,
                                                      SNZ_commodity = tmp_selected_service) )
                          }else{
                             hs_group <-
                                concord_snz_ig %>%
                                filter( HS_codes %in% snz_hs )
                          }
                          
                          colnames(hs_group) <- c("HS_code", "HS_group")
                          ## make columns characters and make sure HS code has 01, and 0122 etc format
      
                          ## 3.1 Build import value line chart -------------------- 
                          tmp_top_g_im <-
                             dtf_shiny_commodity_service_im %>%
                             filter( SNZ_commodity %in% input$select_comodity_im ,
                                     !SNZ_commodity %in% tmp_selected_service,
                                     SNZ_commodity != 'Confidential data' ) %>%
                             filter( Year == max(Year)) %>% 
                             arrange( -Value ) %>%
                             dplyr::select( SNZ_commodity ) %>%
                             as.matrix() %>%
                             as.character
                          
                          tmp_top_s_im <-
                             dtf_shiny_commodity_service_im %>%
                             filter( SNZ_commodity %in% tmp_selected_service) %>%
                             filter( Year == max(Year) ) %>%
                             arrange( -Value ) %>%
                             dplyr::select( SNZ_commodity ) %>%
                             as.matrix() %>%
                             as.character
                          
                          ## top selected commodities and top 5services
                          tmp_top_im <- c( tmp_top_g_im, tmp_top_s_im)
                          
                          ## data frame to plot
                          tmp_dtf_key_line_im <- 
                             dtf_shiny_commodity_service_im %>%
                             filter( SNZ_commodity %in% tmp_top_im,
                                     Year >=2007) %>%
                             mutate( Value = round(Value/10^6),
                                     SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_im)
                             ) %>%
                             arrange( SNZ_commodity )
                          
                          ### plot
                          tmp_import_hc <- 
                             highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             # hc_add_series( data =  tmp_dtf_key_line_im %>% filter( Type_gs == 'Goods' ) ,
                             #                mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                             #                type = 'line',
                             #                marker = list(symbol = 'circle') #,
                             #                #visible = c(T,rep(F,length(tmp_top_g_im)-1))
                             # ) %>%
                             hc_xAxis( categories = c( unique( tmp_dtf_key_line_im$Year) ) ) %>%
                             hc_yAxis( title = list(text = "$ million, NZD"),
                                       labels = list( format = "${value:,.0f} m")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: ${point.y} m"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          
                          ### if any services are selected?
                          if( length(tmp_top_g_im)>=1 & length(tmp_top_s_im)==0 ){
                             output$CIImportValueLine <- 
                                renderHighchart(
                                   tmp_import_hc %>%
                                      hc_add_series( data =  tmp_dtf_key_line_im %>% filter( Type_gs == 'Goods' ) ,
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line',
                                                     marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_g_im)-1))
                                      )
                                )
                          }
                          if( length(tmp_top_s_im)>=1 & length(tmp_top_g_im)==0 ){
                             output$CIImportValueLine <- 
                                renderHighchart(
                                   tmp_import_hc %>%
                                      hc_add_series( data =  tmp_dtf_key_line_im %>% filter( Type_gs == 'Services' ),
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_s_im)-1))
                                      )
                                )
                          }
                          if( length(tmp_top_s_im)>=1 & length(tmp_top_g_im) >= 1 ) {
                             output$CIImportValueLine <- 
                                renderHighchart(
                                   tmp_import_hc %>%
                                      hc_add_series( data =  tmp_dtf_key_line_im %>% filter( Type_gs == 'Goods' ) ,
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line',
                                                     marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_g_im)-1))
                                      ) %>%
                                      hc_add_series( data =  tmp_dtf_key_line_im %>% filter( Type_gs == 'Services' ),
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_s_im)-1))
                                      )
                                )
                          }
                          ## 3.2 build import as a percent of total import line chart -----------------------
                          tmp_tot_im <-
                             dtf_shiny_full %>%
                             filter( Country == 'World',
                                     Type_ie == 'Imports',
                                     Year >= 2007 )  %>%
                             mutate( Value = round(Value/10^6) ) %>%
                             group_by( Year, Country, Type_ie ) %>%
                             summarize( Value = sum(Value, na.rm=T) ) %>%
                             ungroup %>%
                             mutate( SNZ_commodity = 'Total imports' )
                          
                          tmp_dtf_percent_line_im <-
                             tmp_dtf_key_line_im %>%
                             bind_rows( tmp_tot_im ) %>%
                             group_by( Year, Country, Type_ie ) %>%
                             mutate( Share = Value/Value[SNZ_commodity=='Total imports'],
                                     Value = Share*100 ) %>%
                             ungroup %>%
                             filter( SNZ_commodity != 'Total imports' ) %>%
                             mutate( SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_im) ) %>%
                             arrange( SNZ_commodity )
                          
                          ### plot
                          tmp_import_percent_hc <- 
                             highchart() %>%
                             hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                             hc_xAxis( categories = c( unique( tmp_dtf_percent_line_im$Year) ) ) %>%
                             hc_yAxis( title = list(text = "Percentage (%)"),
                                       labels = list( format = "{value:,.1f} %")  ) %>%
                             hc_plotOptions(line = list(
                                dataLabels = list(enabled = F),
                                #stacking = "normal",
                                enableMouseTracking = T)
                             )%>%
                             hc_tooltip(table = TRUE,
                                        sort = TRUE,
                                        pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                              " {series.name}: {point.y:,.1f} %"),
                                        headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                             ) %>%
                             hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          #hc_legend( enabled = FALSE )
                          
                          ### if any services are selected?
                          if( length(tmp_top_g_im)>=1&length(tmp_top_s_im)==0 ) {
                             output$CIImportPercentLine <- 
                                renderHighchart(
                                   tmp_import_percent_hc %>%
                                      hc_add_series( data =  tmp_dtf_percent_line_im %>% filter( Type_gs == 'Goods' ) ,
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line',
                                                     marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                      )
                                )
                          }
                          if( length(tmp_top_g_im)==0 & length(tmp_top_s_im)>=1 ){
                             output$CIImportPercentLine <- 
                                renderHighchart(
                                   tmp_import_percent_hc %>%
                                      hc_add_series( data =  tmp_dtf_percent_line_im %>% filter( Type_gs == 'Services' ),
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_s_ex)-1))
                                      )
                                )
                          }
                          if( length(tmp_top_g_im)>=1 & length(tmp_top_s_im)>=1 ){
                             output$CIImportPercentLine <- 
                                renderHighchart(
                                   tmp_import_percent_hc %>%
                                      hc_add_series( data =  tmp_dtf_percent_line_im %>% filter( Type_gs == 'Goods' ) ,
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line',
                                                     marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                      ) %>%
                                      hc_add_series( data =  tmp_dtf_percent_line_im %>% filter( Type_gs == 'Services' ),
                                                     mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                     type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') #,
                                                     #visible = c(T,rep(F,length(tmp_top_s_ex)-1))
                                      )
                                )
                          }
                          
                          ## !!!!! try UI insert ----------- 
                          insertUI(
                             selector = '#body_im',
                             ui =   div( id = 'body_im_line_value_percent',
                                         fluidRow( h1("Imports for selected commodities/services"),
                                                   p("Click on the commodity or service names in the legend area to show their trends"),
                                                   column(6, div(id = "body_value_im", h4("Import values"), highchartOutput('CIImportValueLine') ) ),
                                                   column(6, div(id = "body_percent_im", h4("As a percent of total imports"), highchartOutput('CIImportPercentLine') ) ))
                             )
                          )
                          ## end Try UI insert --------##
                          
                          ## 3.3 build import value change table ----------------
                          ## data frame to plot
                          tmp_dtf_key_tab_im <- 
                             dtf_shiny_commodity_service_im %>%
                             filter( SNZ_commodity %in% tmp_top_im) %>%
                             mutate( SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_im) ) %>%
                             arrange( SNZ_commodity )
                          
                          tmp_tab <-
                             tmp_dtf_key_tab_im %>%
                             mutate( Name =  SNZ_commodity ) %>%
                             group_by( Name) %>%
                             mutate( CAGR1 = CAGR( Value[Year == max(Year)]/
                                                      Value[Year == (max(Year)-1)], 1)/100,
                                     CAGR5 = CAGR( Value[Year == max(Year)]/
                                                      Value[Year == (max(Year)-5)], 5)/100,
                                     CAGR10 = CAGR( Value[Year == max(Year)]/
                                                       Value[Year == (max(Year)-10)], 10)/100,
                                     ABS5 = Value[Year == max(Year)] - Value[Year == (max(Year)-5)],
                                     ABS10 = Value[Year == max(Year)] - Value[Year == (max(Year)-10)]
                             ) %>%
                             ungroup %>%
                             filter( Year == max(Year) ) %>%
                             left_join(tmp_dtf_percent_line_im %>% dplyr::select(-CAGR5, -Value) ) %>%
                             dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10) %>%
                             mutate( Value =Value/10^6,
                                     ABS5 = ABS5/10^6,
                                     ABS10 = ABS10/10^6 ) %>%
                             #dplyr::select( Name, CAGR1, CAGR5, CAGR10) %>%
                             mutate( Name = factor(Name, levels = tmp_top_im) ) %>%
                             arrange( Name )
                          
                          ### join back to hs code
                          hs_group_flat <- 
                             hs_group %>%
                             group_by( HS_group ) %>%
                             summarise( HS_code = paste0(HS_code, collapse = '; ') ) %>%
                             ungroup
                          
                          tmp_tab %<>%
                             left_join( hs_group_flat, by = c("Name"= 'HS_group') ) %>%
                             dplyr::select( HS_code, Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10 )
                          
                          #build table
                          output$GrowthTabSelectedIm <- renderDataTable(
                             datatable( tmp_tab,
                                        rownames = F,
                                        filter = c("top"),
                                        extensions = 'Buttons',
                                        options = list(dom = 'Bfltp',# 'Bt', 
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1 
                                                       ,scrollX = TRUE
                                                       #,fixedColumns = list(leftColumns = 2) 
                                                       ,autoWidth = T
                                                       ,pageLength = 10
                                                       ,lengthMenu = list(c(10,  -1), list('10', 'All')) ,
                                                       searchHighlight = TRUE,
                                                       search = list(regex = TRUE, caseInsensitive = FALSE )
                                                       ),
                                        colnames=c("HS codes", "Classification", 'Value ($m)', 'Share of total imports','CAGR 1', 'CAGR 5', 'CAGR 10', 'ABS5', 'ABS10')
                             ) %>%
                                formatStyle(
                                   c('CAGR1', 'CAGR5', 'CAGR10'),
                                   background = styleColorBar( c(0, max(c(tmp_tab$CAGR1,tmp_tab$CAGR5, tmp_tab$CAGR10))*2) , 'lightblue'),
                                   backgroundSize = '100% 90%',
                                   backgroundRepeat = 'no-repeat',
                                   backgroundPosition = 'center'
                                ) %>%
                                formatStyle(c('CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'),
                                            color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>%
                                formatPercentage( c('Share','CAGR1', 'CAGR5', 'CAGR10'),digit = 1 ) %>%
                                formatStyle( columns = c('Name', 'Value', 'Share', 'CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'), `font-size`= '115%' ) %>%
                                formatCurrency( columns = c('Value', 'ABS5', 'ABS10'), mark = ' ', digits = 1)
                          )
                          ## !!!!! try UI insert ----------- 
                          insertUI(
                             selector = '#body_growth_im',
                             ui =   div( id = 'body_im_growth_tab',
                                         fluidRow( h1("Short, medium, and long term growth for selected commodities/services"),
                                                   p("Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years."),
                                                   dataTableOutput('GrowthTabSelectedIm')
                                         )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          ## 3.4 Build import by country output groups -------------------
                          ## create a selector for each selected commodity ----------------------
                          output$CIIMSelectorByMarkets <- renderUI({
                             selectizeInput("select_comodity_im_for_market_analysis",
                                            tags$p("Please select or search a commodity/service for its market analysis"), 
                                            choices =  tmp_tab$Name[input$GrowthTabSelectedIm_rows_all]  , # input$select_comodity_im, 
                                            selected = NULL,  width = "500px",
                                            multiple = F)
                          })
                          
                          ### build data for market analysis -- these has to be reactive values
                          ## The name of the selected commodity
                          tmp_selected_im <- 
                             reactive({
                                input$select_comodity_im_for_market_analysis
                             })
                          
                          ## The HS codes of the selected commodity
                          tmp_hs_im <- 
                             reactive({
                                hs_group$HS_code[hs_group$HS_group == tmp_selected_im()]
                             })
                          
                          ## The data from of the selected commodity by markets
                          tmp_dtf_market_im <- 
                             reactive({
                                dtf_shiny_full %>%
                                   filter( Commodity %in% tmp_hs_im(), 
                                           Year >= 2007,
                                           Type_ie == 'Imports') %>%
                                   left_join( concord_country_iso_latlon_raw, by = 'Country' ) %>%
                                   group_by( Year, Country, Type_ie, Type_gs, Note, ISO2, lat, lon ) %>%
                                   summarize( Value = sum(Value, na.rm=T) ) %>%
                                   ungroup %>%
                                   mutate( Commodity = as.character( tmp_selected_im() ) )
                             })
                          
                          ### selcted commodity and service outputs
                          output$SelectedIm <- 
                             renderText({
                                tmp_selected_im()
                             })
                          
                          ## !!!!! try UI insert ----------- 
                          insertUI(
                             selector = '#body_ci_markets_im',
                             ui =   div( id = 'body_ci_markets_im_selector',
                                         fluidRow(h1("Import markets analysis for selected commodity/service"),
                                                  uiOutput("CIIMSelectorByMarkets") ),
                                         fluidRow( shiny::span(h1( HTML(paste0(textOutput("SelectedIm"))), align = "center" ), style = "color:darkblue" ) )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          
                          ### 3.4.1 plot map ---------------
                          ### highchart map 
                          tmp_dtf_market_im_map <- 
                             reactive({
                                tmp_dtf_market_im() %>%
                                   filter( Year == max(Year),
                                           !is.na(lat) ) %>%
                                   mutate( Value = Value/10^6,
                                           z= Value,
                                           name = Country)
                             })
                          
                          output$MapIMMarket <- 
                             renderHighchart({
                                hcmap( data = tmp_dtf_market_im_map() ,
                                       value = 'Value',
                                       joinBy = c('iso-a2','ISO2'), 
                                       name="Imports value",
                                       borderWidth = 1,
                                       borderColor = "#fafafa",
                                       nullColor = "lightgrey",
                                       tooltip = list( table = TRUE,
                                                       sort = TRUE,
                                                       headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                       pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
                                ) %>%
                                   hc_add_series(data =  tmp_dtf_market_im_map(),
                                                 type = "mapbubble",
                                                 color  = hex_to_rgba("#f1c40f", 0.9),
                                                 minSize = 0,
                                                 name="Imports value",
                                                 maxSize = 30,
                                                 tooltip = list(table = TRUE,
                                                                sort = TRUE,
                                                                headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                                pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
                                   ) %>%
                                   hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                   hc_legend( enabled=FALSE ) %>% 
                                   hc_mapNavigation(enabled = TRUE) 
                             })

                          ## !!!!! try UI insert ----------- 
                          insertUI(
                             selector = '#body_ci_markets_im',
                             ui =   div( id = 'body_ci_markets_im_map',
                                         fluidRow(h2( paste0("Map of import values")  ) ,
                                                  p("The size of bubble area and color both represent the value of imports."),
                                                  highchartOutput('MapIMMarket') )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          ### 3.4.2 Top markets for selected commodity line chart ----------------
                          tmp_top_country_selected_im <- 
                             reactive({
                                tmp_dtf_market_im() %>%
                                   filter( Year == max(Year),
                                           Value > 0, 
                                           !Country %in% c("World", 
                                                           "Destination Unknown - EU")
                                   ) %>% ## 1 bn commodity
                                   arrange( -Value ) %>%
                                   dplyr::select( Country ) %>%
                                   as.matrix() %>%
                                   as.character
                             })
                          
                          ### only show top 10 countries 
                          # if( length(tmp_top_country_selected_im())<=10 ){
                          #    tmp_top10_country_selected_im <-
                          #       reactive({
                          #          tmp_top_country_selected_im()
                          #       })
                          # }
                          # if( length(tmp_top_country_selected_im())>10 ){
                          #    tmp_top10_country_selected_im <-
                          #       reactive({
                          #          tmp_top_country_selected_im()[1:10]
                          #       })
                          # }
                          tmp_top10_country_selected_im <-
                             reactive({
                                tmp_top_country_selected_im()[1:min(10,length(tmp_top_country_selected_im()))]
                             })
                          
                          ### derive datafrom for the line plot
                          tmp_dtf_market_im_line <- 
                             reactive({
                                tmp_dtf_market_im() %>%
                                   filter( Country %in%  as.character(tmp_top_country_selected_im()) ) %>%
                                   mutate( Value = Value/10^6 ,
                                           Country = factor(Country, levels = as.character(tmp_top_country_selected_im()) )
                                   ) %>%
                                   arrange(Country)
                             })
                          
                          ## line plot
                          output$SelectedImMarketLine <- renderHighchart(
                             highchart() %>%
                                hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                hc_add_series( data =  tmp_dtf_market_im_line() %>%
                                                  filter( Country %in% as.character(tmp_top10_country_selected_im()) ),
                                               mapping = hcaes(  x = Year, y = Value, group = Country),
                                               type = 'line',
                                               marker = list(symbol = 'circle'), 
                                               visible = c( rep(T,5), rep(F,length( as.character(tmp_top10_country_selected_im()) )-5) )
                                ) %>%
                                hc_xAxis( categories = c( unique( tmp_dtf_market_im_line()$Year) ) ) %>%
                                hc_yAxis( title = list(text = "$ million, NZD"),
                                          labels = list( format = "${value:,.0f} m")  ) %>%
                                hc_plotOptions(line = list(
                                   dataLabels = list(enabled = F),
                                   #stacking = "normal",
                                   enableMouseTracking = T)
                                )%>%
                                hc_tooltip(table = TRUE,
                                           sort = TRUE,
                                           pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                 " {series.name}: ${point.y:,.0f} m"),
                                           headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                ) %>%
                                hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                          )
                          
                          ### 
                          ### 3.4.3 Top markets for selected commodity percent line chart -------------------
                          tmp_dtf_market_im_line_percent <- 
                             reactive({
                                tmp_dtf_market_im_line() %>%
                                   group_by(Year, Type_ie, Type_gs, Note, Commodity) %>%
                                   mutate( Share = Value/sum(Value, na.rm=T)) %>%
                                   ungroup %>%
                                   mutate( Value = Share*100 ) 
                             })
                          
                          output$SelectedImMarketLinePercent <-
                             renderHighchart(
                                highchart() %>%
                                   hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                   hc_add_series( data =  tmp_dtf_market_im_line_percent() %>%
                                                     filter( Country %in% as.character(tmp_top10_country_selected_im()) ),
                                                  mapping = hcaes(  x = Year, y = Value, group = Country),
                                                  type = 'line',
                                                  marker = list(symbol = 'circle'), 
                                                  visible = c( rep(T,5), rep(F,length( as.character(tmp_top10_country_selected_im()) )-5) )
                                   ) %>%
                                   hc_xAxis( categories = c( unique( tmp_dtf_market_im_line_percent()$Year) ) ) %>%
                                   hc_yAxis( title = list(text = "Percentage (%)"),
                                             labels = list( format = "{value:,.1f} %")  ) %>%
                                   hc_plotOptions(line = list(
                                      dataLabels = list(enabled = F),
                                      #stacking = "normal",
                                      enableMouseTracking = T)
                                   )%>%
                                   hc_tooltip(table = TRUE,
                                              sort = TRUE,
                                              pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                    " {series.name}: {point.y:,.1f} %"),
                                              headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                   ) %>%
                                   hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                             )
                          
                          ## !!!!! try UI insert ----------- 
                          insertUI(
                             selector = '#body_ci_markets_im',
                             ui =   div( id = 'body_ci_markets_im_top',
                                         fluidRow( h2(paste0("Top 10 import markets trends") ),
                                                   p("Click on the country names in the legend area to show their trends"),
                                                   column(6, 
                                                          h4("Import values"),
                                                          highchartOutput("SelectedImMarketLine") 
                                                   ),
                                                   column(6,
                                                          h4("As a percent of total imports of the selected"),
                                                          highchartOutput("SelectedImMarketLinePercent")
                                                   )
                                         )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          ### 3.4.4 Growth prospective table ----------------------
                          tmp_tab_im_growth <-
                             reactive({
                                tmp_dtf_market_im_line() %>%
                                   #filter( Country %in% as.character(tmp_top10_country_selected_im()) ) %>%
                                   mutate( Name =  Country ) %>%
                                   group_by( Name) %>%
                                   do( CAGR1 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                        .$Value[.$Year == (max(.$Year)-1)], 1)/100,
                                       CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                        .$Value[.$Year == (max(.$Year)-5)], 5)/100,
                                       CAGR10 =  CAGR( .$Value[.$Year == max(.$Year)]/
                                                          .$Value[.$Year == (max(.$Year)-10)], 10)/100,
                                       ABS5 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)- 5)],
                                       ABS10 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)- 10)]
                                   ) %>%
                                   ungroup %>%
                                   mutate( CAGR1 = as.numeric(CAGR1), 
                                           CAGR5 = as.numeric(CAGR5), 
                                           CAGR10 = as.numeric(CAGR10),
                                           ABS5 = as.numeric(ABS5), 
                                           ABS10 = as.numeric(ABS10) 
                                           ) %>%
                                   #filter( Year == max(Year) ) %>%
                                   left_join( tmp_dtf_market_im_line() %>% rename(Name = Country) %>% filter( Year == max(Year) )  ) %>%
                                   left_join( tmp_dtf_market_im_line_percent() %>% dplyr::select( -Value ) %>% rename( Name = Country) %>% filter( Year == max(Year) )  ) %>%
                                   dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10) %>%
                                   #dplyr::select( Name, CAGR1, CAGR5, CAGR10) %>%
                                   mutate( Name = factor(Name, levels = as.character(tmp_top_country_selected_im()) ) ) %>%
                                   arrange( Name )
                             })
                          
                          output$SelectedImMarketGrowthTab <- renderDataTable(
                             datatable( tmp_tab_im_growth(),
                                        rownames = F,
                                        extensions = 'Buttons',
                                        options = list(dom = 'Bltp',#'Bt', 
                                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1 
                                                       ,scrollX = TRUE
                                                       ,pageLength = 10
                                                       ,lengthMenu = list(c(10,  -1), list('10', 'All'))
                                                       ) ,
                                        colnames=c("Markets",'Value ($m)', 'Share','CAGR 1', 'CAGR 5', 'CAGR 10', 'ABS5', 'ABS10')
                             ) %>%
                                formatStyle(
                                   c('CAGR1', 'CAGR5', 'CAGR10'),
                                   background = styleColorBar( c(0, max(c(tmp_tab_im_growth()$CAGR1,
                                                                          tmp_tab_im_growth()$CAGR5, 
                                                                          tmp_tab_im_growth()$CAGR10))*2, na.rm=T) , 'lightblue'),
                                   backgroundSize = '100% 90%',
                                   backgroundRepeat = 'no-repeat',
                                   backgroundPosition = 'center'
                                ) %>%
                                formatStyle(c('CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'),
                                            color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>%
                                formatPercentage( c('Share','CAGR1', 'CAGR5', 'CAGR10'),digit = 1 ) %>%
                                formatStyle( columns = c('Name','Value','Share','CAGR1', 'CAGR5', 'CAGR10'), `font-size`= '115%' ) %>%
                                formatCurrency( columns = c("Value", 'ABS5', 'ABS10'), mark = ' ', digits = 1)
                          )
                          ## !!!!! try UI insert ----------- 
                          insertUI(
                             selector = '#body_ci_markets_im',
                             ui =   div( id = 'body_ci_markets_im_growth',
                                         fluidRow( h2("Top import markets growth prospective"),
                                                   p("Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years."),
                                                   dataTableOutput("SelectedImMarketGrowthTab")
                                         )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          
                          ## 3.5 show HS groupings -------------------------
                          output$HS_pre_im <- renderDataTable( hs_group,rownames = FALSE, 
                                                               extensions = 'Buttons',
                                                               options = list(dom = 'Bltp', 
                                                                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                                              scrollX = TRUE,
                                                                              pageLength = 5,
                                                                              lengthMenu = list(c(5, -1), list('5', 'All'))  ) 
                          )
                          #shinyjs::show(selector = '#body_appendix_hs_im')
                          
                          ## !!!!! try UI insert ----------- 
                          insertUI(
                             selector = '#body_ci_markets_im',
                             ui =   div( id = 'body_appendix_hs_im',
                                         conditionalPanel("input.rbtn_prebuilt_diy_im == 'Pre-defined'",
                                                          fluidRow( tags$h1("Appendix -- HS grouping selected"),
                                                                    div(id = 'output_hs_pre_im', dataTableOutput( ("HS_pre_im") ) )
                                                          )
                                         ),
                                         
                                         conditionalPanel( "input.rbtn_prebuilt_diy_im == 'Self-defined'",
                                                           fluidRow( tags$h1("Appendix -- HS grouping uploaded"),
                                                                     div(id = 'output_hs_im', dataTableOutput( ("HS_im") ) )
                                                           )
                                                           
                                         )
                             )
                          )
                          ## end Try UI insert --------##
                          
                          
                          ## hide waite message ----
                          shinyjs::hide( id = 'wait_message_ci_im' )
                       }
                       
                       
                       ## 1.3 work on Self defined warning if no .csv HS code and grouping uploaded -------------
                       if(input$rbtn_prebuilt_diy_im=='Self-defined' & is.null(input$file_comodity_im)) {
                          showModal(modalDialog(
                             title = "Warning",
                             tags$b("Please upload an appropriate CSV file with HS codes and groupsings!"),
                             size = 's'
                          ))
                       }
                       
                       ## Now if a csv file is uploaded -- check HS groupings
                       if(input$rbtn_prebuilt_diy_im=='Self-defined' & !is.null(input$file_comodity_im)){
                          ## warning if not a CSV file
                          if( !grepl(".csv",input$file_comodity_im$datapath)){
                             showModal(modalDialog(
                                title = "Warning",
                                tags$b("Only CSV files are accepted!"),
                                size = 's'
                             ))
                             
                          }else{
                             ## read the grouping
                             hs_group <-  read.csv(input$file_comodity_im$datapath, row.names = NULL) 
                             
                             ## check if the first column is HS code
                             tmp_hs_c1 <- gsub("[`]", "", hs_group[,1])
                             if( ncol(hs_group) >2 ){
                                showModal(modalDialog(
                                   title = "Warning",
                                   tags$p("Please check your uploaded HS groupings and make sure", 
                                          tags$b("it contains TWO columns only!")),
                                   size = 's'
                                ))
                             }else if( any( is.na( as.numeric(tmp_hs_c1) )  ) ){
                                showModal(modalDialog(
                                   title = "Warning",
                                   tags$p("Please check your uploaded HS groupings and make sure", 
                                          tags$b("the first column is HS codes!")),
                                   size = 's'
                                ))
                             }else if( any( nchar(tmp_hs_c1) > 6 ) ){
                                showModal(modalDialog(
                                   title = "Warning",
                                   tags$p("Please check your uploaded HS groupings and make sure", 
                                          tags$b("all HS codes are within level 6!") ),
                                   size = 's'
                                ))
                             }else{
                                ## first both checked not passed
                                tmp_execution_self_define <- TRUE
                             }
                             
                             ## 1.3.1 Build graphs self-defined commodity --------------
                             if(tmp_execution_self_define){
                                ## --- hide howto -----
                                shinyjs::hide(id = 'ci_howto_im')
                                ## show waite message ----
                                shinyjs::show( id = 'wait_message_ci_im' )
                                ## disable the buttone ---
                                shinyjs::disable("btn_build_commodity_report_im")
                                ## disable the upload button ---
                                shinyjs::disable("file_comodity_im")
                                shinyjs::disable("rbtn_prebuilt_diy_im")
                                
                                
                                ## make sure the HS codes become characters and HS 1 has 01 format
                                ## standerdise column names
                                colnames(hs_group) <- c("HS_code", "HS_group")
                                ## make columns characters and make sure HS code has 01, and 0122 etc format
                                hs_group %<>%
                                   mutate_all( funs(as.character) ) %>%
                                   mutate( HS_code = gsub("[`]","",HS_code) ) %>%
                                   mutate( HS_code = if_else(nchar(HS_code)%in%c(1,3,5), paste0("0", HS_code), HS_code  )  )
                                
                                ## 3.0.1 Self-define Build the main data.frame -- all selected commodity by country ------
                                tmp_dtf_shiny_full <-
                                   dtf_shiny_full %>%
                                   filter( Type_ie == 'Imports', 
                                           Commodity %in% hs_group$HS_code ) %>%
                                   left_join( concord_country_iso_latlon_raw, by = 'Country' ) %>%
                                   left_join( hs_group, by = c('Commodity' = 'HS_code') ) %>%
                                   group_by( Year, Country, Type_ie, Type_gs, HS_group, ISO2, lat, lon, Note ) %>%
                                   summarise( Value = sum(Value, na.rm=T) ) %>%
                                   ungroup
                                
                                #output$test_full_shiny <- renderDataTable(tmp_dtf_shiny_full)
                                
                                ## commodity only -- sum all countires
                                tmp_dtf_shiny_full_commodity_only <-
                                   tmp_dtf_shiny_full %>%
                                   group_by( Year,  Type_ie, Type_gs, HS_group, Note ) %>%
                                   summarise( Value = sum(Value, na.rm=T) ) %>%
                                   ungroup %>%
                                   mutate( Country = 'World' )
                                
                                ## 3.1 Self-defined: Build import value line chart -------------------- 
                                tmp_top_g_im <-
                                   tmp_dtf_shiny_full_commodity_only %>%
                                   filter( Year == max(Year)) %>% 
                                   arrange( -Value ) %>%
                                   dplyr::select( HS_group ) %>%
                                   as.matrix() %>%
                                   as.character
                                
                                
                                ## top selected commodities and top 5services
                                tmp_top_im <- c( tmp_top_g_im) #, tmp_top_s_ex)
                                
                                ## data frame to plot
                                tmp_dtf_key_line_im <- 
                                   tmp_dtf_shiny_full_commodity_only%>%
                                   filter( HS_group %in% tmp_top_im,
                                           Year >=2007) %>%
                                   mutate( Value = round(Value/10^6),
                                           HS_group = factor(HS_group, levels = tmp_top_im)
                                   ) %>%
                                   arrange( HS_group )
                                
                                ### plot
                                output$CIImportValueLine <- 
                                   renderHighchart(
                                      highchart() %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_xAxis( categories = c( unique( tmp_dtf_key_line_im$Year) ) ) %>%
                                         hc_yAxis( title = list(text = "$ million, NZD"),
                                                   labels = list( format = "${value:,.0f} m")  ) %>%
                                         hc_plotOptions(line = list(
                                            dataLabels = list(enabled = F),
                                            #stacking = "normal",
                                            enableMouseTracking = T #,
                                            #series = list(events = list(legendItemClick = sharelegend)) ,
                                            #showInLegend = T
                                         )
                                         )%>%
                                         hc_tooltip(table = TRUE,
                                                    sort = TRUE,
                                                    pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                          " {series.name}: ${point.y} m"),
                                                    headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                         ) %>%
                                         hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                         hc_add_series( data =  tmp_dtf_key_line_im %>% filter( Type_gs == 'Goods' ) ,
                                                        mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                                                        type = 'line',
                                                        marker = list(symbol = 'circle') #,
                                                        #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                         )
                                   )
                                
                                ## 3.2 Self-defined: build import as a percent of total export line chart -----------------------
                                tmp_tot_im <-
                                   dtf_shiny_full %>%
                                   filter( Country == 'World',
                                           Type_ie == 'Imports',
                                           Year >= 2007 )  %>%
                                   mutate( Value = round(Value/10^6) ) %>%
                                   group_by( Year, Country, Type_ie ) %>%
                                   summarize( Value = sum(Value, na.rm=T) ) %>%
                                   ungroup %>%
                                   mutate( HS_group = 'Total imports' )
                                
                                tmp_dtf_percent_line_im <-
                                   tmp_dtf_key_line_im %>%
                                   bind_rows( tmp_tot_im ) %>%
                                   group_by( Year, Country, Type_ie ) %>%
                                   mutate( Share = Value/Value[HS_group=='Total imports'],
                                           Value = Share*100 ) %>%
                                   ungroup %>%
                                   filter( HS_group != 'Total imports' ) %>%
                                   mutate( HS_group = factor(HS_group, levels = tmp_top_im) ) %>%
                                   arrange( HS_group )
                                
                                # ### plot
                                output$CIImportPercentLine <-
                                   renderHighchart(
                                      highchart() %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_xAxis( categories = c( unique( tmp_dtf_percent_line_im$Year) ) ) %>%
                                         hc_yAxis( title = list(text = "Percentage (%)"),
                                                   labels = list( format = "{value:,.1f} %")  ) %>%
                                         hc_plotOptions(line = list(
                                            dataLabels = list(enabled = F),
                                            #stacking = "normal",
                                            enableMouseTracking = T)
                                         )%>%
                                         hc_tooltip(table = TRUE,
                                                    sort = TRUE,
                                                    pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                          " {series.name}: {point.y:,.1f} %"),
                                                    headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                         ) %>%
                                         hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                         hc_add_series( data =  tmp_dtf_percent_line_im %>% filter( Type_gs == 'Goods' ) ,
                                                        mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                                                        type = 'line',
                                                        marker = list(symbol = 'circle') #,
                                                        #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                         )
                                   )
                                
                                ## !!!!! try UI insert ----------- 
                                if( length(unique(hs_group$HS_group)) < 70 ){
                                   insertUI(
                                      selector = '#body_im_self_defined',
                                      ui =   div( id = 'body_im_line_value_percent_self_defined',
                                                  fluidRow( h1("Imports for selected commodities/services"),
                                                            p("Click on the commodity or service names in the legend area to show their trends"),
                                                            column(6, div(id = "body_value_im", h4("Import values"), highchartOutput('CIImportValueLine') ) ),
                                                            column(6, div(id = "body_percent_im", h4("As a percent of total imports"), highchartOutput('CIImportPercentLine') ) ))
                                      )
                                   )
                                }
                                ## end Try UI insert --------##
                                ## 2.3 Self-defined: build import value change table ----------------
                                ## data frame to plot
                                tmp_dtf_key_tab_im <- 
                                   tmp_dtf_shiny_full_commodity_only %>%
                                   filter( HS_group %in% tmp_top_im) %>%
                                   mutate( HS_group = factor(HS_group, levels = tmp_top_im) ) %>%
                                   arrange( HS_group )
                                
                                
                                tmp_tab <-
                                   tmp_dtf_key_tab_im %>%
                                   mutate( Name =  HS_group ) %>%
                                   group_by( Name) %>%
                                   do(CAGR1 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                       .$Value[.$Year == (max(.$Year)-1)], 1)/100,
                                      CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                       .$Value[.$Year == (max(.$Year)-5)], 5)/100,
                                      CAGR10 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                        .$Value[.$Year == (max(.$Year)-10)], 10)/100 ,
                                      ABS5 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)-5)],
                                      ABS10 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)-10)]
                                      ) %>%
                                   ungroup %>%
                                   mutate( CAGR1 = as.numeric(CAGR1),
                                           CAGR5 = as.numeric(CAGR5),
                                           CAGR10 = as.numeric(CAGR10),
                                           ABS5 = as.numeric(ABS5),
                                           ABS10 = as.numeric(ABS10)
                                           ) %>%
                                   left_join( tmp_dtf_key_tab_im , 
                                              by =c('Name'='HS_group') ) %>%
                                   left_join( tmp_dtf_percent_line_im %>% dplyr::select( -Value) %>% rename(Name = HS_group) ) %>%
                                   filter( Year == max(Year) ) %>%
                                   mutate( Value = Value/10^6, ABS5 = ABS5/10^6, ABS10 = ABS10/10^6 ) %>%
                                   dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10) %>%
                                   mutate( Name = factor(Name, levels = tmp_top_im),
                                           CAGR1 = ifelse(CAGR1 %in% c(Inf,-Inf), NA, CAGR1),
                                           CAGR5 = ifelse(CAGR5 %in% c(Inf,-Inf), NA, CAGR5),
                                           CAGR10 = ifelse(CAGR10 %in% c(Inf,-Inf), NA, CAGR10)
                                   ) %>%
                                   arrange( Name )
                                
                                ### join back to hs code
                                hs_group_flat <- 
                                   hs_group %>%
                                   group_by( HS_group ) %>%
                                   summarise( HS_code = paste0(HS_code, collapse = '; ') ) %>%
                                   ungroup
                                
                                tmp_tab %<>%
                                   left_join( hs_group_flat, by = c("Name"= 'HS_group') ) %>%
                                   dplyr::select( HS_code, Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10 )
                                

                                output$GrowthTabSelectedIm <- renderDataTable(
                                   datatable( tmp_tab,
                                              rownames = F,
                                              filter = c("top"),
                                              extensions = 'Buttons',
                                              options = list(dom = 'Bfltp',# 'Bt', 
                                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1 
                                                             ,scrollX = TRUE
                                                             #,fixedColumns = list(leftColumns = 2) 
                                                             ,autoWidth = T
                                                             ,pageLength = 10
                                                             ,lengthMenu = list(c(10,  -1), list('10', 'All')) ,
                                                             searchHighlight = TRUE,
                                                             search = list(regex = TRUE, caseInsensitive = FALSE )
                                                             ) ,
                                              colnames=c("HS codes", "Classification" ,'Value ($m)', 'Share of total imports','CAGR 1', 'CAGR 5', 'CAGR 10', 'ABS5', 'ABS10')
                                   ) %>%
                                      formatStyle(
                                         c('CAGR1', 'CAGR5', 'CAGR10'),
                                         background = styleColorBar( c(0, max(c(tmp_tab$CAGR1,tmp_tab$CAGR5, tmp_tab$CAGR10))*2, na.rm=T) , 'lightblue'),
                                         backgroundSize = '100% 90%',
                                         backgroundRepeat = 'no-repeat',
                                         backgroundPosition = 'center'
                                      ) %>%
                                      formatStyle(c('CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'),
                                                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>%
                                      formatPercentage( c('Share','CAGR1', 'CAGR5', 'CAGR10'),digit = 1 ) %>%
                                      formatStyle( columns = c('Name','Value', 'Share','CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'), `font-size`= '115%' ) %>%
                                      formatCurrency( columns = c('Value', 'ABS5', 'ABS10'), mark = ' ', digits = 1)
                                )
                                
                                ## !!!!! try UI insert ----------- 
                                insertUI(
                                   selector = '#body_growth_im_self_defined',
                                   ui =   div( id = 'body_im_growth_tab_self_defined',
                                               fluidRow( h1("Short, medium, and long term growth for selected commodities/services"),
                                                         p("Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years."),
                                                         dataTableOutput('GrowthTabSelectedIm')
                                               )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                ## 3.4 Self-defined: Build import by country output groups -------------------
                                ## create a selector for each selected commodity ----------------
                                output$CIIMSelectorByMarkets <- renderUI({
                                   selectizeInput("select_comodity_im_for_market_analysis",
                                                  tags$p("Please select or search a commodity/service for its market analysis"), 
                                                  choices =  tmp_tab$Name[input$GrowthTabSelectedIm_rows_all], #tmp_top_im, 
                                                  selected = tmp_top_im[1],  width = "500px",
                                                  multiple = F)
                                })
                                
                                ### build data for market analysis -- these has to be reactive values
                                ## The name of the selected commodity
                                tmp_selected_im <- 
                                   reactive({
                                      input$select_comodity_im_for_market_analysis
                                   })
                                
                                ## The HS codes of the selected commodity
                                tmp_hs_im <- 
                                   reactive({
                                      hs_group$HS_code[hs_group$HS_group == tmp_selected_im()]
                                   })
                                
                                ## The data from of the selected commodity by markets
                                tmp_dtf_market_im <- 
                                   reactive({
                                      dtf_shiny_full %>%
                                         filter( Commodity %in% tmp_hs_im(), 
                                                 Year >= 2007,
                                                 Type_ie == 'Imports') %>%
                                         left_join( concord_country_iso_latlon_raw, by = 'Country' ) %>%
                                         group_by( Year, Country, Type_ie, Type_gs, Note, ISO2, lat, lon ) %>%
                                         summarize( Value = sum(Value, na.rm=T) ) %>%
                                         ungroup %>%
                                         mutate( Commodity = as.character( tmp_selected_im() ) )
                                   })
                                
                                ### selcted commodity and service outputs
                                output$SelectedIm <- 
                                   renderText({
                                      tmp_selected_im()
                                   })
                                
                                ## !!!!! try UI insert ----------- 
                                insertUI(
                                   selector = '#body_ci_markets_im_self_defined',
                                   ui =   div( id = 'body_ci_markets_im_selector_self_defined',
                                               fluidRow(h1("Import markets analysis for selected commodity/service"),
                                                        uiOutput("CIIMSelectorByMarkets") ),
                                               fluidRow( shiny::span(h1( HTML(paste0(textOutput("SelectedIm"))), align = "center" ), style = "color:darkblue" ) )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                ### 3.4.0 Value Line and Percentage line for selected commodities ----------------
                                tmp_dtf_line_selected_im <-
                                   reactive({
                                      tmp_dtf_key_line_im %>%
                                         filter( HS_group %in% as.character( tmp_selected_im() ) )
                                   })
                                
                                ### plot
                                output$CISelectedImportValueLine <- 
                                   renderHighchart(
                                      highchart() %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_xAxis( categories = c( unique( tmp_dtf_line_selected_im()$Year) ) ) %>%
                                         hc_yAxis( title = list(text = "$ million, NZD"),
                                                   labels = list( format = "${value:,.0f} m")  ) %>%
                                         hc_plotOptions(line = list(
                                            dataLabels = list(enabled = F),
                                            #stacking = "normal",
                                            enableMouseTracking = T #,
                                            #series = list(events = list(legendItemClick = sharelegend)) ,
                                            #showInLegend = T
                                         )
                                         )%>%
                                         hc_tooltip(table = TRUE,
                                                    sort = TRUE,
                                                    pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                          " {series.name}: ${point.y} m"),
                                                    headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                         ) %>%
                                         hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                         hc_add_series( data =  tmp_dtf_line_selected_im() %>% filter( Type_gs == 'Goods' ) ,
                                                        mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                                                        type = 'line',
                                                        marker = list(symbol = 'circle') #,
                                                        #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                         )
                                   )
                                
                                ## percentage line
                                tmp_dtf_percent_selected_line_im <-
                                   reactive({
                                      tmp_dtf_percent_line_im %>%
                                         filter( HS_group %in% as.character( tmp_selected_im() ) )
                                   })
                                
                                # ### plot
                                output$CISelectedImportPercentLine <-
                                   renderHighchart(
                                      highchart() %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_xAxis( categories = c( unique( tmp_dtf_percent_selected_line_im()$Year) ) ) %>%
                                         hc_yAxis( title = list(text = "Percentage (%)"),
                                                   labels = list( format = "{value:,.1f} %")  ) %>%
                                         hc_plotOptions(line = list(
                                            dataLabels = list(enabled = F),
                                            #stacking = "normal",
                                            enableMouseTracking = T)
                                         )%>%
                                         hc_tooltip(table = TRUE,
                                                    sort = TRUE,
                                                    pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                          " {series.name}: {point.y:,.1f} %"),
                                                    headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                         ) %>%
                                         hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                         hc_add_series( data =  tmp_dtf_percent_selected_line_im() %>% filter( Type_gs == 'Goods' ) ,
                                                        mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                                                        type = 'line',
                                                        marker = list(symbol = 'circle') #,
                                                        #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                         )
                                   )
                                
                                ## !!!!! try UI insert ----------- 
                                insertUI(
                                   selector = '#body_ci_markets_im_self_defined',
                                   ui =   div( id = 'body_selected_im_line_value_percent_self_defined',
                                               fluidRow( h1("Imports trend"),
                                                         p("Click on the commodity or service names in the legend area to show their trends"),
                                                         column(6, div(id = "body_value_selected_im", h4("Import values"), highchartOutput('CISelectedImportValueLine') ) ),
                                                         column(6, div(id = "body_percent_selected_im", h4("As a percent of total imports"), highchartOutput('CISelectedImportPercentLine') ) ))
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                ### 3.4.1 Self-defined: build highchart map  ---------------------------
                                tmp_dtf_market_im_map <- 
                                   reactive({
                                      tmp_dtf_market_im() %>%
                                         filter( Year == max(Year),
                                                 !is.na(lat) ) %>%
                                         mutate( Value = Value/10^6,
                                                 z= Value,
                                                 name = Country)
                                   })
                                
                                ## plot map
                                output$MapIMMarket <- 
                                   renderHighchart({
                                      hcmap( data = tmp_dtf_market_im_map() ,
                                             value = 'Value',
                                             joinBy = c('iso-a2','ISO2'), 
                                             name="Imports value",
                                             borderWidth = 1,
                                             borderColor = "#fafafa",
                                             nullColor = "lightgrey",
                                             tooltip = list( table = TRUE,
                                                             sort = TRUE,
                                                             headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                             pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
                                      ) %>%
                                         hc_add_series(data =  tmp_dtf_market_im_map(),
                                                       type = "mapbubble",
                                                       color  = hex_to_rgba("#f1c40f", 0.9),
                                                       minSize = 0,
                                                       name="Imports value",
                                                       maxSize = 30,
                                                       tooltip = list(table = TRUE,
                                                                      sort = TRUE,
                                                                      headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                                                      pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
                                         ) %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_legend( enabled=FALSE ) %>% 
                                         hc_mapNavigation(enabled = TRUE) 
                                   })
                                
                                ## !!!!! try UI insert ----------- 
                                insertUI(
                                   selector = '#body_ci_markets_im_self_defined',
                                   ui =   div( id = 'body_ci_markets_im_map_self_defined',
                                               fluidRow(h2( paste0("Map of import values")  ) ,
                                                        p("The size of bubble area and color both represent the value of imports."),
                                                        highchartOutput('MapIMMarket') )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                ### 3.4.2 Self-defined: Top markets for selected commodity line chart ----------------
                                tmp_top_country_selected_im <- 
                                   reactive({
                                      tmp_dtf_market_im() %>%
                                         filter( Year == max(Year),
                                                 Value > 0 , 
                                                 !Country %in% c("World", 
                                                                 "Destination Unknown - EU")
                                         ) %>% ## 1 bn commodity
                                         arrange( -Value ) %>%
                                         dplyr::select( Country ) %>%
                                         as.matrix() %>%
                                         as.character
                                   })
                                

                                tmp_top10_country_selected_im <-
                                   reactive({
                                      tmp_top_country_selected_im()[1:min(10,length(tmp_top_country_selected_im()))]
                                   })
                                
                                ## test the see top countries
                                # output$test_top_country_ex <- 
                                #    renderText({
                                #       tmp_top_country_selected_ex()
                                #    })
                                
                                ### derive datafrom for the line plot
                                tmp_dtf_market_im_line <- 
                                   reactive({
                                      tmp_dtf_market_im() %>%
                                         filter( Country %in%  as.character(tmp_top_country_selected_im()) ) %>%
                                         mutate( Value = Value/10^6 ,
                                                 Country = factor(Country, levels = as.character(tmp_top_country_selected_im()) )
                                         ) %>%
                                         arrange(Country)
                                   })
                                
                                ## test the see top countries
                                # output$test_top_country_ex_dtf <- 
                                #    renderDataTable({
                                #       tmp_dtf_market_ex_line()
                                #    })
                                
                                ## line plot
                                output$SelectedImMarketLine <- renderHighchart(
                                   highchart() %>%
                                      hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                      hc_add_series( data =  tmp_dtf_market_im_line() %>%
                                                        filter( Country %in% as.character(tmp_top10_country_selected_im()) ),
                                                     mapping = hcaes(  x = Year, y = Value, group = Country),
                                                     type = 'line',
                                                     marker = list(symbol = 'circle'), 
                                                     visible = c( rep(T,5), rep(F,length( as.character(tmp_top10_country_selected_im()) )-5) )
                                      ) %>%
                                      hc_xAxis( categories = c( unique( tmp_dtf_market_im_line()$Year) ) ) %>%
                                      hc_yAxis( title = list(text = "$ million, NZD"),
                                                labels = list( format = "${value:,.0f} m")  ) %>%
                                      hc_plotOptions(line = list(
                                         dataLabels = list(enabled = F),
                                         #stacking = "normal",
                                         enableMouseTracking = T)
                                      )%>%
                                      hc_tooltip(table = TRUE,
                                                 sort = TRUE,
                                                 pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                       " {series.name}: ${point.y:,.0f} m"),
                                                 headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                      ) %>%
                                      hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                                )
                                
                                
                                ### 3.4.3 Self-defined: Top markets for selected commodity percent line chart -------------------
                                tmp_dtf_market_im_line_percent <- 
                                   reactive({
                                      tmp_dtf_market_im_line() %>%
                                         group_by(Year, Type_ie, Type_gs, Note, Commodity) %>%
                                         mutate( Share = Value/sum(Value, na.rm=T)) %>%
                                         ungroup %>%
                                         mutate( Value = Share*100 ) 
                                   })
                                
                                output$SelectedImMarketLinePercent <-
                                   renderHighchart(
                                      highchart() %>%
                                         hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                         hc_add_series( data =  tmp_dtf_market_im_line_percent() %>%
                                                           filter( Country %in% as.character(tmp_top10_country_selected_im()) ),
                                                        mapping = hcaes(  x = Year, y = Value, group = Country),
                                                        type = 'line',
                                                        marker = list(symbol = 'circle'), 
                                                        visible = c( rep(T,5), rep(F,length( as.character(tmp_top10_country_selected_im()) )-5) )
                                         ) %>%
                                         hc_xAxis( categories = c( unique( tmp_dtf_market_im_line_percent()$Year) ) ) %>%
                                         hc_yAxis( title = list(text = "Percentage (%)"),
                                                   labels = list( format = "{value:,.1f} %")  ) %>%
                                         hc_plotOptions(line = list(
                                            dataLabels = list(enabled = F),
                                            #stacking = "normal",
                                            enableMouseTracking = T)
                                         )%>%
                                         hc_tooltip(table = TRUE,
                                                    sort = TRUE,
                                                    pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                          " {series.name}: {point.y:,.1f} %"),
                                                    headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                         ) %>%
                                         hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                                   )
                                
                                ## !!!!! try UI insert ----------- 
                                insertUI(
                                   selector = '#body_ci_markets_im_self_defined',
                                   ui =   div( id = 'body_ci_markets_im_top_self_defined',
                                               fluidRow( h2(paste0("Top 10 import markets trends") ),
                                                         p("Click on the country names in the legend area to show their trends"),
                                                         column(6, 
                                                                h4("Import values"),
                                                                highchartOutput("SelectedImMarketLine") 
                                                         ),
                                                         column(6,
                                                                h4("As a percent of total imports of the selected"),
                                                                highchartOutput("SelectedImMarketLinePercent")
                                                         )
                                               )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                ### 3.4.4 Self-defined: Growth prospective tab ----------------------
                                tmp_tab_im_growth <-
                                   reactive({
                                      tmp_dtf_market_im_line() %>%
                                         #filter( Country %in% as.character(tmp_top10_country_selected_im()) ) %>%
                                         mutate( Name =  Country ) %>%
                                         group_by( Name) %>%
                                         do( CAGR1 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                              .$Value[.$Year == (max(.$Year)-1)], 1)/100,
                                             CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                                              .$Value[.$Year == (max(.$Year)-5)], 5)/100,
                                             CAGR10 =  CAGR( .$Value[.$Year == max(.$Year)]/
                                                                .$Value[.$Year == (max(.$Year)-10)], 10)/100,
                                             ABS5 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)- 5)],
                                             ABS10 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)- 10)]
                                         ) %>%
                                         ungroup %>%
                                         mutate( CAGR1 = as.numeric(CAGR1), 
                                                 CAGR5 = as.numeric(CAGR5), 
                                                 CAGR10 = as.numeric(CAGR10),
                                                 ABS5 = as.numeric(ABS5),
                                                 ABS10 = as.numeric(ABS10) ) %>%
                                         #filter( Year == max(Year) ) %>%
                                         left_join( tmp_dtf_market_im_line() %>% rename(Name = Country) %>% filter( Year == max(Year) )  ) %>%
                                         left_join( tmp_dtf_market_im_line_percent() %>% dplyr::select( -Value ) %>% rename( Name = Country) %>% filter( Year == max(Year) )  ) %>%
                                         dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10) %>%
                                         #dplyr::select( Name, CAGR1, CAGR5, CAGR10) %>%
                                         mutate( Name = factor(Name, levels = as.character(tmp_top_country_selected_im()) ) ) %>%
                                         arrange( Name )
                                   })
                                
                                output$SelectedImMarketGrowthTab <- renderDataTable(
                                   datatable( tmp_tab_im_growth(),
                                              rownames = F,
                                              extensions = 'Buttons',
                                              options = list(dom = 'Bltp',#'Bt', 
                                                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1 
                                                             ,scrollX = TRUE
                                                             ,pageLength = 10
                                                             ,lengthMenu = list(c(10,  -1), list('10', 'All'))
                                                             ) ,
                                              colnames=c("Markets",'Value ($m)', 'Share of world market','CAGR 1', 'CAGR 5', 'CAGR 10', 'ABS5', 'ABS10')
                                   ) %>%
                                      formatStyle(
                                         c('CAGR1', 'CAGR5', 'CAGR10'),
                                         background = styleColorBar( c(0, max(c(tmp_tab_im_growth()$CAGR1,
                                                                                tmp_tab_im_growth()$CAGR5, 
                                                                                tmp_tab_im_growth()$CAGR10))*2, na.rm=T) , 'lightblue'),
                                         backgroundSize = '100% 90%',
                                         backgroundRepeat = 'no-repeat',
                                         backgroundPosition = 'center'
                                      ) %>%
                                      formatStyle(c('CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'),
                                                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>%
                                      formatPercentage( c('Share','CAGR1', 'CAGR5', 'CAGR10'),digit = 1 ) %>%
                                      formatStyle( columns = c('Name', 'Value','Share' ,'CAGR1', 'CAGR5', 'CAGR10'), `font-size`= '115%' ) %>%
                                      formatCurrency( columns = c('Value', 'ABS5', 'ABS10'), mark = ' ', digits = 1)
                                )
                                
                                ## !!!!! try UI insert ----------- 
                                insertUI(
                                   selector = '#body_ci_markets_im_self_defined',
                                   ui =   div( id = 'body_ci_markets_im_growth_self_defined',
                                               fluidRow( h2("Top import markets growth prospective"),
                                                         p("Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years."),
                                                         dataTableOutput("SelectedImMarketGrowthTab")
                                               )
                                   )
                                )
                                ## end Try UI insert --------##
                                
                                
                                ## 3.5 Self-defined: show HS groupings in appendix -------------------
                                output$HS_im <- renderDataTable( hs_group,rownames = FALSE, 
                                                                 extensions = 'Buttons',
                                                                 options = list(dom = 'Bltp', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                                                pageLength = 5,
                                                                                lengthMenu = list(c(5,  -1), list('5', 'All')) 
                                                                 ) 
                                )
                                
                                ## !!!!! try UI insert ----------- 
                                insertUI(
                                   selector = '#body_ci_markets_im_self_defined',
                                   ui =   div( id = 'body_appendix_hs_im_self_defined',
                                               conditionalPanel("input.rbtn_prebuilt_diy_im == 'Pre-defined'",
                                                                fluidRow( tags$h1("Appendix -- HS grouping selected"),
                                                                          div(id = 'output_hs_pre_im', dataTableOutput( ("HS_pre_im") ) )
                                                                )
                                               ),
                                               
                                               conditionalPanel( "input.rbtn_prebuilt_diy_im == 'Self-defined'",
                                                                 fluidRow( tags$h1("Appendix -- HS grouping uploaded"),
                                                                           div(id = 'output_hs_im', dataTableOutput( ("HS_im") ) )
                                                                 )
                                                                 
                                               )
                                   )
                                )
                                ## end Try UI insert --------##
                                ## show waite message ----
                                shinyjs::hide( id = 'wait_message_ci_im' )
                             }
                          }
                       }
                    }
           )
      
      
      ## III. Country intelligence ------------------------
      observeEvent(input$btn_build_country_report,
                   {
                      tmp_execution <- FALSE
                      
                      if(is.null(input$select_country)) {
                         showModal(modalDialog(
                            title = "Warning",
                            tags$b("Please select one or multiple countries or a country group!"),
                            size = 's'
                         ))
                      }
                      
                      ## III.0 select country or country group prerequisit ------------------------
                      ## One can either select one or multiple countries, OR only one country group
                      if( any(input$select_country %in% list_country[['Country groups']]) &
                          length(input$select_country)>1 ){
                         showModal(modalDialog(
                            title = "Warning",
                            tags$b("You can select only ONE of the country groups!"),
                            size = 's'
                         ))
                      }
                      
                      ##  a country group selected
                      if( any(input$select_country %in% list_country[['Country groups']]) & length(input$select_country)==1 ){
                         tmp_selected_countries <- concord_country_member$Country[concord_country_member$Group==input$select_country]
                         tmp_execution <- TRUE
                         tmp_single_country <- FALSE
                         output$SelectedMarketMultiple <-
                            renderText({paste0("Market group selected: ",input$select_country)})
                      }
                      
                      # multiple countries selected
                      if( !any(input$select_country %in% list_country[['Country groups']]) & length(input$select_country)>1 ){
                         tmp_selected_countries <- input$select_country
                         tmp_execution <- TRUE
                         tmp_single_country <- FALSE
                         output$SelectedMarketMultiple <-
                            renderText({paste0( length(which(tmp_selected_countries!='New Zealand')) , " markets selected")})
                      }
                      
                      ## only one country selected!
                      if(!any(input$select_country %in% list_country[['Country groups']]) &  length(input$select_country)==1 ) {
                         tmp_selected_countries <- input$select_country
                         tmp_execution <- TRUE
                         tmp_single_country <- TRUE
                         output$SelectedMarketSingle <-
                            renderText({paste0('Single market selected: ',tmp_selected_countries)})
                      }

                      ### work on next only when the inputs are correct!!!
                      if( tmp_execution ){
                         ## hide howto ----
                         shinyjs::hide(id = 'country_howto')
                         ## show wait message ----
                         shinyjs::show( id = 'wait_message_country_intel' )
                         ## disable a button -----
                         shinyjs::disable("btn_build_country_report")
                         ## disable a country selection button -----
                         shinyjs::disable("select_country")
                         
                         ## !!!!!!!!!!!!!!!! insert UI country name --------------------
                         insertUI(
                            selector = "#country_name",
                            ui = div(
                               id = 'country_name_single_or_multiple',
                               conditionalPanel( "input.select_country.length == 1 &&
                                                 input.select_country.valueOf() != 'APEC' &&
                                                 input.select_country.valueOf() != 'EU28' &&
                                                 input.select_country.valueOf() != 'CPTPP' &&
                                                 input.select_country.valueOf() != 'GCC' &&
                                                 input.select_country.valueOf() != 'Pacific Islands Forum' &&
                                                 input.select_country.valueOf() != 'ASEAN' &&
                                                 input.select_country.valueOf() != 'OECD' &&
                                                 input.select_country.valueOf() != 'Five Eyes' &&
                                                 input.select_country.valueOf() != 'Latin America' &&
                                                 input.select_country.valueOf() != 'OPEC' &&
                                                 input.select_country.valueOf() != 'FTA in force' &&
                                                 input.select_country.valueOf() != 'Middle East' && 
                                                 input.select_country.valueOf() != 'Northern Africa' && 
                                                 input.select_country.valueOf() != 'Eastern Africa' && 
                                                 input.select_country.valueOf() != 'Central Africa' && 
                                                 input.select_country.valueOf() != 'Southern Africa' && 
                                                 input.select_country.valueOf() != 'Western Africa' && 
                                                 input.select_country.valueOf() != 'Africa' && 
                                                 input.select_country.valueOf() != 'Arab Maghreb Union' &&
                                                 input.select_country.valueOf() != 'Eastern African Community' &&
                                                 input.select_country.valueOf() != 'Economic Community of West African States' &&
                                                 input.select_country.valueOf() != 'Southern African Development Community' &&
                                                 input.select_country.valueOf() != 'G7' &&
                                                 input.select_country.valueOf() != 'BRI countries' " ,
                                                 fluidRow( shiny::span(h1( HTML(paste0(textOutput("SelectedMarketSingle"))), align = "center" ), style = "color:darkblue" ) )
                               ),
                               
                               conditionalPanel( "input.select_country.length > 1 || 
                                              input.select_country.valueOf() == 'APEC' || 
                                              input.select_country.valueOf() == 'EU28'||
                                              input.select_country.valueOf() == 'CPTPP' ||
                                              input.select_country.valueOf() == 'GCC' ||
                                                 input.select_country.valueOf() == 'Pacific Islands Forum' ||
                                                 input.select_country.valueOf() == 'ASEAN' ||
                                                 input.select_country.valueOf() == 'OECD' ||
                                                 input.select_country.valueOf() == 'Five Eyes' ||
                                                 input.select_country.valueOf() == 'Latin America' ||
                                                 input.select_country.valueOf() == 'OPEC' ||
                                                 input.select_country.valueOf() == 'FTA in force' ||
                                                 input.select_country.valueOf() == 'Middle East' || 
                                                 input.select_country.valueOf() == 'Northern Africa' || 
                                                 input.select_country.valueOf() == 'Eastern Africa' || 
                                                 input.select_country.valueOf() == 'Central Africa' || 
                                                 input.select_country.valueOf() == 'Southern Africa' || 
                                                 input.select_country.valueOf() == 'Western Africa' || 
                                                 input.select_country.valueOf() == 'Africa' || 
                                                 input.select_country.valueOf() == 'Arab Maghreb Union' ||
                                                 input.select_country.valueOf() == 'Eastern African Community' ||
                                                 input.select_country.valueOf() == 'Economic Community of West African States' ||
                                                 input.select_country.valueOf() == 'Southern African Development Community' ||
                                                 input.select_country.valueOf() == 'G7' ||
                                                 input.select_country.valueOf() == 'BRI countries' ",
                                               fluidRow( shiny::span(h1( HTML(paste0(textOutput("SelectedMarketMultiple"))), align = "center" ), style = "color:darkblue" ) )

                               )
                            )
                         )
                         #shinyjs::hide( id = 'wait_message' )
                         ## III.1 Basic country info table ---------------------------------
                         # ### define the select country
                         print("------------------  Basic country tables -------------------------")
                         dtf_select_country <- 
                            data.frame(Country = tmp_selected_countries) %>% 
                            mutate( Country = as.character(tmp_selected_countries) )

                         dtf_select_country <-
                            dtf_country_group %>%
                            right_join( dtf_select_country, by = 'Country' ) %>%
                            dplyr::select( -Region ) %>%
                            left_join( flag_table ) %>%
                            mutate( Flag_img = paste0( "<img src='",
                                                       Flag_link,
                                                       "' height = '21' width = '42'>", "</img>") ) %>%
                            dplyr::select( Country, Flag = Flag_img, ISO2 )

                         #dtf_selected_country_map <- dtf_select_country[,c('Country','ISO2')]

                         ### get population -- if the country does not have any data
                         print("------------------  Basic country tables - get population -------------------------")
                         pop_download_fail <- try(
                             tmp_population <-
                                WDI(indicator='SP.POP.TOTL',
                                    country = dtf_select_country$ISO2,
                                    start=2014, end = max(dtf_shiny_full$Year))
                         )

                         if( class(pop_download_fail)=='try-error' ){
                            tmp_population <-
                               data.frame( iso2c = dtf_select_country$ISO2,
                                           country = dtf_select_country$Country,
                                           `SP.POP.TOTL` = NA,
                                           year = max(dtf_shiny_full$Year) ) %>%
                               dplyr::select( ISO2 = iso2c, `Population` =  'SP.POP.TOTL')
                         }else{
                            tmp_population <-
                               #WDI(indicator='SP.POP.TOTL',
                               #    country = dtf_select_country$ISO2,
                               #    start=2014, end = max(dtf_shiny_full$Year)) %>%
                               tmp_population %>%
                               filter(!is.na(`SP.POP.TOTL`) ) %>%
                               filter( year == max(year)) %>%
                               dplyr::select( ISO2 = iso2c, `Population` =  'SP.POP.TOTL') %>%
                               mutate( Population = Population/10^3 )
                         }


                         dtf_select_country %<>%
                            left_join(  tmp_population, by = 'ISO2' )

                         ### get gdp per capita and population data from Worldbank
                         print("------------------  Basic country tables - get GDP -------------------------")
                         gdp_download_fail <- try(
                             tmp_gdp_per_cap <-
                                WDI(indicator='NY.GDP.PCAP.CD',
                                    country = dtf_select_country$ISO2,
                                    start=2014, end = max(dtf_shiny_full$Year))
                         )

                         if( class(gdp_download_fail)=='try-error' ){
                            tmp_gdp_per_cap <-
                               data.frame( iso2c = dtf_select_country$ISO2,
                                           country = dtf_select_country$Country,
                                           `NY.GDP.PCAP.CD` = NA,
                                           year = max(dtf_shiny_full$Year) ) %>%
                               dplyr::select( ISO2 = iso2c, `GDP per capita` =  'NY.GDP.PCAP.CD')
                         }else{
                            tmp_gdp_per_cap <-
                               #WDI(indicator='SP.POP.TOTL',
                               #    country = dtf_select_country$ISO2,
                               #    start=2014, end = max(dtf_shiny_full$Year)) %>%
                               tmp_gdp_per_cap %>%
                               filter(!is.na(`NY.GDP.PCAP.CD`) ) %>%
                               filter( year == max(year)) %>%
                               dplyr::select( ISO2 = iso2c, `GDP per capita` =  'NY.GDP.PCAP.CD')
                         }


                         dtf_select_country %<>%
                            left_join(  tmp_gdp_per_cap, by = 'ISO2' )

                         ### get nearest distance to NZ
                         print("------------------  Basic country tables - get distance -------------------------")
                         dtf_select_country$`Distance to NZ` <- NA

                         for( i_country in 1:nrow(dtf_select_country) ){
                            print(dtf_select_country$Country[i_country])
                            if( dtf_select_country$Country[i_country] != "Destination Unknown - EU" ){
                               tmp_distance <- distm( concord_country[ concord_country$ISO2=='NZ' ,c('lon','lat')],
                                                      concord_country[ concord_country$ISO2 %in% dtf_select_country$ISO2[i_country]  ,c('lon','lat')])
                               tmp_distance <- round( as.numeric(tmp_distance)/1000, -2)
                               dtf_select_country$`Distance to NZ`[i_country] <- tmp_distance
                            }else{
                               dtf_select_country$`Distance to NZ`[i_country] <- 17880 #https://www.distancefromto.net/distance-from/New+Zealand/to/Europe
                            }
                         }

                         ## sort by population
                         dtf_select_country %<>%
                            ## World bank does not provide Taiwan data. we get data from 'https://eng.stat.gov.tw/ct.asp?xItem=41871&ctNode=2265&mp=5'
                            mutate( Population = ifelse(Country=='Taiwan',23540, Population ),
                                    `GDP per capita` = ifelse( Country=='Taiwan', 25119, `GDP per capita`) ) %>%
                            arrange( -Population )

                         ## generate map data
                         print("------------------  Basic country tables - build data for map -------------------------")
                         dtf_select_country_map <-
                            left_join(dtf_select_country,
                                      concord_country %>% dplyr::select(-Country),
                                      by = 'ISO2') %>%
                            mutate( z = 1, name = Country )

                         ### data for table
                         if( tmp_single_country ){
                            dtf_select_country %<>%
                               dplyr::select( -ISO2 )
                         }else{
                            dtf_select_country %<>%
                               dplyr::select( -ISO2 ) %>%
                               bind_rows( data.frame( Country='Total selected markets',
                                                      Flag = '',
                                                      Population = sum(dtf_select_country$Population, na.rm=T) ,
                                                      `GDP per capita` = sum( (dtf_select_country$Population/
                                                                                  sum(dtf_select_country$Population, na.rm=T)
                                                                               ) * dtf_select_country$`GDP per capita`, na.rm=T ),
                                                      `Distance to NZ` = mean( dtf_select_country$`Distance to NZ` , na.rm=T),
                                                      check.names = FALSE
                                                      )
                               )
                         }

                         ## render a country table
                         output$CountryTable <-
                            renderDataTable({
                               datatable( dtf_select_country,
                                          escape=FALSE,
                                          rownames = F,
                                          colnames=c("","", "Population<br>('000)" ,
                                                     "GDP per capita<br>(current US$)",
                                                     "Distance to NZ<br>(KM)"
                                          ),
                                          options = list(dom = 'ltp',
                                                         scrollX = TRUE, 
                                                         pageLength = 5,
                                                         lengthMenu = list(c(5,  -1), list('5', 'All'))  )
                               ) %>%
                                  formatCurrency( c("GDP per capita"), digits = 0, mark = ' ' ) %>%
                                  formatCurrency( c('Population'), digits = 0, mark = ' ', currency = '' ) %>%
                                  formatCurrency( c('Distance to NZ'), digits = 0, mark = ' ', currency = '' )
                            })


                         ## III.2 Map of selected countries -----------------
                         print("------------------  Country maps -------------------------")
                         output$MapSelectedCountry <-
                            renderHighchart({
                               base_selected_country_map <-
                                  hcmap( data = dtf_select_country_map ,#%>% mutate( Selected = 1 ),
                                         #value = 'Selected',
                                         value = 'z',
                                         joinBy = c('iso-a2','ISO2'),
                                         name="Selected market",
                                         borderWidth = 1,
                                         borderColor = "#fafafa",
                                         nullColor = "lightgrey" #,
                                         # tooltip = list( table = TRUE,
                                         #                 sort = TRUE,
                                         #                 headerFormat = '',
                                         #                 pointFormat = '{point.name}' ),
                                         # dataLabels = list(enabled=F)
                                  )%>%
                                  hc_add_series( data = dtf_select_country_map %>% dplyr::select(-name),
                                                 type = "mappoint",
                                                 color  = hex_to_rgba("#00ff00", 0.9),
                                                 marker = list( radius = 2 ),
                                                 dataLables = list(enabled=F),
                                                 #minSize = 0,
                                                 name="" #,#,
                                                 #maxSize = 4 #,
                                                 # tooltip = list(table = TRUE,
                                                 #               sort = TRUE,
                                                 #               headerFormat = '',
                                                 #               pointFormat = '{point.name}')
                                                 # dataLabels = list(enabled=T,
                                                 #                   format="{point.name}",
                                                 #                   style = list(fontSize = '10px', fontWeight = 'normal', color = 'white')
                                                 #)
                                  ) %>%
                                  hc_legend( enabled=FALSE ) %>%
                                  hc_tooltip( enabled = F) %>%
                                  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                  hc_mapNavigation(enabled = TRUE)

                               if( length(input$CountryTable_rows_selected)==0 ){
                                  base_selected_country_map
                               }else{
                                  update_selected_country_map <-
                                     base_selected_country_map %>%
                                     hc_add_series( data = dtf_select_country_map[input$CountryTable_rows_selected,],
                                                    type = "mappoint",
                                                    dataLabels = list(enabled=T,
                                                                      format="{point.name}",
                                                                      style = list(fontSize = '10px', fontWeight = 'normal', color = 'white')
                                                    ),
                                                    color  = hex_to_rgba("#FF0000", 0.9),
                                                    marker = list( radius = 3 ),
                                                    #minSize = 0,
                                                    name=""#,
                                                    #maxSize = 9
                                     )

                                  update_selected_country_map
                               }

                            })

                         ## !!!!!!!!!!!!! insert UI --------------------
                         insertUI(
                            selector = "#country_info",
                            ui = div(id = 'country_info_table_map',
                                     fluidRow( h1("Background market information"),
                                               column(6, dataTableOutput('CountryTable') %>% withSpinner(type=4),
                                                      HTML("<footer>Note: Population and GDP per capital are the latest data from the World Bank. Distance to New Zealand is the nearest distance between two territories' centre points. </footer>")
                                               ),
                                               column(6, highchartOutput("MapSelectedCountry") )
                                     )
                            )
                         )
                         ## III.3 Trade summary table for selected markets ALL CountryTradeTableTotal -----------------------
                         print("------------------  Trade summary tables -------------------------")
                         tmp_tab_all_country <- sum_selected_country( tmp_selected_countries )
                         
                         ## for some countries, there are only 5 years of service data 
                         if( is.na(tmp_tab_all_country$CAGR10[tmp_tab_all_country$Name == 'Services exports']) ){
                            tmp_tab_all_country$CAGR10[tmp_tab_all_country$Name == 'Total exports'] <- NA
                            tmp_tab_all_country$CAGR10[tmp_tab_all_country$Name == 'Two-way trade'] <- NA
                         }
                         if( is.na(tmp_tab_all_country$CAGR10[tmp_tab_all_country$Name == 'Services imports']) ){
                            tmp_tab_all_country$CAGR10[tmp_tab_all_country$Name == 'Total imports'] <- NA
                         }
                         
                         ## plot
                         output$CountryTradeTableTotal <- renderDataTable({
                            datatable( tmp_tab_all_country,
                                       rownames = F,
                                       extensions = 'Buttons',
                                       options = list(dom = 'Bt', 
                                                      scrollX = TRUE ,
                                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print') ) ,
                                       colnames=c("","Value ($m)", 'Share of world market','CAGR 1', 'CAGR 5', 'CAGR 10')
                            ) %>%
                               formatStyle(columns = 'Name',
                                           target = 'row',
                                           fontWeight = styleEqual(c('Total imports','Total exports','Two-way trade', 'Trade balance'),
                                                                   c('bold','bold','bold', 'bold')),
                                           backgroundColor = styleEqual(c('Total imports','Total exports'),
                                                                        c('lightgrey','lightgrey'))
                               ) %>%
                               formatStyle(
                                  c('CAGR1', 'CAGR5', 'CAGR10'),
                                  background = styleColorBar( c(0,max(tmp_tab_all_country[,c('CAGR1','CAGR5','CAGR10')],na.rm=T)*2) , 'lightblue'),
                                  backgroundSize = '100% 90%',
                                  backgroundRepeat = 'no-repeat',
                                  backgroundPosition = 'center',
                                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                               ) %>%
                               formatPercentage( c('CAGR1', 'CAGR5', 'CAGR10', 'Share'),digit = 1 ) %>%
                               formatStyle( columns = c('Name','CAGR1', 'CAGR5', 'CAGR10', 'Share', 'Value'), `font-size`= '115%' ) %>%
                               formatCurrency( columns = c('Value'), digits = 0 )
                         })
                         
                         ## III 3.1 Investment position summary table --------------------------------------
                         print("------------------  Investment summary tables -------------------------")
                         tmp_tab_all_country_investment <-
                            sum_selected_country_investment( tmp_selected_countries )
                         
                         output$CountryInvestmentTableTotal <- renderDataTable({
                            datatable( tmp_tab_all_country_investment,
                                       rownames = F,
                                       extensions = 'Buttons',
                                       options = list(dom = 'Bt', 
                                                      scrollX = TRUE,
                                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print') ) ,
                                       colnames=c("","Value ($m)", 'Share of world market','CAGR 1', 'CAGR 5', 'CAGR 10')
                            ) %>%
                               formatStyle(columns = 'Name',
                                           #target = 'row',
                                           fontWeight = styleEqual(c('Foreign direct investment',
                                                                     'Overseas direct investment',
                                                                     'Two-way direct investment'),
                                                                   c('bold','bold','bold')
                                                                   ) #,
                                           #backgroundColor = styleEqual(c('Foreign direct investment',
                                           #                               'Overseas direct investment'),
                                           #                             c('lightgrey','lightgrey'))
                               ) %>%
                               formatStyle(
                                  c('CAGR1', 'CAGR5', 'CAGR10'),
                                  background = styleColorBar( c(0,max(tmp_tab_all_country[,c('CAGR1','CAGR5','CAGR10')],na.rm=T)*2) , 'lightblue'),
                                  backgroundSize = '100% 90%',
                                  backgroundRepeat = 'no-repeat',
                                  backgroundPosition = 'center',
                                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                               ) %>%
                               formatPercentage( c('CAGR1', 'CAGR5', 'CAGR10', 'Share'),digit = 1 ) %>%
                               formatStyle( columns = c('Name','CAGR1', 'CAGR5', 'CAGR10', 'Share', 'Value'), `font-size`= '115%' ) %>%
                               formatCurrency( columns = c('Value'), digits = 0 )
                         })

                         ## III 3.2 People movement summary table ---------------------------
                         print("------------------  People movement summary tables -------------------------")
                         tmp_tab_all_country_pplmove <-
                            sum_selected_country_pplmove( tmp_selected_countries )
                         
                         output$CountryPplMovementTableTotal <- renderDataTable({
                            datatable( tmp_tab_all_country_pplmove,
                                       rownames = F,
                                       extensions = 'Buttons',
                                       options = list(dom = 'Bt', 
                                                      scrollX = TRUE,
                                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print') ) ,
                                       colnames=c("","Value ('000)", 'Share of world market','CAGR 1', 'CAGR 5', 'CAGR 10')
                            ) %>%
                               formatStyle(columns = 'Name',
                                           #target = 'row',
                                           fontWeight = styleEqual(c('Foreign visitors travelling in',
                                                                     'NZ visitors travelling out',
                                                                     'Two-way visitor movement'),
                                                                   c('bold','bold','bold')
                                           ) #,
                                           #backgroundColor = styleEqual(c('Foreign direct investment',
                                           #                               'Overseas direct investment'),
                                           #                             c('lightgrey','lightgrey'))
                               ) %>%
                               formatStyle(
                                  c('CAGR1', 'CAGR5', 'CAGR10'),
                                  background = styleColorBar( c(0,max(tmp_tab_all_country_pplmove[,c('CAGR1','CAGR5','CAGR10')],na.rm=T)*2) , 'lightblue'),
                                  backgroundSize = '100% 90%',
                                  backgroundRepeat = 'no-repeat',
                                  backgroundPosition = 'center',
                                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                               ) %>%
                               formatPercentage( c('CAGR1', 'CAGR5', 'CAGR10', 'Share'),digit = 1 ) %>%
                               formatStyle( columns = c('Name','CAGR1', 'CAGR5', 'CAGR10', 'Share', 'Value'), `font-size`= '115%' ) %>%
                               formatCurrency( columns = c('Value'), digits = 0, currency = '' )
                         })
                         
                         ## III.4 Line graph two-way trade CountryTwowayTradeGraphTotal ----------------------
                         print("------------------  Line graphs -------------------------")
                         tmp_dtf_twoway_line <-
                            dtf_shiny_country_gs %>%
                            filter( Year >=2007, Country %in% tmp_selected_countries ) %>%
                            group_by( Year ) %>%
                            do( Value = sum( .$Value, na.rm=T ) ) %>%
                            ungroup %>%
                            mutate( Value = as.numeric(Value)/10^6 ) %>%
                            mutate( Name = 'Two-way trade' ) %>%
                            mutate( Country = 'The selected markets' )

                         output$CountryTwowayTradeGraphTotal <- renderHighchart({
                            highchart() %>%
                               hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                               hc_xAxis( categories = c( unique( tmp_dtf_twoway_line$Year) ) ) %>%
                               hc_yAxis( title = list(text = "$ million, NZD"),
                                         labels = list( format = "${value:,.0f} m")  ) %>%
                               hc_plotOptions(line = list(
                                  dataLabels = list(enabled = F),
                                  #stacking = "normal",
                                  enableMouseTracking = T #,
                                  #series = list(events = list(legendItemClick = sharelegend)) ,
                                  #showInLegend = T
                               )
                               )%>%
                               hc_tooltip(table = TRUE,
                                          sort = TRUE,
                                          pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                " {series.name}: ${point.y:,.0f} m"),
                                          headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                               ) %>%
                               hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                               hc_add_series( data =  tmp_dtf_twoway_line  ,
                                              mapping = hcaes(  x = Year, y = Value, group = Name ),
                                              type = 'line',
                                              marker = list(symbol = 'circle') #,
                                              #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                               )
                         })
                         ## III.5 Line graph trade balance CountryTradeBalanceGraphTotal ----------------------
                         tmp_dtf_balance_line <-
                            dtf_shiny_country_gs %>%
                            filter( Year >=2007, Country %in% tmp_selected_countries ) %>%
                            group_by( Year, Type_ie ) %>%
                            do( Value = sum( .$Value, na.rm=T ) ) %>%
                            ungroup %>%
                            mutate( Value = as.numeric(Value)/10^6 ) %>%
                            group_by( Year ) %>%
                            do( Value = .$Value[.$Type_ie=='Exports'] - .$Value[.$Type_ie=='Imports']) %>%
                            ungroup %>%
                            mutate( Value = round(as.numeric(Value)) ) %>%
                            mutate( Name = 'Trade balance' ) %>%
                            bind_rows(dtf_shiny_country_gs %>%
                                         filter( Type_gs == 'Goods') %>%
                                         filter( Year >=2007, Country %in% tmp_selected_countries ) %>%
                                         group_by( Year, Type_ie ) %>%
                                         do( Value = sum( .$Value, na.rm=T ) ) %>%
                                         ungroup %>%
                                         mutate( Value = as.numeric(Value)/10^6 ) %>%
                                         group_by( Year ) %>%
                                         do( Value = .$Value[.$Type_ie=='Exports'] - .$Value[.$Type_ie=='Imports']) %>%
                                         ungroup %>%
                                         mutate( Value = round(as.numeric(Value)) ) %>%
                                         mutate( Name = 'Goods balance' )
                                      ) %>%
                            bind_rows(dtf_shiny_country_gs %>%
                                         filter( Type_gs == 'Services') %>%
                                         filter( Year >=2007, Country %in% tmp_selected_countries ) %>%
                                         group_by( Year, Type_ie ) %>%
                                         do( Value = sum( .$Value, na.rm=T ) ) %>%
                                         ungroup %>%
                                         mutate( Value = as.numeric(Value)/10^6 ) %>%
                                         group_by( Year ) %>%
                                         do( Value = .$Value[.$Type_ie=='Exports'] - .$Value[.$Type_ie=='Imports']) %>%
                                         ungroup %>%
                                         mutate( Value = round(as.numeric(Value)) ) %>%
                                         mutate( Name = 'Services balance' )
                                      ) %>%
                            mutate( Country = 'The selected markets' )
                         
                         
                         ## check if service data full, if not make it full
                         tmp_year_balance_line_g <- tmp_dtf_balance_line$Year[tmp_dtf_balance_line$Name == 'Goods balance']
                         tmp_year_balance_line_s <- tmp_dtf_balance_line$Year[tmp_dtf_balance_line$Name == 'Services balance']
                         
                         if( any(tmp_year_balance_line_s != 
                                 tmp_year_balance_line_g) ){
                            ## year missing
                            tmp_year_balance_line_missing <- setdiff( tmp_year_balance_line_g, tmp_year_balance_line_s )
                            
                            ## reconstruct the dataset
                            tmp_dtf_balance_line %<>%
                               bind_rows( data.frame( Year = tmp_year_balance_line_missing,
                                                      Value = NA,
                                                      Name = "Services balance", 
                                                      Country = "The selected markets") ) %>%
                               group_by( Name, Country ) %>%
                               arrange( Year ) %>%
                               ungroup
                         }

                         # output$CountryTradeBalanceGraphTotal <-
                         #    renderHighchart({
                         #       highchart() %>%
                         #          hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                         #          hc_xAxis( categories = c( unique( tmp_dtf_balance_line$Year) ) ) %>%
                         #          hc_yAxis( title = list(text = "$ million, NZD"),
                         #                    labels = list( format = "${value:,.0f} m")
                         #          ) %>%
                         #          hc_plotOptions(line = list(
                         #             dataLabels = list(enabled = F),
                         #             #stacking = "normal",
                         #             enableMouseTracking = T #,
                         #             #series = list(events = list(legendItemClick = sharelegend)) ,
                         #             #showInLegend = T
                         #          ) )%>%
                         #          hc_tooltip(table = TRUE,
                         #                     sort = TRUE,
                         #                     pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                         #                                           " {series.name}: ${point.y:,.0f} m"),
                         #                     headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                         #          ) %>%
                         #          hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                         #          hc_add_series( data =  tmp_dtf_balance_line  ,
                         #                         mapping = hcaes(  x = Year, y = Value, group = Name ),
                         #                         type = 'line',
                         #                         marker = list(symbol = 'circle') #,
                         #                         #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                         #          )
                         #    })
                         
                         output$CountryTradeBalanceGraphTotal <-
                            renderHighchart({
                               highchart() %>%
                                  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                  hc_chart(type = 'line') %>%
                                  hc_series( list(name = 'Trade balance', data =tmp_dtf_balance_line$Value[tmp_dtf_balance_line$Name =='Trade balance'], color='brown' , marker = list(enabled = F), lineWidth = 3 ),
                                             list(name = 'Goods balance', data =tmp_dtf_balance_line$Value[tmp_dtf_balance_line$Name =='Goods balance'], color = 'darkgreen', dashStyle = 'shortDot', marker = list(symbol = 'circle') ),
                                             list(name = 'Services balance', data =tmp_dtf_balance_line$Value[tmp_dtf_balance_line$Name =='Services balance'], color = 'darkblue', dashStyle = 'shortDot',  marker = list(symbol = 'triangle') )
                                  )%>%
                                  hc_xAxis( categories = unique(tmp_dtf_balance_line$Year) ) %>%
                                  hc_yAxis( title = list(text = "$ million, NZD"),
                                            labels = list( format = "${value:,.0f} m"),
                                            plotLines = list(
                                               list(#label = list(text = "This is a plotLine"),
                                                  color = "#ff0000",
                                                  #dashStyle = 'shortDot',
                                                  width = 2,
                                                  value = 0 ) )
                                  ) %>%
                                  hc_plotOptions(column = list(
                                     dataLabels = list(enabled = F),
                                     #stacking = "normal",
                                     enableMouseTracking = T ) 
                                  )%>%
                                  hc_tooltip(table = TRUE,
                                             sort = TRUE,
                                             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                   " {series.name}: ${point.y} m"),
                                             headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                  ) %>%
                                  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = 000 )
                            })
                         
                         
                         ## III.6 Line graph Exports CountryExportsGraphTotal ----------------------
                         tmp_dtf_ex_line <-
                            dtf_shiny_country_gs %>%
                            filter( Year >=2007, Country %in% tmp_selected_countries,
                                    Type_ie == 'Exports') %>%
                            group_by( Year, Type_gs ) %>%
                            do( Value = sum( .$Value, na.rm=T ) ) %>%
                            ungroup %>%
                            mutate( Value = as.numeric(Value)/10^6 ) %>%
                            mutate( Name = paste0(Type_gs,' exports') ) %>%
                            dplyr::select( -Type_gs )

                         tmp_dtf_ex_line %<>%
                            bind_rows( tmp_dtf_ex_line %>%
                                          group_by( Year ) %>%
                                          do( Value = sum(.$Value, na.rm=T) ) %>%
                                          ungroup %>%
                                          mutate( Value = as.numeric(Value) ) %>%
                                          mutate(Name = 'Total exports')
                            ) %>%
                            mutate( Country = 'The selected markets' ) %>%
                            mutate( Name = factor(Name, levels = c('Total exports','Goods exports','Services exports')) )

                         output$CountryExportsGraphTotal <-
                            renderHighchart({
                               highchart() %>%
                                  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                  hc_xAxis( categories = c( unique( tmp_dtf_ex_line$Year) ) ) %>%
                                  hc_yAxis( title = list(text = "$ million, NZD"),
                                            labels = list( format = "${value:,.0f} m")
                                  ) %>%
                                  hc_plotOptions(line = list(
                                     dataLabels = list(enabled = F),
                                     #stacking = "normal",
                                     enableMouseTracking = T #,
                                     #series = list(events = list(legendItemClick = sharelegend)) ,
                                     #showInLegend = T
                                  ) )%>%
                                  hc_tooltip(table = TRUE,
                                             sort = TRUE,
                                             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                   " {series.name}: ${point.y:,.0f} m"),
                                             headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                  ) %>%
                                  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                  hc_add_series( data =  tmp_dtf_ex_line  ,
                                                 mapping = hcaes(  x = Year, y = Value, group = Name ),
                                                 type = 'line',
                                                 marker = list(symbol = 'circle') #,
                                                 #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                  )
                            })
                         ## III.7 Line graph Exports CountryExportsGraphTotalPercent ----------------------
                         tmp_dtf_ex_line_world <-
                            dtf_shiny_country_gs %>%
                            filter( Year >=2007, Country %in% 'World',
                                    Type_ie == 'Exports') %>%
                            group_by( Year, Type_gs ) %>%
                            do( Value = sum( .$Value, na.rm=T ) ) %>%
                            ungroup %>%
                            mutate( Value = as.numeric(Value)/10^6 ) %>%
                            mutate( Name = paste0(Type_gs,' exports') ) %>%
                            dplyr::select( -Type_gs )

                         tmp_dtf_ex_line_world %<>%
                            bind_rows( tmp_dtf_ex_line_world %>%
                                          group_by( Year ) %>%
                                          do( Value = sum(.$Value, na.rm=T) ) %>%
                                          ungroup %>%
                                          mutate( Value = as.numeric(Value) ) %>%
                                          mutate(Name = 'Total exports')
                            ) %>%
                            mutate( Country = 'World' ) %>%
                            mutate( Name = factor(Name, levels = c('Total exports','Goods exports','Services exports')) )

                         tmp_dtf_ex_line_percent <-
                            tmp_dtf_ex_line %>%
                            bind_rows( tmp_dtf_ex_line_world ) %>%
                            group_by( Year, Name ) %>%
                            do( Share = .$Value/.$Value[.$Country=='World'] ) %>%
                            ungroup %>%
                            rowwise %>%
                            mutate( Value = ifelse( length(unlist(Share))==2, unlist(Share)[1]*100, NA) )

                         output$CountryExportsGraphTotalPercent <-
                            renderHighchart({
                               highchart() %>%
                                  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                  hc_xAxis( categories = c( unique( tmp_dtf_ex_line_percent$Year) ) ) %>%
                                  hc_yAxis( title = list(text = "Percentage (%)"),
                                            labels = list( format = "{value:,.1f} %")  ) %>%
                                  hc_plotOptions(line = list(
                                     dataLabels = list(enabled = F),
                                     #stacking = "normal",
                                     enableMouseTracking = T)
                                  )%>%
                                  hc_tooltip(table = TRUE,
                                             sort = TRUE,
                                             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                   " {series.name}: {point.y:,.1f} %"),
                                             headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                  ) %>%
                                  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                  hc_add_series( data =  tmp_dtf_ex_line_percent  ,
                                                 mapping = hcaes(  x = Year, y = Value, group = Name ),
                                                 type = 'line',
                                                 marker = list(symbol = 'circle') #,
                                                 #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                  )
                            })

                         ## III.8 Line graph Imports CountryImportsGraphTotal ----------------------
                         tmp_dtf_im_line <-
                            dtf_shiny_country_gs %>%
                            filter( Year >=2007, Country %in% tmp_selected_countries,
                                    Type_ie == 'Imports') %>%
                            group_by( Year, Type_gs ) %>%
                            do( Value = sum( .$Value, na.rm=T ) ) %>%
                            ungroup %>%
                            mutate( Value = as.numeric(Value)/10^6 ) %>%
                            mutate( Name = paste0(Type_gs,' imports') ) %>%
                            dplyr::select( -Type_gs )

                         tmp_dtf_im_line %<>%
                            bind_rows( tmp_dtf_im_line %>%
                                          group_by( Year ) %>%
                                          do( Value = sum(.$Value, na.rm=T) ) %>%
                                          ungroup %>%
                                          mutate( Value = as.numeric(Value) ) %>%
                                          mutate(Name = 'Total imports')
                            ) %>%
                            mutate( Country = 'The selected markets' ) %>%
                            mutate( Name = factor(Name, levels = c('Total imports','Goods imports','Services imports')) )

                         output$CountryImportsGraphTotal <-
                            renderHighchart({
                               highchart() %>%
                                  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                  hc_xAxis( categories = c( unique( tmp_dtf_im_line$Year) ) ) %>%
                                  hc_yAxis( title = list(text = "$ million, NZD"),
                                            labels = list( format = "${value:,.0f} m")
                                  ) %>%
                                  hc_plotOptions(line = list(
                                     dataLabels = list(enabled = F),
                                     #stacking = "normal",
                                     enableMouseTracking = T #,
                                     #series = list(events = list(legendItemClick = sharelegend)) ,
                                     #showInLegend = T
                                  ) )%>%
                                  hc_tooltip(table = TRUE,
                                             sort = TRUE,
                                             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                   " {series.name}: ${point.y:,.0f} m"),
                                             headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                  ) %>%
                                  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                  hc_add_series( data =  tmp_dtf_im_line  ,
                                                 mapping = hcaes(  x = Year, y = Value, group = Name ),
                                                 type = 'line',
                                                 marker = list(symbol = 'circle') #,
                                                 #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                  )
                            })
                         ## III.9 Line graph Imports CountryImportsGraphTotalPercent ----------------------
                         tmp_dtf_im_line_world <-
                            dtf_shiny_country_gs %>%
                            filter( Year >=2007, Country %in% 'World',
                                    Type_ie == 'Imports') %>%
                            group_by( Year, Type_gs ) %>%
                            do( Value = sum( .$Value, na.rm=T ) ) %>%
                            ungroup %>%
                            mutate( Value = as.numeric(Value)/10^6 ) %>%
                            mutate( Name = paste0(Type_gs,' imports') ) %>%
                            dplyr::select( -Type_gs )

                         tmp_dtf_im_line_world %<>%
                            bind_rows( tmp_dtf_im_line_world %>%
                                          group_by( Year ) %>%
                                          do( Value = sum(.$Value, na.rm=T) ) %>%
                                          ungroup %>%
                                          mutate( Value = as.numeric(Value) ) %>%
                                          mutate(Name = 'Total imports')
                            ) %>%
                            mutate( Country = 'World' ) %>%
                            mutate( Name = factor(Name, levels = c('Total imports','Goods imports','Services imports')) )

                         tmp_dtf_im_line_percent <-
                            tmp_dtf_im_line %>%
                            bind_rows( tmp_dtf_im_line_world ) %>%
                            group_by( Year, Name ) %>%
                            do( Share = .$Value/.$Value[.$Country=='World'] ) %>%
                            ungroup %>%
                            rowwise %>%
                            mutate( Value = ifelse( length(unlist(Share))==2, unlist(Share)[1]*100, NA) )

                         output$CountryImportsGraphTotalPercent <-
                            renderHighchart({
                               highchart() %>%
                                  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                  hc_xAxis( categories = c( unique( tmp_dtf_im_line_percent$Year) ) ) %>%
                                  hc_yAxis( title = list(text = "Percentage (%)"),
                                            labels = list( format = "{value:,.1f} %")  ) %>%
                                  hc_plotOptions(line = list(
                                     dataLabels = list(enabled = F),
                                     #stacking = "normal",
                                     enableMouseTracking = T)
                                  )%>%
                                  hc_tooltip(table = TRUE,
                                             sort = TRUE,
                                             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                   " {series.name}: {point.y:,.1f} %"),
                                             headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                  ) %>%
                                  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                  hc_add_series( data =  tmp_dtf_im_line_percent  ,
                                                 mapping = hcaes(  x = Year, y = Value, group = Name ),
                                                 type = 'line',
                                                 marker = list(symbol = 'circle') #,
                                                 #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                  )
                            })
                         ## III.9.1 Line graph Investment position CountryInvestmentGraphTotal ----------------------
                         tmp_dtf_invest_line <-
                            dtf_fdi_odi %>%
                            filter( Year >=2007, 
                                    Country %in% tmp_selected_countries 
                                    ) %>%
                            group_by( Year, Type ) %>%
                            do( Value = sum( .$Value, na.rm=T ) ) %>%
                            ungroup %>%
                            mutate( Value = as.numeric(Value) ) %>%
                            mutate( Name =  ifelse(Type=="FDI", 
                                                   'Foreign direct investment',
                                                   "Overseas direct investment") ) %>%
                            dplyr::select( -Type )
                         
                         tmp_dtf_invest_line %<>%
                            bind_rows( tmp_dtf_invest_line %>%
                                          group_by( Year ) %>%
                                          do( Value = sum(.$Value, na.rm=T) ) %>%
                                          ungroup %>%
                                          mutate( Value = as.numeric(Value) ) %>%
                                          mutate(Name = 'Two-way direct investment')
                            ) %>%
                            mutate( Country = 'The selected markets' ) %>%
                            mutate( Name = factor(Name, levels = c('Two-way direct investment',
                                                                   'Foreign direct investment',
                                                                   'Overseas direct investment')) 
                                    )
                         
                         output$CountryInvestmentGraphTotal <-
                            renderHighchart({
                               highchart() %>%
                                  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                  hc_xAxis( categories = c( unique( tmp_dtf_invest_line$Year) ) ) %>%
                                  hc_yAxis( title = list(text = "$ million, NZD"),
                                            labels = list( format = "${value:,.0f} m")
                                  ) %>%
                                  hc_plotOptions(line = list(
                                     dataLabels = list(enabled = F),
                                     #stacking = "normal",
                                     enableMouseTracking = T #,
                                     #series = list(events = list(legendItemClick = sharelegend)) ,
                                     #showInLegend = T
                                  ) )%>%
                                  hc_tooltip(table = TRUE,
                                             sort = TRUE,
                                             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                   " {series.name}: ${point.y:,.0f} m"),
                                             headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                  ) %>%
                                  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                  hc_add_series( data =  tmp_dtf_invest_line  ,
                                                 mapping = hcaes(  x = Year, y = Value, group = Name ),
                                                 type = 'line',
                                                 marker = list(symbol = 'circle') #,
                                                 #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                  )
                            })
                         
                         ## III.9.2 Line graph Investment CountryInvestmentGraphTotalPercent ----------------------
                         tmp_dtf_invest_line_world <-
                            dtf_fdi_odi %>%
                            filter( Year >=2007, 
                                    Country %in% 'World' #,
                                    #Type_ie == 'Imports' 
                                    ) %>%
                            group_by( Year, Type ) %>%
                            do( Value = sum( .$Value, na.rm=T ) ) %>%
                            ungroup %>%
                            mutate( Value = as.numeric(Value) ) %>%
                            mutate( Name =  ifelse(Type=="FDI", 
                                                   'Foreign direct investment',
                                                   "Overseas direct investment") ) %>%
                            dplyr::select( -Type )
                         
                         tmp_dtf_invest_line_world %<>%
                            bind_rows( tmp_dtf_invest_line_world %>%
                                          group_by( Year ) %>%
                                          do( Value = sum(.$Value, na.rm=T) ) %>%
                                          ungroup %>%
                                          mutate( Value = as.numeric(Value) ) %>%
                                          mutate(Name = 'Two-way direct investment')
                            ) %>%
                            mutate( Country = 'World' ) %>%
                            mutate( Name = factor(Name, levels = c('Two-way direct investment',
                                                                   'Foreign direct investment',
                                                                   'Overseas direct investment') ) )
                         
                         tmp_dtf_invest_line_percent <-
                            tmp_dtf_invest_line %>%
                            bind_rows( tmp_dtf_invest_line_world ) %>%
                            group_by( Year, Name ) %>%
                            do( Share = .$Value/.$Value[.$Country=='World'] ) %>%
                            ungroup %>%
                            rowwise %>%
                            mutate( Value = ifelse( length(unlist(Share))==2, unlist(Share)[1]*100, NA) )
                         
                         output$CountryInvestmentGraphTotalPercent <-
                            renderHighchart({
                               highchart() %>%
                                  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                  hc_xAxis( categories = c( unique( tmp_dtf_invest_line_percent$Year) ) ) %>%
                                  hc_yAxis( title = list(text = "Percentage (%)"),
                                            labels = list( format = "{value:,.1f} %")  ) %>%
                                  hc_plotOptions(line = list(
                                     dataLabels = list(enabled = F),
                                     #stacking = "normal",
                                     enableMouseTracking = T)
                                  )%>%
                                  hc_tooltip(table = TRUE,
                                             sort = TRUE,
                                             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                   " {series.name}: {point.y:,.1f} %"),
                                             headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                  ) %>%
                                  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                  hc_add_series( data =  tmp_dtf_invest_line_percent  ,
                                                 mapping = hcaes(  x = Year, y = Value, group = Name ),
                                                 type = 'line',
                                                 marker = list(symbol = 'circle') #,
                                                 #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                  )
                            })
                         
                         ## III.9.3 Line graph Ppl Movement CountryPplMovementGraphTotal ----------------------
                         tmp_dtf_pplmove_line <-
                            dtf_in_out %>%
                            filter( Year >=2007, 
                                    Country %in% tmp_selected_countries 
                            ) %>%
                            group_by( Year, Type ) %>%
                            do( Value = sum( .$Value, na.rm=T ) ) %>%
                            ungroup %>%
                            mutate( Value = as.numeric(Value)/10^3 ) %>%
                            mutate( Name =  Type ) %>%
                            dplyr::select( -Type )
                         
                         tmp_dtf_pplmove_line %<>%
                            bind_rows( tmp_dtf_pplmove_line %>%
                                          group_by( Year ) %>%
                                          do( Value = sum(.$Value, na.rm=T) ) %>%
                                          ungroup %>%
                                          mutate( Value = as.numeric(Value) ) %>%
                                          mutate(Name = 'Two-way visitor movement')
                            ) %>%
                            mutate( Country = 'The selected markets' ) %>%
                            mutate( Name = factor(Name, levels = c('Two-way visitor movement',
                                                                   'Foreign visitors travelling in',
                                                                   'NZ visitors travelling out')) 
                            )
                         
                         output$CountryPplMovementGraphTotal <-
                            renderHighchart({
                               highchart() %>%
                                  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                  hc_xAxis( categories = c( unique( tmp_dtf_pplmove_line$Year) ) ) %>%
                                  hc_yAxis( title = list(text = "Number of visitors, '000"),
                                            labels = list( format = "{value:,.0f}")
                                  ) %>%
                                  hc_plotOptions(line = list(
                                     dataLabels = list(enabled = F),
                                     #stacking = "normal",
                                     enableMouseTracking = T #,
                                     #series = list(events = list(legendItemClick = sharelegend)) ,
                                     #showInLegend = T
                                  ) )%>%
                                  hc_tooltip(table = TRUE,
                                             sort = TRUE,
                                             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                   " {series.name}: {point.y:,.0f} 000"),
                                             headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                  ) %>%
                                  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                  hc_add_series( data =  tmp_dtf_pplmove_line ,
                                                 mapping = hcaes(  x = Year, y = Value, group = Name ),
                                                 type = 'line',
                                                 marker = list(symbol = 'circle') #,
                                                 #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                  )
                            })
                         
                         ## III.9.4 Line graph ppl movement CountryPplMovementGraphTotalPercent ----------------------
                         tmp_dtf_pplmove_line_world <-
                            dtf_in_out %>%
                            filter( Year >=2007, 
                                    Country %in% 'World'
                            ) %>%
                            group_by( Year, Type ) %>%
                            do( Value = sum( .$Value, na.rm=T ) ) %>%
                            ungroup %>%
                            mutate( Value = as.numeric(Value)/10^3 ) %>%
                            mutate( Name =  Type ) %>%
                            dplyr::select( -Type )
                         
                         tmp_dtf_pplmove_line_world %<>%
                            bind_rows( tmp_dtf_pplmove_line_world %>%
                                          group_by( Year ) %>%
                                          do( Value = sum(.$Value, na.rm=T) ) %>%
                                          ungroup %>%
                                          mutate( Value = as.numeric(Value) ) %>%
                                          mutate(Name = 'Two-way visitor movement')
                            ) %>%
                            mutate( Country = 'World' ) %>%
                            mutate( Name = factor(Name, levels = c('Two-way visitor movement',
                                                                   'Foreign visitors travelling in',
                                                                   'NZ visitors travelling out') ) )
                         
                         tmp_dtf_pplmove_line_percent <-
                            tmp_dtf_pplmove_line %>%
                            bind_rows( tmp_dtf_pplmove_line_world ) %>%
                            group_by( Year, Name ) %>%
                            do( Share = .$Value/.$Value[.$Country=='World'] ) %>%
                            ungroup %>%
                            rowwise %>%
                            mutate( Value = ifelse( length(unlist(Share))==2, unlist(Share)[1]*100, NA) )
                         
                         output$CountryPplMovementGraphTotalPercent <-
                            renderHighchart({
                               highchart() %>%
                                  hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                  hc_xAxis( categories = c( unique( tmp_dtf_pplmove_line_percent$Year) ) ) %>%
                                  hc_yAxis( title = list(text = "Percentage (%)"),
                                            labels = list( format = "{value:,.1f} %")  ) %>%
                                  hc_plotOptions(line = list(
                                     dataLabels = list(enabled = F),
                                     #stacking = "normal",
                                     enableMouseTracking = T)
                                  )%>%
                                  hc_tooltip(table = TRUE,
                                             sort = TRUE,
                                             pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                                   " {series.name}: {point.y:,.1f} %"),
                                             headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                                  ) %>%
                                  hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
                                  hc_add_series( data =  tmp_dtf_pplmove_line_percent  ,
                                                 mapping = hcaes(  x = Year, y = Value, group = Name ),
                                                 type = 'line',
                                                 marker = list(symbol = 'circle') #,
                                                 #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
                                  )
                            })
                         
                         ## III.10 Treemap Key export commodities KeyExCountryTotalTreeMap -------------------
                         print("------------------  Tree maps -------------------------")
                         tmp_dtf_ex_country <-
                            get_snz_gs_country("Exports",tmp_selected_countries) %>%
                            mutate( Value = Value/10^6)
                         
                         fail_tm_ex_country <- try(
                            tmp_tm_ex_country <-
                               treemap( tmp_dtf_ex_country  %>%
                                           filter(Year==max(Year)),
                                        index = c("Type_gs", "SNZ_commodity"),
                                        vSize = "Value",
                                        vColor = "CAGR5",
                                        type = 'value',
                                        #aspRatio = 1.618,
                                        overlap.labels = 1,
                                        fun.aggregate = "weighted.mean",
                                        #palette = "RdYlGn",
                                        draw = FALSE)
                         )
                         
                         if( class(fail_tm_ex_country)=='try-error' ){
                            output$KeyExCountryTotalTreeMap <-
                               renderHighchart({
                                  highchart %>%
                                     hc_title(text = "key commodities and services EXPORTS")
                               })
                         }else{
                            output$KeyExCountryTotalTreeMap <-
                               renderHighchart({
                                  highchart() %>%
                                     hc_add_series_treemap2(
                                  #hctreemap( 
                                     tmp_tm_ex_country ,
                                            allowDrillToNode = TRUE,
                                            layoutAlgorithm = "squarified",
                                            levelIsConstant = FALSE,
                                            levels = list(list(level = 1,
                                                               dataLabels = list(enabled = TRUE,
                                                                                 style = list(fontSize = '20px', color = 'white',
                                                                                              fontWeight = 'normal'),
                                                                                 backgroundColor = 'lightgrey',
                                                                                 align = 'left', verticalAlign = 'top'),
                                                               borderColor = "#555",
                                                               borderWidth = 2 ),
                                                          list(level = 2,
                                                               dataLabels = list(enabled = TRUE,
                                                                                 style = list(fontSize = '9px',
                                                                                              fontWeight = 'normal')
                                                               )
                                                          )
                                            )
                                  ) %>%
                                     hc_chart(backgroundColor = NULL, plotBorderColor = "#555", plotBorderWidth = 2) %>%
                                     hc_title(text = "key commodities and services EXPORTS") %>%
                                     hc_subtitle(text = "Coloured by compound annual growth rate (CAGR) for the past 5 years (%)") %>%
                                     hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                     hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                                             Export value: ${point.value:,.0f} m <br>
                                             CAGR 5: {point.colorValue:,.1f}%") %>% 
                                     hc_colorAxis(minColor = tmp_tm_ex_country$tm$color[which.min(tmp_tm_ex_country$tm$vColorValue)],
                                                  maxColor = tmp_tm_ex_country$tm$color[which.max(tmp_tm_ex_country$tm$vColorValue)] ,
                                                  labels = list(format = "{value}%", useHTML = TRUE), reversed = FALSE
                                     ) %>%
                                     hc_legend(align = "right", layout = "vertical", verticalAlign = "top",
                                               reversed = TRUE , y = 70, symbolHeight = 250, itemMarginTop = 10)
                               })
                         }

                         
                         ## III.11 Treemap Key import commodities KeyImCountryTotalTreeMap -------------------
                         tmp_dtf_im_country <-
                            get_snz_gs_country("Imports",tmp_selected_countries)%>%
                            mutate( Value = Value/10^6)

                         fail_tm_im_country <- 
                            try( tmp_tm_im_country <-
                                    treemap( tmp_dtf_im_country  %>%
                                                filter(Year==max(Year)) ,
                                             index = c("Type_gs", "SNZ_commodity"),
                                             vSize = "Value",
                                             vColor = "CAGR5",
                                             type = 'value',
                                             #aspRatio = 1.618,
                                             overlap.labels = 1,
                                             fun.aggregate = "weighted.mean",
                                             #palette = "RdYlGn",
                                             draw = FALSE)
                                 )
                         
                         if( class(fail_tm_im_country) == 'try-error' ){
                            output$KeyImCountryTotalTreeMap <-
                               renderHighchart({ 
                                  highchart() %>%
                                     hc_title(text = "key commodities and services IMPORTS") 
                            })
                         }else{
                            output$KeyImCountryTotalTreeMap <-
                               renderHighchart({
                                  highchart() %>%
                                     hc_add_series_treemap2(
                                  #hctreemap(
                                     tmp_tm_im_country ,
                                            allowDrillToNode = TRUE,
                                            layoutAlgorithm = "squarified",
                                            levelIsConstant = FALSE,
                                            levels = list(list(level = 1,
                                                               dataLabels = list(enabled = TRUE,
                                                                                 style = list(fontSize = '20px', color = 'white',
                                                                                              fontWeight = 'normal'),
                                                                                 backgroundColor = 'lightgrey',
                                                                                 align = 'left', verticalAlign = 'top'),
                                                               borderColor = "#555",
                                                               borderWidth = 2 ),
                                                          list(level = 2,
                                                               dataLabels = list(enabled = TRUE,
                                                                                 style = list(fontSize = '9px',
                                                                                              fontWeight = 'normal')
                                                               )
                                                          )
                                            )
                                  ) %>%
                                     hc_chart(backgroundColor = NULL, plotBorderColor = "#555", plotBorderWidth = 2) %>%
                                     hc_title(text = "key commodities and services IMPORTS") %>%
                                     hc_subtitle(text = "Coloured by compound annual growth rate (CAGR) for the past 5 years (%)") %>%
                                     hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                                     hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                                                Import value: ${point.value:,.0f} m <br>
                                                CAGR 5: {point.colorValue:,.1f}%") %>% 
                                     hc_colorAxis(minColor = tmp_tm_im_country$tm$color[which.min(tmp_tm_im_country$tm$vColorValue)],
                                                  maxColor = tmp_tm_im_country$tm$color[which.max(tmp_tm_im_country$tm$vColorValue)] ,
                                                  labels = list(format = "{value}%", useHTML = TRUE), reversed = FALSE
                                     ) %>%
                                     hc_legend(align = "right", layout = "vertical", verticalAlign = "top",
                                               reversed = TRUE , y = 70, symbolHeight = 250, itemMarginTop = 10)
                               })
                         }
                         
                         ## III.12 Line graph Key commodity country key Exports KeyExCountryTotalLine ---------------------
                         print("------------------  Line graph for commodities -------------------------")
                         tmp_top_g_country_ex <-
                            tmp_dtf_ex_country %>%
                            filter( Year == max(Year),
                                    Type_gs == 'Goods',
                                    !SNZ_commodity %in% c('Confidential data', 'Other goods')
                            ) %>%
                            arrange( -Value ) %>%
                            dplyr::select( SNZ_commodity ) %>%
                            as.matrix() %>%
                            as.character

                         ## top 10 commodities
                         tmp_top_g_country_ex <-  tmp_top_g_country_ex[1:(min(10, length(tmp_top_g_country_ex)))]

                         tmp_top_s_country_ex <-
                            tmp_dtf_ex_country %>%
                            filter( Year == max(Year),
                                    Type_gs == 'Services',
                                    !SNZ_commodity %in% c('Other business services', 'Other services')
                            ) %>%
                            arrange( -Value ) %>%
                            dplyr::select( SNZ_commodity ) %>%
                            as.matrix() %>%
                            as.character

                         ## top 5 services
                         tmp_top_s_country_ex <-  na.omit(tmp_top_s_country_ex[1:(min(5, length(tmp_top_s_country_ex)))])

                         ## top 10 commodities and top 5services
                         tmp_top_country_ex <- c( tmp_top_g_country_ex, tmp_top_s_country_ex)

                         tmp_dtf_key_line_country_ex <-
                            tmp_dtf_ex_country %>%
                            filter( SNZ_commodity %in% tmp_top_country_ex,
                                    Year >=2007) %>%
                            mutate( SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_country_ex)
                            ) %>%
                            arrange( SNZ_commodity )

                         ### plot
                         tmp_hc_ex_country <-
                            highchart() %>%
                            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                            hc_xAxis( categories = c( unique( tmp_dtf_key_line_country_ex$Year) ) ) %>%
                            hc_yAxis( title = list(text = "$ million, NZD"), #"Commodities and services exports over $1 bn"
                                      labels = list( format = "${value:,.0f} m")  ) %>%
                            hc_plotOptions(line = list(
                               dataLabels = list(enabled = F),
                               #stacking = "normal",
                               enableMouseTracking = T)
                            )%>%
                            hc_tooltip(table = TRUE,
                                       sort = TRUE,
                                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                             " {series.name}: ${point.y:,.0f} m"),
                                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                            ) %>%
                            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )

                         ### if any services are selected?
                         if( length(tmp_top_g_country_ex)>=1&length(tmp_top_s_country_ex)==0 ) {
                            output$KeyExCountryTotalLine <-
                               renderHighchart(
                                  tmp_hc_ex_country %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_ex %>% filter( Type_gs == 'Goods' ) ,
                                                    mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                    type = 'line',
                                                    marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_g_country_ex)-1))
                                     )
                               )
                         }
                         if( length(tmp_top_g_country_ex)==0 & length(tmp_top_s_country_ex)>=1 ){
                            output$KeyExCountryTotalLine <-
                               renderHighchart(
                                  tmp_hc_ex_country %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_ex %>% filter( Type_gs == 'Services' ),
                                                    mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                    type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_s_country_ex)-1))
                                     )
                               )
                         }
                         if( length(tmp_top_g_country_ex)>=1 & length(tmp_top_s_country_ex)>=1 ){
                            output$KeyExCountryTotalLine <-
                               renderHighchart(
                                  tmp_hc_ex_country %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_ex %>% filter( Type_gs == 'Goods' ) ,
                                                    mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                    type = 'line',
                                                    marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_g_country_ex)-1))
                                     ) %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_ex %>% filter( Type_gs == 'Services' ),
                                                    mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                    type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_s_country_ex)-1))
                                     )
                               )
                         }


                         ## III.13 Line graph Key commodity country key Exports Percent KeyExCountryTotalLinePercent ---------------------
                         tmp_ex_country_percent_hc <-
                            highchart() %>%
                            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                            hc_xAxis( categories = c( unique( tmp_dtf_key_line_country_ex$Year) ) ) %>%
                            hc_yAxis( title = list(text = "Percentage (%)"),
                                      labels = list( format = "{value:,.1f} %")  ) %>%
                            hc_plotOptions(line = list(
                               dataLabels = list(enabled = F),
                               #stacking = "normal",
                               enableMouseTracking = T)
                            )%>%
                            hc_tooltip(table = TRUE,
                                       sort = TRUE,
                                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                             " {series.name}: {point.y:,.1f} %"),
                                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                            ) %>%
                            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                         #hc_legend( enabled = FALSE )

                         ### if any services are selected?
                         if( length(tmp_top_g_country_ex)>=1&length(tmp_top_s_country_ex)==0 ) {
                            output$KeyExCountryTotalLinePercent <-
                               renderHighchart(
                                  tmp_ex_country_percent_hc %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_ex %>% filter( Type_gs == 'Goods' ) ,
                                                    mapping = hcaes(  x = Year, y = Share, group = SNZ_commodity ),
                                                    type = 'line',
                                                    marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_g_country_ex)-1))
                                     )
                               )
                         }
                         if( length(tmp_top_g_country_ex)==0 & length(tmp_top_s_country_ex)>=1 ){
                            output$KeyExCountryTotalLinePercent <-
                               renderHighchart(
                                  tmp_ex_country_percent_hc %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_ex %>% filter( Type_gs == 'Services' ),
                                                    mapping = hcaes(  x = Year, y = Share, group = SNZ_commodity ),
                                                    type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_s_country_ex)-1))
                                     )
                               )
                         }
                         if( length(tmp_top_g_country_ex)>=1 & length(tmp_top_s_country_ex)>=1 ){
                            output$KeyExCountryTotalLinePercent <-
                               renderHighchart(
                                  tmp_ex_country_percent_hc %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_ex %>% filter( Type_gs == 'Goods' ) ,
                                                    mapping = hcaes(  x = Year, y = Share, group = SNZ_commodity ),
                                                    type = 'line',
                                                    marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_g_country_ex)-1))
                                     ) %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_ex %>% filter( Type_gs == 'Services' ),
                                                    mapping = hcaes(  x = Year, y = Share, group = SNZ_commodity ),
                                                    type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_s_country_ex)-1))
                                     )
                               )
                         }

                         ## III.14 Line graph Key commodity country key Imports KeyImCountryTotalLine ---------------------
                         tmp_top_g_country_im <-
                            tmp_dtf_im_country %>%
                            filter( Year == max(Year),
                                    Type_gs == 'Goods',
                                    !SNZ_commodity %in% c('Confidential data', 'Other goods')
                            ) %>%
                            arrange( -Value ) %>%
                            dplyr::select( SNZ_commodity ) %>%
                            as.matrix() %>%
                            as.character

                         ## top 10 commodities
                         tmp_top_g_country_im <-  tmp_top_g_country_im[1:(min(10, length(tmp_top_g_country_im)))]

                         tmp_top_s_country_im <-
                            tmp_dtf_im_country %>%
                            filter( Year == max(Year),
                                    Type_gs == 'Services',
                                    !SNZ_commodity %in% c('Other business services', 'Other services')
                            ) %>%
                            arrange( -Value ) %>%
                            dplyr::select( SNZ_commodity ) %>%
                            as.matrix() %>%
                            as.character

                         ## top 5 services
                         tmp_top_s_country_im <-  na.omit(tmp_top_s_country_im[1:(min(5, length(tmp_top_s_country_im)))])

                         ## top 10 commodities and top 5services
                         tmp_top_country_im <- c( tmp_top_g_country_im, tmp_top_s_country_im)

                         tmp_dtf_key_line_country_im <-
                            tmp_dtf_im_country %>%
                            filter( SNZ_commodity %in% tmp_top_country_im,
                                    Year >=2007) %>%
                            mutate( SNZ_commodity = factor(SNZ_commodity, levels = tmp_top_country_im)
                            ) %>%
                            arrange( SNZ_commodity )

                         ### plot
                         tmp_hc_im_country <-
                            highchart() %>%
                            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                            hc_xAxis( categories = c( unique( tmp_dtf_key_line_country_im$Year) ) ) %>%
                            hc_yAxis( title = list(text = "$ million, NZD"), #"Commodities and services exports over $1 bn"
                                      labels = list( format = "${value:,.0f} m")  ) %>%
                            hc_plotOptions(line = list(
                               dataLabels = list(enabled = F),
                               #stacking = "normal",
                               enableMouseTracking = T)
                            )%>%
                            hc_tooltip(table = TRUE,
                                       sort = TRUE,
                                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                             " {series.name}: ${point.y:,.0f} m"),
                                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                            ) %>%
                            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )

                         ### if any services are selected?
                         if( length(tmp_top_g_country_im)>=1&length(tmp_top_s_country_im)==0 ) {
                            output$KeyImCountryTotalLine <-
                               renderHighchart(
                                  tmp_hc_im_country %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_im %>% filter( Type_gs == 'Goods' ) ,
                                                    mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                    type = 'line',
                                                    marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_g_country_im)-1))
                                     )
                               )
                         }
                         if( length(tmp_top_g_country_im)==0 & length(tmp_top_s_country_im)>=1 ){
                            output$KeyImCountryTotalLine <-
                               renderHighchart(
                                  tmp_hc_im_country %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_im %>% filter( Type_gs == 'Services' ),
                                                    mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                    type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_s_country_im)-1))
                                     )
                               )
                         }
                         if( length(tmp_top_g_country_im)>=1 & length(tmp_top_s_country_im)>=1 ){
                            output$KeyImCountryTotalLine <-
                               renderHighchart(
                                  tmp_hc_im_country %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_im %>% filter( Type_gs == 'Goods' ) ,
                                                    mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                    type = 'line',
                                                    marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_g_country_im)-1))
                                     ) %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_im %>% filter( Type_gs == 'Services' ),
                                                    mapping = hcaes(  x = Year, y = Value, group = SNZ_commodity ),
                                                    type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_s_country_im)-1))
                                     )
                               )
                         }

                         ## III.15 Line graph Key commodity country key Imports Percent KeyImCountryTotalLinePercent ---------------------
                         tmp_im_country_percent_hc <-
                            highchart() %>%
                            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
                            hc_xAxis( categories = c( unique( tmp_dtf_key_line_country_im$Year) ) ) %>%
                            hc_yAxis( title = list(text = "Percentage (%)"),
                                      labels = list( format = "{value:,.1f} %")  ) %>%
                            hc_plotOptions(line = list(
                               dataLabels = list(enabled = F),
                               #stacking = "normal",
                               enableMouseTracking = T)
                            )%>%
                            hc_tooltip(table = TRUE,
                                       sort = TRUE,
                                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                             " {series.name}: {point.y:,.1f} %"),
                                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
                            ) %>%
                            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
                         #hc_legend( enabled = FALSE )

                         ### if any services are selected?
                         if( length(tmp_top_g_country_im)>=1&length(tmp_top_s_country_im)==0 ) {
                            output$KeyImCountryTotalLinePercent <-
                               renderHighchart(
                                  tmp_im_country_percent_hc %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_im %>% filter( Type_gs == 'Goods' ) ,
                                                    mapping = hcaes(  x = Year, y = Share, group = SNZ_commodity ),
                                                    type = 'line',
                                                    marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_g_country_im)-1))
                                     )
                               )
                         }
                         if( length(tmp_top_g_country_im)==0 & length(tmp_top_s_country_im)>=1 ){
                            output$KeyImCountryTotalLinePercent <-
                               renderHighchart(
                                  tmp_im_country_percent_hc %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_im %>% filter( Type_gs == 'Services' ),
                                                    mapping = hcaes(  x = Year, y = Share, group = SNZ_commodity ),
                                                    type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_s_country_im)-1))
                                     )
                               )
                         }
                         if( length(tmp_top_g_country_im)>=1 & length(tmp_top_s_country_im)>=1 ){
                            output$KeyImCountryTotalLinePercent <-
                               renderHighchart(
                                  tmp_im_country_percent_hc %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_im %>% filter( Type_gs == 'Goods' ) ,
                                                    mapping = hcaes(  x = Year, y = Share, group = SNZ_commodity ),
                                                    type = 'line',
                                                    marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_g_country_im)-1))
                                     ) %>%
                                     hc_add_series( data =  tmp_dtf_key_line_country_im %>% filter( Type_gs == 'Services' ),
                                                    mapping = hcaes(  x = Year, y = Share, group = SNZ_commodity ),
                                                    type = 'line', dashStyle = 'DashDot', marker = list(symbol = 'circle') ,
                                                    visible = c(T,rep(F,length(tmp_top_s_country_im)-1))
                                     )
                               )
                         }
                         ## !!!!!!!!!!!!! insert UI ---------------------
                         insertUI(
                            selector = "#country_trade_summary",
                            ui = 
                               div( id = 'country_trade_summary_all_items',
                                    ## 3.3.1.1 Sumary trade table for total selected country ---------------
                                    fluidRow( h1("Trade summary"),
                                              p("Compound annual growth rate (CAGR) for the past 1, 5, 10 years"),
                                              dataTableOutput("CountryTradeTableTotal") ) ,
                                    
                                    ## 3.3.1.1.0 Sumary investment table for total selected country ---------------
                                    fluidRow( h1("Investment position summary"),
                                              p(paste0("Directional basis stock of direct investment for the year ended March ", max(dtf_fdi_odi$Year) , " is used. Compound annual growth rate (CAGR) for the past 1, 5, 10 years")),
                                              dataTableOutput("CountryInvestmentTableTotal") ) ,
                                    
                                    ## 3.3.1.1.1 Sumary Ppl movement table for total selected country ---------------
                                    fluidRow( h1("Visitor movement summary"),
                                              p("Only short-term visitors are included. Compound annual growth rate (CAGR) for the past 1, 5, 10 years"),
                                              dataTableOutput("CountryPplMovementTableTotal") ) ,
                                    
                                    ## 3.3.1.2 Trade Trends graph for total selected country -----------------
                                    fluidRow( h1("Trade trends"),
                                              p("click the legend to hide/show the corresponding series") ),
                                    
                                    ### two way trade and trade deficit graph
                                    fluidRow( h3("Two-way trade and trade balance", align = 'center'),
                                              column(width = 6, h4("Two-way trade"), highchartOutput("CountryTwowayTradeGraphTotal")),
                                              column(width = 6, h4("Trade balance"), highchartOutput("CountryTradeBalanceGraphTotal")) ) ,
                                    
                                    ## total Exports
                                    fluidRow( h3("Total exports, goods exports and services exports" ,align = 'center'),
                                              column( width = 6, h4("Export values"), highchartOutput("CountryExportsGraphTotal") ),
                                              column( width = 6, h4("As a percentage of world exports"), highchartOutput("CountryExportsGraphTotalPercent") ) ) ,
                                    
                                    ## total Impports
                                    fluidRow( h3("Total imports, goods imports and services imports" ,align = 'center'),
                                              column( width = 6, h4("Import values"), highchartOutput("CountryImportsGraphTotal") ),
                                              column( width = 6, h4("As a percentage of world imports"), highchartOutput("CountryImportsGraphTotalPercent") ) ),
                                    
                                    
                                    ## 3.3.1.2.0 Investment position Trends graph for total selected country -----------------
                                    fluidRow( h1("Investment position trends"),
                                              p("Click the legend to hide/show the corresponding series. The series are annual data of March ended.") ),
                                    
                                    ## FDI and ODI ---
                                    fluidRow( h3("Two-way, foreign and overseas direct investment" ,align = 'center'),
                                              column( width = 6, h4("Investment stock values"), highchartOutput("CountryInvestmentGraphTotal") ),
                                              column( width = 6, h4("As a percentage of world"), highchartOutput("CountryInvestmentGraphTotalPercent") ) ),
                                    
                                    ## 3.3.1.2.1 Ppl movement Trends graph for total selected country -----------------
                                    fluidRow( h1("Visitor movement trends"),
                                              p("click the legend to hide/show the corresponding series") ),
                                    
                                    ## FDI and ODI ---
                                    fluidRow( h3("Two-way travel, foreign visitors travelling in and NZ visitors travelling out" ,align = 'center'),
                                              column( width = 6, h4("Number of visitors"), highchartOutput("CountryPplMovementGraphTotal") ),
                                              column( width = 6, h4("As a percentage of world"), highchartOutput("CountryPplMovementGraphTotalPercent") ) ),
                                    
                                    ## 3.3.1.1.2 exports and imports commodity - TREE MAP ----------------
                                    fluidRow( h2(paste0('Key commodities and services')), 
                                              tags$a(href = 'http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports.aspx', "Key commodities and services are defined by Stats NZ", target = "_blank") ),
                                    fluidRow( highchartOutput('KeyExCountryTotalTreeMap') ),
                                    fluidRow( highchartOutput('KeyImCountryTotalTreeMap') ),
                                    
                                    ## 3.3.1.1.3 trends of key exports and imports commodity ---------------
                                    fluidRow( h2(paste0('Trends of key commodities and services')), 
                                              p("Click on the commodity or service names in the legend area to show their trends") ),
                                    fluidRow( h3("key commodities and services EXPORTS", align = 'center'),
                                              column( width = 6, h4("Export values"), highchartOutput('KeyExCountryTotalLine') ),
                                              column( width = 6, h4("As a percentage of world exports"), highchartOutput('KeyExCountryTotalLinePercent') ) ),
                                    fluidRow( h3("key commodities and services IMPORTS", align = 'center'),
                                              column( width = 6, h4("Import values"), highchartOutput('KeyImCountryTotalLine') ),
                                              column( width = 6, h4("As a percentage of world imports"), highchartOutput('KeyImCountryTotalLinePercent') ) )
                                    
                               )
                         )
                         
                         
                         ## IF multiple countries selected -------------------------------
                         if( !tmp_single_country ){
                            ## III.16 Appendix -- Export table CountrySummaryAllExports --------------------------
                            print("------------------  Appendix tables -------------------------")
                            tmp_ex_im_tb_country <- sum_selected_country_individual(tmp_selected_countries)
                            tmp_ex_country_tab <- tmp_ex_im_tb_country$Ex

                            # container of the table
                            sketch_ex <-  htmltools::withTags(table(
                               class = 'display',
                               thead(
                                  tr(
                                     th(rowspan = 2, 'Market'),
                                     th(colspan = 3, 'Total exports'),
                                     th(colspan = 3, 'Goods exports'),
                                     th(colspan = 3, 'Services exports')
                                  ),
                                  tr( #th('Country'),
                                     lapply(rep(c('Value ($m)', 'Share of world market', 'CAGR5'), 3), th, align = 'center')
                                  )
                               )
                            ))

                            output$CountrySummaryAllExports <-
                               renderDataTable({
                                  datatable(tmp_ex_country_tab,
                                            container = sketch_ex,
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(dom = 'Bltp', 
                                                           scrollX = TRUE,
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                           pageLength = 5,
                                                           lengthMenu = list(c(5,  -1), list('5', 'All')),
                                                           columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(tmp_ex_country_tab)-1) ) )
                                                           )
                                             ) %>%
                                     formatPercentage( c('TotExShare', 'TotExCAGR5', 'GExShare', 'GExCAGR5', 'SExShare', 'SExCAGR5'),digit = 1 ) %>%
                                     formatCurrency( columns = c('TotExValue','GExValue', 'SExValue'), digits = 0 ) %>%
                                     formatStyle(
                                        c('TotExCAGR5', 'GExCAGR5', 'SExCAGR5'),
                                        background = styleColorBar( c(0,max(tmp_ex_country_tab[,c('TotExCAGR5','GExCAGR5', 'SExCAGR5')],na.rm=T)*2) ,
                                                                    'lightblue'),
                                        backgroundSize = '100% 90%',
                                        backgroundRepeat = 'no-repeat',
                                        backgroundPosition = 'center',
                                        color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                     ) %>%
                                     formatStyle( 1:ncol(tmp_ex_country_tab), 'vertical-align'='center', 'text-align' = 'center' )
                               })

                            ## III.17 Appendix -- Import table CountrySummaryAllImports--------------------------
                            tmp_im_country_tab <- tmp_ex_im_tb_country$Im

                            # container of the table
                            sketch_im <-  htmltools::withTags(table(
                               class = 'display',
                               thead(
                                  tr(
                                     th(rowspan = 2, 'Market'),
                                     th(colspan = 3, 'Total imports'),
                                     th(colspan = 3, 'Goods imports'),
                                     th(colspan = 3, 'Services imports')
                                  ),
                                  tr( #th('Country'),
                                     lapply(rep(c('Value ($m)', 'Share of world market', 'CAGR5'), 3), th)
                                  )
                               )
                            ))

                            output$CountrySummaryAllImports <-
                               renderDataTable({
                                  datatable(tmp_im_country_tab,
                                            container = sketch_im,
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(dom = 'Bltp', 
                                                           scrollX = TRUE , 
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                           pageLength = 5,
                                                           lengthMenu = list(c(5,  -1), list('5', 'All')),
                                                           columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(tmp_im_country_tab)-1) ) )
                                                           )
                                  ) %>%
                                     formatPercentage( c('TotImShare', 'TotImCAGR5', 'GImShare', 'GImCAGR5', 'SImShare', 'SImCAGR5'),digit = 1 ) %>%
                                     formatCurrency( columns = c('TotImValue','GImValue', 'SImValue'), digits = 0 ) %>%
                                     formatStyle(
                                        c('TotImCAGR5', 'GImCAGR5', 'SImCAGR5'),
                                        background = styleColorBar( c(0,max(tmp_im_country_tab[,c('TotImCAGR5','GImCAGR5', 'SImCAGR5')],na.rm=T)*2) ,
                                                                    'lightblue'),
                                        backgroundSize = '100% 90%',
                                        backgroundRepeat = 'no-repeat',
                                        backgroundPosition = 'center',
                                        color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                     ) %>%
                                     formatStyle( 1:ncol(tmp_im_country_tab), 'vertical-align'='center', 'text-align' = 'center' )
                               })

                            ## III.18 Appendxi -- Twoway trade and balance talbe CountrySummaryAllTwowayBalance------------------
                            tmp_tb_country_tab <-tmp_ex_im_tb_country$TB

                            # container of the table
                            sketch_tb <-  htmltools::withTags(table(
                               class = 'display',
                               thead(
                                  tr(
                                     th(rowspan = 2, 'Market'),
                                     th(colspan = 3, 'Two-way trade'),
                                     th(colspan = 1, 'Trade balance'),
                                     th(colspan = 1, 'Goods balance'),
                                     th(colspan = 1, 'Services balance')
                                     
                                  ),
                                  tr( #th('Country'),
                                     lapply(rep(c('Value ($m)', 'Share of world market', 'CAGR5'), 1), th),
                                     th('Value ($m)'),
                                     th('Value ($m)'),
                                     th('Value ($m)')
                                     
                                  )
                               )
                            ))

                            output$CountrySummaryAllTwowayBalance <-
                               renderDataTable({
                                  datatable(tmp_tb_country_tab,
                                            container = sketch_tb,
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(dom = 'Bltp', 
                                                           scrollX = TRUE,
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                           pageLength = 5,
                                                           lengthMenu = list(c(5,  -1), list('5', 'All')) ,
                                                           columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(tmp_tb_country_tab)-1) ) )
                                            )
                                  ) %>%
                                     formatPercentage( c('TwowayShare', 'TwowayCAGR5') , digit = 1 ) %>%
                                     formatCurrency( columns = c('TwowayValue','BalanceValue','BalanceValue_g','BalanceValue_s'), digits = 0 ) %>%
                                     formatStyle(
                                        c('TwowayCAGR5'),
                                        background = styleColorBar( c(0,max(tmp_tb_country_tab[,c('TwowayCAGR5')],na.rm=T)*2) ,
                                                                    'lightblue'),
                                        backgroundSize = '100% 90%',
                                        backgroundRepeat = 'no-repeat',
                                        backgroundPosition = 'center',
                                        color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                     ) %>%
                                     formatStyle( c('BalanceValue','BalanceValue_g','BalanceValue_s'),
                                                   color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                                   ) %>%
                                     formatStyle( 1:ncol(tmp_tb_country_tab), 'vertical-align'='center', 'text-align' = 'center' )
                               })
                            
                            ## III.19 Appendix -- FDI ODI and Two-wayDI table CountrySummaryAllInvestment --------------------------
                            print("------------------  Appendix Investment tables -------------------------")
                            tmp_invest_tb_country <- sum_selected_country_individual_investment(tmp_selected_countries)
                            
                            # container of the table
                            sketch_invest <-  htmltools::withTags(table(
                               class = 'display',
                               thead(
                                  tr(
                                     th(rowspan = 2, 'Market'),
                                     th(colspan = 3, 'Foreign direct investment'),
                                     th(colspan = 3, 'Overseas direct investment'),
                                     th(colspan = 3, 'Two-way direct investment')
                                  ),
                                  tr( #th('Country'),
                                     lapply(rep(c('Value ($m)', 'Share of world market', 'CAGR5'), 3), th, align = 'center')
                                  )
                               )
                            ))
                            
                            output$CountrySummaryAllInvestment <-
                               renderDataTable({
                                  datatable(tmp_invest_tb_country,
                                            container = sketch_invest,
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(dom = 'Bltp', 
                                                           scrollX = TRUE,
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                           pageLength = 5,
                                                           lengthMenu = list(c(5,  -1), list('5', 'All')),
                                                           columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(tmp_invest_tb_country)-1) ) )
                                            )
                                  ) %>%
                                     formatPercentage( c('FDIShare', 'FDICAGR5', 'ODIShare', 'ODICAGR5', 'TwowayDIShare', 'TwowayDICAGR5'),digit = 1 ) %>%
                                     formatCurrency( columns = c('FDIValue','ODIValue', 'TwowayDIValue'), digits = 0 ) %>%
                                     formatStyle(
                                        c('FDICAGR5', 'ODICAGR5', 'TwowayDICAGR5'),
                                        background = styleColorBar( c(0,max(tmp_invest_tb_country[,c('FDICAGR5','ODICAGR5', 'TwowayDICAGR5')],na.rm=T)*2) ,
                                                                    'lightblue'),
                                        backgroundSize = '100% 90%',
                                        backgroundRepeat = 'no-repeat',
                                        backgroundPosition = 'center',
                                        color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                     ) %>%
                                     formatStyle( 1:ncol(tmp_invest_tb_country), 'vertical-align'='center', 'text-align' = 'center' )
                               })


                            ## III.19 Appendix -- Visitor movement table CountrySummaryAllPplMovement --------------------------
                            print("------------------  Appendix Visitor movement tables -------------------------")
                            tmp_pplmove_tb_country <- sum_selected_country_individual_pplmove(tmp_selected_countries)
                            
                            # container of the table
                            sketch_pplmove <-  htmltools::withTags(table(
                               class = 'display',
                               thead(
                                  tr(
                                     th(rowspan = 2, 'Market'),
                                     th(colspan = 3, 'Foreign visitors travelling in'),
                                     th(colspan = 3, 'NZ visitors travelling out'),
                                     th(colspan = 3, 'Two-way visitor movement')
                                  ),
                                  tr( #th('Country'),
                                     lapply(rep(c("Value ('000)", 'Share of world market', 'CAGR5'), 3), th, align = 'center')
                                  )
                               )
                            ))
                            
                            output$CountrySummaryAllPplMovement <-
                               renderDataTable({
                                  datatable(tmp_pplmove_tb_country,
                                            container = sketch_pplmove,
                                            rownames = FALSE,
                                            extensions = 'Buttons',
                                            options = list(dom = 'Bltp',
                                                           scrollX = TRUE,
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                           pageLength = 5,
                                                           lengthMenu = list(c(5,  -1), list('5', 'All')),
                                                           columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(tmp_invest_tb_country)-1) ) )
                                            )
                                  ) %>%
                                     formatPercentage( c('InShare', 'InCAGR5', 'OutShare', 'OutCAGR5', 'TwowayMoveShare', 'TwowayMoveCAGR5'),digit = 1 ) %>%
                                     formatCurrency( columns = c('InValue','OutValue', 'TwowayMoveValue'), digits = 0, currency = '' ) %>%
                                     formatStyle(
                                        c('InCAGR5', 'OutCAGR5', 'TwowayMoveCAGR5'),
                                        background = styleColorBar( c(0,max(tmp_pplmove_tb_country[,c('InCAGR5','OutCAGR5', 'TwowayMoveCAGR5')],na.rm=T)*2) ,
                                                                    'lightblue'),
                                        backgroundSize = '100% 90%',
                                        backgroundRepeat = 'no-repeat',
                                        backgroundPosition = 'center',
                                        color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
                                     ) %>%
                                     formatStyle( 1:ncol(tmp_pplmove_tb_country), 'vertical-align'='center', 'text-align' = 'center' )
                               })
                            
                            
                            ## !!!!!!!!!!!!!!!! insert UI ------------------------
                            insertUI(
                               selector = "#country_appendix",
                               ui = div( id = "country_trade_summary_appendix",
                                         conditionalPanel( "input.select_country.length > 1 || 
                                                            input.select_country.valueOf() == 'APEC' || 
                                                           input.select_country.valueOf() == 'EU28'||
                                                           input.select_country.valueOf() == 'CPTPP' ||
                                                           input.select_country.valueOf() == 'GCC' ||
                                                           input.select_country.valueOf() == 'Pacific Islands Forum' ||
                                                           input.select_country.valueOf() == 'ASEAN' ||
                                                           input.select_country.valueOf() == 'OECD' ||
                                                           input.select_country.valueOf() == 'Five Eyes' ||
                                                           input.select_country.valueOf() == 'Latin America'||
                                                           input.select_country.valueOf() == 'OPEC' ||
                                                           input.select_country.valueOf() == 'FTA in force' ||
                                                           input.select_country.valueOf() == 'Middle East' || 
                                                           input.select_country.valueOf() == 'Northern Africa' || 
                                                           input.select_country.valueOf() == 'Eastern Africa' || 
                                                           input.select_country.valueOf() == 'Central Africa' || 
                                                           input.select_country.valueOf() == 'Southern Africa' || 
                                                           input.select_country.valueOf() == 'Western Africa' || 
                                                           input.select_country.valueOf() == 'Africa' || 
                                                           input.select_country.valueOf() == 'Arab Maghreb Union' ||
                                                           input.select_country.valueOf() == 'Eastern African Community' ||
                                                           input.select_country.valueOf() == 'Economic Community of West African States' ||
                                                           input.select_country.valueOf() == 'Southern African Development Community' ||
                                                           input.select_country.valueOf() == 'G7' ||
                                                           input.select_country.valueOf() == 'BRI countries' ",
                                                           
                                                           ### trade appendix 
                                                           fluidRow( h1("Appendix -- trade, investment, visitor movement statistics for all selected markets") ),
                                                           fluidRow( h2("Exports"),
                                                                     dataTableOutput("CountrySummaryAllExports") ),
                                                           fluidRow( h2("Imports"),
                                                                     dataTableOutput("CountrySummaryAllImports") ),
                                                           fluidRow( h2("Two-way trade and trade balance"),
                                                                     dataTableOutput("CountrySummaryAllTwowayBalance") ),
                                                           
                                                           ### investment appendix
                                                           #fluidRow( h1("Appendix -- investment position statistics for all selected markets") ),
                                                           fluidRow( h2("Directional investment stocks"),
                                                                     p(paste0("Directional basis stock of direct investment for the year ended March ", max(dtf_fdi_odi$Year) , " is used.")),
                                                                     dataTableOutput("CountrySummaryAllInvestment") ) ,
                                                           
                                                           ### ppl movement appendix
                                                           #fluidRow( h1("Appendix -- visitor movement statistics for all selected markets") ),
                                                           fluidRow( h2("Visitor movement"),
                                                                     dataTableOutput("CountrySummaryAllPplMovement") ) 
                                                           )
                                         )
                            )
                         }
                         ## hide wait message ----
                         shinyjs::hide( id = 'wait_message_country_intel' )

                     }
                   }
                  )
      
      ## IV. HS code finder/ Quick intelligence by HS code -----------------------------------
      ## 4.0.0 setup HS code table values ---------------
      output$HSCodeTable <- 
         renderDataTable({
            datatable( concord_hs24,
                       rownames = FALSE,
                       filter = c("top"),
                       #sDom = "top",
                       extensions = 'Buttons',
                       options = list(dom = 'Bfltp', 
                                      scrollX = TRUE,
                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                      pageLength = 10,
                                      lengthMenu = list(c(10,  -1), list('10', 'All')),
                                      searchHighlight = TRUE,
                                      search = list(regex = TRUE, caseInsensitive = FALSE )
                       ),
                       colnames = c("HS level", "HS code", 'Classification')
                       )
         })
      
      HSCodeTable_proxy = dataTableProxy("HSCodeTable")
      
      ## Insert a button to clear all selections --
      observeEvent( input$action_bnt_ClearTable,
                    ({
                       HSCodeTable_proxy %>% selectRows(NULL)
                    })
      )
      
      ## 4.0 setup this reative values ---------------
      rv_intelHS <- reactiveValues()
      
      ## reactive values ---------------------
      observe({
         ## --- show loading message when click on HS codes -------
         try(
            if( !is.null(input$HSCodeTable_rows_selected) #& 
                #is.null(rv_intelHS$tmp_tab) 
                ){
               shinyjs::show( id = "ci_intel_hs_loading_message" )
            }
         )
         
         ## 4.1 setup hs group table and related reative values ---------------
         rv_intelHS$ie <- input$rbtn_intel_by_hs  ## imports or exports?
         rv_intelHS$total_ie <- paste0('Total ', tolower(rv_intelHS$ie) ) ## Total exports or Total imports
         rv_intelHS$selected_hs_table <- concord_hs24[input$HSCodeTable_rows_selected,] ## selected table
         rv_intelHS$hs <- rv_intelHS$selected_hs_table$HS_codes  ## hs codes
         rv_intelHS$classification <- rv_intelHS$selected_hs_table$HS_description ## hs code desription
         rv_intelHS$hs_group <- 
            data.frame(HS_code = rv_intelHS$hs,
                       HS_group = rv_intelHS$classification) %>%
            arrange( HS_code )
         
         ## 4.2 commodity by country data ----------
         rv_intelHS$tmp_dtf_shiny_full <-
            dtf_shiny_full %>%
            filter( Type_ie == rv_intelHS$ie, 
                    Commodity %in% rv_intelHS$hs_group$HS_code ) %>%
            left_join( rv_intelHS$hs_group, by = c('Commodity' = 'HS_code') ) %>%
            left_join( concord_country_iso_latlon_raw, by = 'Country' ) %>%
            group_by( Year, Country, Type_ie, Type_gs, HS_group, ISO2, lat, lon, Note ) %>%
            summarise( Value = sum(Value, na.rm=T) ) %>%
            ungroup
         
         ## 4.3 commodity only data ------------
         rv_intelHS$tmp_dtf_shiny_full_commodity_only <-
            rv_intelHS$tmp_dtf_shiny_full %>%
            group_by( Year,  Type_ie, Type_gs, HS_group, Note ) %>%
            summarise( Value = sum(Value, na.rm=T) ) %>%
            ungroup %>%
            mutate( Country = 'World' )
         
         ## 4.4 Data for Build export/import value line chart ---------
         rv_intelHS$tmp_top_g <-
            rv_intelHS$tmp_dtf_shiny_full_commodity_only %>%
            filter( Year == max(Year)) %>% 
            arrange( -Value ) %>%
            dplyr::select( HS_group ) %>%
            as.matrix() %>%
            as.character
         
         ## top selected commodities and top 5services
         rv_intelHS$tmp_top <- c( rv_intelHS$tmp_top_g) #, tmp_top_s_ex)
         
         ## data frame to plot
         rv_intelHS$tmp_dtf_key_line <- 
            rv_intelHS$tmp_dtf_shiny_full_commodity_only %>%
            filter( HS_group %in% rv_intelHS$tmp_top,
                    Year >=2007) %>%
            mutate( Value = round(Value/10^6),
                    HS_group = factor(HS_group, levels = rv_intelHS$tmp_top)
            ) %>%
            arrange( HS_group )
         
         ## 4.5 Data for build export/import percent line chart ---------
         rv_intelHS$tmp_tot <-
            dtf_shiny_full %>%
            filter( Country == 'World',
                    Type_ie == rv_intelHS$ie,
                    Year >= 2007 )  %>%
            mutate( Value = round(Value/10^6) ) %>%
            group_by( Year, Country, Type_ie ) %>%
            summarize( Value = sum(Value, na.rm=T) ) %>%
            ungroup %>%
            mutate( HS_group =  rv_intelHS$total_ie )
         
         rv_intelHS$tmp_dtf_percent_line <-
            rv_intelHS$tmp_dtf_key_line %>%
            bind_rows( rv_intelHS$tmp_tot ) %>%
            group_by( Year, Country, Type_ie ) %>%
            mutate( Share = Value/Value[HS_group == rv_intelHS$total_ie ], #'Total exports' ],
                    Value = Share*100 ) %>%
            ungroup %>%
            filter( HS_group != rv_intelHS$total_ie) %>% #'Total exports' ) %>%
            mutate( HS_group = factor(HS_group, levels = rv_intelHS$tmp_top ) ) %>%
            arrange( HS_group )
         
         ## 4.6 Data for build export value change table ----------------
         rv_intelHS$tmp_dtf_key_tab <- 
            rv_intelHS$tmp_dtf_shiny_full_commodity_only %>%
            filter( HS_group %in% rv_intelHS$tmp_top) %>%
            mutate( HS_group = factor(HS_group, levels = rv_intelHS$tmp_top) ) %>%
            arrange( HS_group )
         
         rv_intelHS$tmp_tab_nohs <-
            rv_intelHS$tmp_dtf_key_tab %>%
            mutate( Name =  HS_group ) %>%
            group_by( Name) %>%
            do(CAGR1 = CAGR( .$Value[.$Year == max(.$Year)]/
                                .$Value[.$Year == (max(.$Year)-1)], 1)/100,
               CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                .$Value[.$Year == (max(.$Year)-5)], 5)/100,
               CAGR10 = CAGR( .$Value[.$Year == max(.$Year)]/
                                 .$Value[.$Year == (max(.$Year)-10)], 10)/100 ,
               ABS5 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)-5)],
               ABS10 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)-10)]
            ) %>%
            ungroup %>%
            mutate( CAGR1 = as.numeric(CAGR1),
                    CAGR5 = as.numeric(CAGR5),
                    CAGR10 = as.numeric(CAGR10),
                    ABS5 = as.numeric(ABS5),
                    ABS10 = as.numeric(ABS10)
            ) %>%
            left_join( rv_intelHS$tmp_dtf_key_tab , 
                       by =c('Name'='HS_group') ) %>%
            left_join( rv_intelHS$tmp_dtf_percent_line %>% dplyr::select( -Value) %>% rename(Name = HS_group) ) %>%
            filter( Year == max(Year) ) %>%
            mutate( Value = Value/10^6, ABS5 = ABS5/10^6, ABS10 = ABS10/10^6 ) %>%
            dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10) %>%
            mutate( Name = factor(Name, levels = rv_intelHS$tmp_top),
                    CAGR1 = ifelse(CAGR1 %in% c(Inf,-Inf), NA, CAGR1),
                    CAGR5 = ifelse(CAGR5 %in% c(Inf,-Inf), NA, CAGR5),
                    CAGR10 = ifelse(CAGR10 %in% c(Inf,-Inf), NA, CAGR10)
            ) %>%
            arrange( Name )
         
         ### join back to hs code
         rv_intelHS$hs_group_flat <- 
            rv_intelHS$hs_group %>%
            group_by( HS_group ) %>%
            summarise( HS_code = paste0(HS_code, collapse = '; ') ) %>%
            ungroup
         
         rv_intelHS$tmp_tab <- 
            rv_intelHS$tmp_tab_nohs %>%
            left_join( rv_intelHS$hs_group_flat, by = c("Name"= 'HS_group') ) %>%
            dplyr::select( HS_code, Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10 )
         
         
         ## 4.7 Data Build exports/imports by country output groups -------------------
         ## The name of the selected commodity
         rv_intelHS$tmp_selected <- 
               input$select_comodity_for_market_analysis
         
         ## The HS codes of the selected commodity
         rv_intelHS$tmp_hs <- 
            rv_intelHS$hs_group$HS_code[rv_intelHS$hs_group$HS_group == rv_intelHS$tmp_selected ]
         
         ## The data from of the selected commodity by markets
         rv_intelHS$tmp_dtf_market <- 
               dtf_shiny_full %>%
                  filter( Commodity %in% rv_intelHS$tmp_hs, 
                          Year >= 2007,
                          Type_ie == rv_intelHS$ie ) %>%
                  left_join( concord_country_iso_latlon_raw, by = 'Country' ) %>%
                  left_join( rv_intelHS$hs_group, by = c('Commodity' = 'HS_code') ) %>%
                  group_by( Year, Country, Type_ie, Type_gs, Note, ISO2, lat, lon ) %>%
                  summarize( Value = sum(Value, na.rm=T) ) %>%
                  ungroup %>%
                  mutate( Commodity = as.character( rv_intelHS$tmp_selected ) )
         
         ## 4.8 Data for Value Line and Percentage line for selected commodities ----------------
         rv_intelHS$tmp_dtf_line_selected <-
            rv_intelHS$tmp_dtf_key_line %>%
                  filter( HS_group %in% as.character( rv_intelHS$tmp_selected ))
         
         ## percentage line
         rv_intelHS$tmp_dtf_percent_selected_line <-
            rv_intelHS$tmp_dtf_percent_line %>%
                  filter( HS_group %in% as.character( rv_intelHS$tmp_selected ) )
         
         ## 4.9 Data for build highchart map  ---------------------------
         rv_intelHS$tmp_dtf_market_map <- 
            rv_intelHS$tmp_dtf_market %>%
                  filter( Year == max(Year),
                          !is.na(lat) ) %>%
                  mutate( Value = Value/10^6,
                          z= Value,
                          name = Country)
         
         
         
         ## 4.10 Data for Top markets for selected commodity line chart ----------------
         rv_intelHS$tmp_top_country_selected <- 
               rv_intelHS$tmp_dtf_market %>%
                  filter( Year == max(Year),
                          Value > 0 , 
                          !Country %in% c("World", 
                                          "Destination Unknown - EU")
                  ) %>% ## 1 bn commodity
                  arrange( -Value ) %>%
                  dplyr::select( Country ) %>%
                  as.matrix() %>%
                  as.character
         
         ### only show top 10 countries 
         rv_intelHS$tmp_top10_country_selected <-
            rv_intelHS$tmp_top_country_selected[1:min(10,length( rv_intelHS$tmp_top_country_selected  ))]
         

         ### derive datafrom for the line plot
         rv_intelHS$tmp_dtf_market_line <- 
            rv_intelHS$tmp_dtf_market %>%
                  filter( Country %in%  as.character( rv_intelHS$tmp_top_country_selected ) ) %>%
                  mutate( Value = Value/10^6 ,
                          Country = factor(Country, levels = as.character( rv_intelHS$tmp_top_country_selected ) )
                  ) %>%
                  arrange(Country)
         
         
         rv_intelHS$tmp_dtf_market_line_percent <- 
            rv_intelHS$tmp_dtf_market_line %>%
                  group_by(Year, Type_ie, Type_gs, Note, Commodity) %>%
                  mutate( Share = Value/sum(Value, na.rm=T)) %>%
                  ungroup %>%
                  mutate( Value = Share*100 ) 
         
         
         ## 4.11 Data for Growth prospective tab ----------------------
         rv_intelHS$tmp_tab_growth <-
            rv_intelHS$tmp_dtf_market_line %>%
                  #filter( Country %in% as.character(tmp_top10_country_selected_ex()) ) %>%
                  mutate( Name =  Country ) %>%
                  group_by( Name) %>%
                  do( CAGR1 = CAGR( .$Value[.$Year == max(.$Year)]/
                                       .$Value[.$Year == (max(.$Year)-1)], 1)/100,
                      CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                       .$Value[.$Year == (max(.$Year)-5)], 5)/100,
                      CAGR10 =  CAGR( .$Value[.$Year == max(.$Year)]/
                                         .$Value[.$Year == (max(.$Year)-10)], 10)/100,
                      ABS5 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)- 5)],
                      ABS10 = .$Value[.$Year == max(.$Year)] - .$Value[.$Year == (max(.$Year)- 10)]
                  ) %>%
                  ungroup %>%
                  mutate( CAGR1 = as.numeric(CAGR1), 
                          CAGR5 = as.numeric(CAGR5), 
                          CAGR10 = as.numeric(CAGR10),
                          ABS5 = as.numeric(ABS5),
                          ABS10 = as.numeric(ABS10) ) %>%
                  #filter( Year == max(Year) ) %>%
                  left_join( rv_intelHS$tmp_dtf_market_line %>% rename(Name = Country) %>% filter( Year == max(Year) )  ) %>%
                  left_join( rv_intelHS$tmp_dtf_market_line_percent %>% dplyr::select( -Value ) %>% rename( Name = Country) %>% filter( Year == max(Year) )  ) %>%
                  dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10, ABS5, ABS10) %>%
                  mutate( Name = factor(Name, levels = as.character( rv_intelHS$tmp_top_country_selected ) ) ) %>%
                  arrange( Name )
         
         ## 4.12 Data for global situation from UN comtrade (ONLY for Export analysis) ----------------
         if(  input$rbtn_intel_by_hs == 'Exports' ){
            print("--------- Building Reactive values for global analysis -------------")
            
            ## old code ----------------------
            # rv_intelHS$Fail_uncomtrade_country <- 
            #    try(
            #       rv_intelHS$tmp_global_by_country_raw <-
            #          #get.Comtrade(r="all", p="0", rg = "1,2"  ## 1 means imports; 2 means exports (3 is re-exports excluded here)
            #          #             , ps = paste0(tmp_un_comtrade_max_year, "," ,tmp_un_comtrade_max_year-5)
            #          #             , cc = paste0(rv_intelHS$tmp_hs, collapse = ','), fmt = 'csv' )$data #%>%
            #          # dplyr::select( yr, cmdCode, rgDesc, rtTitle, rt3ISO, ptTitle, qtDesc,  TradeQuantity, TradeValue) %>%
            #          # mutate_all( as.character ) %>%
            #          # mutate( yr = as.numeric(yr),
            #          #         TradeQuantity = as.numeric( TradeQuantity ),
            #          #         TradeValue = as.numeric( TradeValue )
            #          # ) %>%
            #          # rename( Year = yr, `Commodity.Code` = cmdCode ,
            #          #         `Trade.Flow` = rgDesc,
            #          #         Reporter = rtTitle,
            #          #         `Reporter.ISO` = rt3ISO,
            #          #         Partner = ptTitle,
            #          #         `Qty.Unit` = qtDesc,
            #          #         `Alt.Qty.Unit` = TradeQuantity,
            #          #         `Trade.Value..US..` = TradeValue )
            #       
            #       m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
            #                    commod_codes = as.character(rv_intelHS$tmp_hs),
            #                    start_date = tmp_un_comtrade_max_year ,
            #                    end_date = tmp_un_comtrade_max_year) %>%
            #          bind_rows( 
            #             m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
            #                          commod_codes = as.character(rv_intelHS$tmp_hs),
            #                          start_date = tmp_un_comtrade_max_year - 5 ,
            #                          end_date = tmp_un_comtrade_max_year - 5)
            #             ) %>%
            #          #filter( year >= tmp_un_comtrade_max_year-5 &
            #          #           year <= tmp_un_comtrade_max_year ) %>%
            #          dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
            #          rename( Year = year, 
            #                  `Commodity.Code` = commodity_code ,
            #                  `Trade.Flow` = trade_flow,
            #                  Reporter = reporter,
            #                  `Reporter.ISO` =  reporter_iso,
            #                  Partner = partner,
            #                  `Qty.Unit` = qty_unit,
            #                  `Alt.Qty.Unit` = qty,
            #                  `Trade.Value..US..` = trade_value_usd )
            #       
            #    )
            # ## 
            # if( class(rv_intelHS$Fail_uncomtrade) == "try-error" )
            # print(rv_intelHS$Fail_uncomtrade)
            
            ## new download code --------------
            print("----------- Download Uncomtrade trade by country --------------")
            rv_intelHS$Fail_uncomtrade_country <- 
               try(
                  rv_intelHS$tmp_global_by_country_raw_list <- 
                     lapply( as.character(rv_intelHS$tmp_hs) ,
                             function(i){
                                m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                             commod_codes = i,
                                             start_date = tmp_un_comtrade_max_year,
                                             end_date = tmp_un_comtrade_max_year ) %>%
                                   bind_rows(  m_ct_search( reporters = "All", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                                            commod_codes = i,
                                                            start_date = tmp_un_comtrade_max_year - 5,
                                                            end_date = tmp_un_comtrade_max_year - 5 ) 
                                   )
                             } 
                     )
               )
            
            ## try get EU data
            print("----------- Download Uncomtrade trade by EU --------------")
            rv_intelHS$Fail_uncomtrade_eu <- 
               try(
                  rv_intelHS$tmp_global_by_eu_raw_list <- 
                     lapply( as.character(rv_intelHS$tmp_hs) ,
                             function(i){
                                m_ct_search( reporters = "EU-28", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                             commod_codes = i,
                                             start_date = tmp_un_comtrade_max_year,
                                             end_date = tmp_un_comtrade_max_year )  %>%
                                   bind_rows(  m_ct_search( reporters = "EU-28", partners = 'World', trade_direction = c("imports", "exports"), freq = "annual",
                                                            commod_codes = i,
                                                            start_date = tmp_un_comtrade_max_year - 5,
                                                            end_date = tmp_un_comtrade_max_year - 5 ) 
                                   )
                             } 
                     )
               )
            
            
            ## then consolidate the list into dataframe
            if( class(rv_intelHS$Fail_uncomtrade_country) != 'try-error' ){
               print("----------- Success: Download Uncomtrade trade by country --------------")
               ## get list to data frame
               try(
                  rv_intelHS$tmp_global_by_country_raw1 <- 
                     do.call( rbind, rv_intelHS$tmp_global_by_country_raw_list )
               )
               
               ## change names
               try(
                  rv_intelHS$tmp_global_by_country_raw <-
                     rv_intelHS$tmp_global_by_country_raw1 %>%
                     dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
                     rename( Year = year,
                             `Commodity.Code` = commodity_code ,
                             `Trade.Flow` = trade_flow,
                             Reporter = reporter,
                             `Reporter.ISO` =  reporter_iso,
                             Partner = partner,
                             `Qty.Unit` = qty_unit,
                             `Alt.Qty.Unit` = qty,
                             `Trade.Value..US..` = trade_value_usd )
               )
            }
            
            
            if( class(rv_intelHS$Fail_uncomtrade_eu) != 'try-error' ){
               print("----------- Success: Download Uncomtrade trade by EU --------------")
               ## get list to data frame
               try(
                  rv_intelHS$tmp_global_by_eu_raw1 <- 
                     do.call( rbind, rv_intelHS$tmp_global_by_eu_raw_list )
               )
               
               ## change names
               try(
                  rv_intelHS$tmp_global_by_eu_raw <-
                     rv_intelHS$tmp_global_by_eu_raw1 %>%
                     dplyr::select( year, commodity_code, trade_flow, reporter, reporter_iso, partner, qty_unit,  qty, trade_value_usd) %>%
                     rename( Year = year,
                             `Commodity.Code` = commodity_code ,
                             `Trade.Flow` = trade_flow,
                             Reporter = reporter,
                             `Reporter.ISO` =  reporter_iso,
                             Partner = partner,
                             `Qty.Unit` = qty_unit,
                             `Alt.Qty.Unit` = qty,
                             `Trade.Value..US..` = trade_value_usd )
               )
            }
            
            ## 
            if( class(rv_intelHS$Fail_uncomtrade_country) == "try-error" )
               print(rv_intelHS$Fail_uncomtrade_country)
            
            if( class(rv_intelHS$Fail_uncomtrade_eu) == "try-error" )
               print(rv_intelHS$Fail_uncomtrade_eu)
            
         }
         
         ## when both data downloaded successfully then do
         if( class(rv_intelHS$Fail_uncomtrade_country) != "try-error" & 
             class(rv_intelHS$Fail_uncomtrade_eu) != "try-error" & 
             !is.null(rv_intelHS$tmp_global_by_country_raw) & 
             input$rbtn_intel_by_hs == 'Exports'  ){
            ## 1. format the data -----
            print("-------------- 1. Format uncomtrade country data  ------------------")
            ## global import and export of A commodity (sum over all HS code under this commodity) by country
            rv_intelHS$tmp_global_by_country <- 
               rv_intelHS$tmp_global_by_country_raw %>%
               dplyr::select( Year,`Commodity.Code` , `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`, `Alt.Qty.Unit`, `Trade.Value..US..`) %>%
               #group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
               group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner ) %>%
               summarise( `Alt.Qty.Unit` = sum(`Alt.Qty.Unit`, na.rm=T),
                       `Trade.Value..US..` = sum(`Trade.Value..US..`, na.rm=T)
                       ) %>%
               ungroup %>%
               mutate( Price = `Trade.Value..US..`/ `Alt.Qty.Unit`) 
            
            print("-------------- 1.0 Format uncomtrade eu data  ------------------")
            ## EU import and export of A commodity from world
            rv_intelHS$tmp_eu_trade_extra_raw <- 
               rv_intelHS$tmp_global_by_eu_raw %>%
               dplyr::select( Year,`Commodity.Code` , `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`, `Alt.Qty.Unit`, `Trade.Value..US..`) %>%
               #group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
               group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner ) %>%
               summarise( `Alt.Qty.Unit` = sum(`Alt.Qty.Unit`, na.rm=T),
                          `Trade.Value..US..` = sum(`Trade.Value..US..`, na.rm=T)
               ) %>%
               ungroup 
            
            ## 5 yr change in value and prices % and abs 
            rv_intelHS$tmp_global_by_country_change <-    
               rv_intelHS$tmp_global_by_country %>%
               #group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
               group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
               do( Value_per_change = CAGR(.$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year]/
                                              .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ,
                   Value_abs_change = .$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year] - .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5] ,
                   Price_per_change = CAGR(.$Price[.$Year==tmp_un_comtrade_max_year]/
                                              .$Price[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ) %>%
               ungroup %>%
               mutate( Value_per_change = as.numeric(Value_per_change ),
                       Value_abs_change = as.numeric(Value_abs_change ),
                       Price_per_change = as.numeric(Price_per_change )
                       )
            
            ## data frame for producing highchart tables 
            rv_intelHS$tmp_global_by_country_all <- 
               rv_intelHS$tmp_global_by_country %>%
               filter( Year == tmp_un_comtrade_max_year ) %>%
               left_join( rv_intelHS$tmp_global_by_country_change ) %>%
               group_by( Year, Trade.Flow  ) %>%
               mutate( Share = as.numeric(`Trade.Value..US..`)/ sum(as.numeric(`Trade.Value..US..`), na.rm=T ) ) %>%
               ungroup %>%
               arrange( `Trade.Flow`, -`Trade.Value..US..`) 
            
            
            ## 1.1 formate data -- get Eu28 intra and extra trade for later use in table ------
            print("-------------- 1.1 Format uncomtrade eu data  ------------------")
            rv_intelHS$tmp_eu_trade_all <- 
               rv_intelHS$tmp_global_by_country %>%
               filter( Reporter.ISO %in% concord_eu28$ISO3 ) %>%
               #group_by( Year , `Trade.Flow`, Partner, `Qty.Unit` ) %>%
               group_by( Year , `Trade.Flow`, Partner ) %>%
               summarise(  `Alt.Qty.Unit` = sum( as.numeric(`Alt.Qty.Unit`), na.rm=T ),
                           `Trade.Value..US..` = sum( as.numeric(`Trade.Value..US..`), na.rm=T ) ) %>%
               ungroup %>%
               mutate( Reporter = "EU-28", Reporter.ISO = 'EU2'   )
            
            ## derive EU trade intra
            print("-------------- 1.1.2 derive EU trade intra  ------------------")
            rv_intelHS$tmp_eu_trade_intra_raw <-
               rv_intelHS$tmp_eu_trade_all %>%
               left_join( rv_intelHS$tmp_eu_trade_extra_raw,
                          #by = c("Year", "Trade.Flow","Reporter", "Reporter.ISO", "Partner","Qty.Unit" )
                          by = c("Year", "Trade.Flow","Reporter", "Reporter.ISO", "Partner" )
               ) %>%
               mutate( `Alt.Qty.Unit` = Alt.Qty.Unit.x - Alt.Qty.Unit.y, 
                       `Trade.Value..US..` =  `Trade.Value..US...x` - `Trade.Value..US...y` ) %>%
               dplyr::select( -Alt.Qty.Unit.x, -Alt.Qty.Unit.y, 
                              -`Trade.Value..US...x`,  -`Trade.Value..US...y`) #%>%
            #mutate( Partner = "EU-28") 
            
            ### formate data
            print("-------------- 1.1.3 derive EU trade extra  ------------------")
            rv_intelHS$tmp_eu_trade_intra <- 
               rv_intelHS$tmp_eu_trade_intra_raw %>%
               mutate( Reporter = 'EU-28-Intra', Reporter.ISO = 'EU2-intra' )
            
            rv_intelHS$tmp_eu_trade_extra <- 
               rv_intelHS$tmp_eu_trade_extra_raw %>%
               mutate( Reporter = 'EU-28-Extra', Reporter.ISO = 'EU2-extra' )
            
            ## join EU intra and extra back
            print("-------------- 1.1.4 join EU intra and extra back  ------------------")
            rv_intelHS$tmp_global_by_country_and_eu <-
               rv_intelHS$tmp_global_by_country_raw %>%
               filter( !Reporter.ISO %in% concord_eu28$ISO3 ) %>%
               dplyr::select( Year,`Commodity.Code` , `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`, `Alt.Qty.Unit`, `Trade.Value..US..`) %>%
               #group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
               group_by(Year, `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
               summarise( `Alt.Qty.Unit` = sum(`Alt.Qty.Unit`, na.rm=T),
                          `Trade.Value..US..` = sum(`Trade.Value..US..`, na.rm=T)
               ) %>%
               ungroup %>%
               bind_rows( rv_intelHS$tmp_eu_trade_intra ) %>%
               bind_rows( rv_intelHS$tmp_eu_trade_extra  ) %>%
               mutate( Price = `Trade.Value..US..`/ `Alt.Qty.Unit`)
            
            ## 5 yr change in value and prices % and abs 
            print("-------------- 1.1.5 5 yr change in value and prices % and abs   ------------------")
            rv_intelHS$tmp_global_by_country_and_eu_change <-    
               rv_intelHS$tmp_global_by_country_and_eu %>%
               #group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner, `Qty.Unit`) %>%
               group_by( `Trade.Flow`, Reporter, `Reporter.ISO`, Partner) %>%
               do( Value_per_change = CAGR(.$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year]/
                                              .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ,
                   Value_abs_change = .$`Trade.Value..US..`[.$Year==tmp_un_comtrade_max_year] - .$`Trade.Value..US..`[.$Year== (tmp_un_comtrade_max_year)-5] ,
                   Price_per_change = CAGR(.$Price[.$Year==tmp_un_comtrade_max_year]/
                                              .$Price[.$Year== (tmp_un_comtrade_max_year)-5], 5)/100 ) %>%
               ungroup %>%
               mutate( Value_per_change = as.numeric(Value_per_change ),
                       Value_abs_change = as.numeric(Value_abs_change ),
                       Price_per_change = as.numeric(Price_per_change ) )
            
            ## data frame for producing highchart tables 
            print("-------------- 1.1.6 data frame for producing highchart tables  ------------------")
            rv_intelHS$tmp_global_by_country_and_eu_all <- 
               rv_intelHS$tmp_global_by_country_and_eu %>%
               filter( Year == tmp_un_comtrade_max_year ) %>%
               left_join( rv_intelHS$tmp_global_by_country_and_eu_change ) %>%
               group_by( Year, Trade.Flow  ) %>%
               mutate( Share = as.numeric(`Trade.Value..US..`)/ sum(as.numeric(`Trade.Value..US..`), na.rm=T ) ) %>%
               ungroup %>%
               arrange( `Trade.Flow`, -`Trade.Value..US..`)
            ## 2. calculate values for later use ------------   
            print("-------------- 2 Calculate values for facts boxes  ------------------")
            ## Global market size -- value now
            rv_intelHS$tmp_global_size_value_now <- 
               rv_intelHS$tmp_global_by_country %>%
               group_by(Year, `Trade.Flow`,  Partner ) %>%
               summarise(`Trade.Value..US..` = sum(as.numeric(`Trade.Value..US..`), na.rm=T) ) %>%
               ungroup %>%
               filter( Year == tmp_un_comtrade_max_year,
                       `Trade.Flow` == 'Import') %>%
               dplyr::select( `Trade.Value..US..` ) %>%
               as.numeric()
            
            ## Global market size -- value 5 years ago
            rv_intelHS$tmp_global_size_value_pre <- 
               rv_intelHS$tmp_global_by_country %>%
               group_by(Year, `Trade.Flow`,  Partner ) %>%
               summarise(`Trade.Value..US..` = sum(as.numeric(`Trade.Value..US..`), na.rm=T) ) %>%
               ungroup %>%
               filter( Year == tmp_un_comtrade_max_year-5,
                       `Trade.Flow` == 'Import') %>%
               dplyr::select( `Trade.Value..US..` ) %>%
               as.numeric()
            
            ## Global market size -- value change %
            rv_intelHS$tmp_global_size_value_change <-
               CAGR( rv_intelHS$tmp_global_size_value_now/
                        rv_intelHS$tmp_global_size_value_pre, 5)/100
            
            ## Global market size -- value change abs
            rv_intelHS$tmp_global_size_value_change_abs <-
               rv_intelHS$tmp_global_size_value_now - rv_intelHS$tmp_global_size_value_pre 
            
            ## Top 3 importers share
            rv_intelHS$tmp_top3_importers_share <-
               rv_intelHS$tmp_global_by_country_all %>%
               filter( `Trade.Flow` == 'Import' ) %>%
               arrange( -Share ) %>%
               slice(1:3) %>%
               group_by(Year) %>%
               summarise( Share = sum(Share, na.rm=T) ) %>%
               ungroup %>%
               dplyr::select(Share) %>%
               as.numeric
            
            ## Top 10 importers share
            rv_intelHS$tmp_top10_importers_share <-
               rv_intelHS$tmp_global_by_country_all %>%
               filter( `Trade.Flow` == 'Import' ) %>%
               arrange( -Share ) %>%
               slice(1:10) %>%
               group_by(Year) %>%
               summarise( Share = sum(Share, na.rm=T) ) %>%
               ungroup %>%
               dplyr::select(Share) %>%
               as.numeric
            
            ##  of top 20 markets -- number of high growth market
            rv_intelHS$tmp_number_high_growth_importers <-
               nrow(
                  rv_intelHS$tmp_global_by_country_all %>%
                     filter( `Trade.Flow` == 'Import' ) %>%
                     arrange( -Share ) %>%
                     slice(1:20) %>%
                     filter( Value_per_change >= 0.1 )
               )
            
            ## Top 3 exporters share
            rv_intelHS$tmp_top3_exporters_share <-
               rv_intelHS$tmp_global_by_country_all %>%
               filter( `Trade.Flow` == 'Export' ) %>%
               arrange( -Share ) %>%
               slice(1:3) %>%
               group_by(Year) %>%
               summarise( Share = sum(Share, na.rm=T) ) %>%
               ungroup %>%
               dplyr::select(Share) %>%
               as.numeric
            
            ## Top 10 exporters share
            rv_intelHS$tmp_top10_exporters_share <-
               rv_intelHS$tmp_global_by_country_all %>%
               filter( `Trade.Flow` == 'Export' ) %>%
               arrange( -Share ) %>%
               slice(1:10) %>%
               group_by(Year) %>%
               summarise( Share = sum(Share, na.rm=T) ) %>%
               ungroup %>%
               dplyr::select(Share) %>%
               as.numeric
            
            ## NZ's share
            rv_intelHS$tmp_nz_share <-
               rv_intelHS$tmp_global_by_country_all %>%
               filter( `Trade.Flow` == 'Export' ) %>%
               filter( Reporter == 'New Zealand' ) %>%
               dplyr::select(Share) %>%
               as.numeric
            
            ## 3. build data for importers and exporter maps -------------------
            print("-------------- 3 Format uncomtrade data for im/ex maps  ------------------")
            rv_intelHS$tmp_un_comtrade_importer_map <- 
               rv_intelHS$tmp_global_by_country_all %>%
               filter( `Trade.Flow` == "Import" ) %>%
               left_join( concord_uncomtrade_country, by = c('Reporter.ISO' = 'ISO3') ) %>%
               filter( !is.na(lat) ) %>%
               mutate( Value = `Trade.Value..US..`/10^6,
                       z= Value,
                       name = Reporter)
            
            rv_intelHS$tmp_un_comtrade_exporter_map <- 
               rv_intelHS$tmp_global_by_country_all %>%
               filter( `Trade.Flow` == "Export" ) %>%
               left_join( concord_uncomtrade_country, by = c('Reporter.ISO' = 'ISO3') ) %>%
               filter( !is.na(lat) ) %>%
               mutate( Value = `Trade.Value..US..`/10^6,
                       z= Value,
                       name = Reporter)

            ## 4. Build data for the summary table -----------------
            print("-------------- 4 Format uncomtrade data for summary table  ------------------")
            ## import tab
            rv_intelHS$tmp_un_comtrade_import_summary_tab <- 
               rv_intelHS$tmp_global_by_country_and_eu_all %>%
               filter( `Trade.Flow` == 'Import' ) %>%
               dplyr::select( Reporter, Share, 
                              `Trade.Value..US..` ,Value_per_change, Value_abs_change,  
                              Price, Price_per_change ) %>%
               mutate( `Trade.Value..US..` = `Trade.Value..US..`/10^6,
                       Value_abs_change = Value_abs_change/10^6)
            
            ## export tab
            rv_intelHS$tmp_un_comtrade_export_summary_tab <- 
               rv_intelHS$tmp_global_by_country_and_eu_all %>%
               filter( `Trade.Flow` == 'Export' ) %>%
               dplyr::select( Reporter, Share, 
                              `Trade.Value..US..` ,Value_per_change, Value_abs_change,  
                              Price, Price_per_change ) %>%
               mutate( `Trade.Value..US..` = `Trade.Value..US..`/10^6,
                       Value_abs_change = Value_abs_change/10^6)
         }
      })
      
      # ## some tests on reative values -----
      # output$testHS <- renderDataTable({
      #    req(rv_intelHS$selected_hs_table)
      #    rv_intelHS$tmp_global_by_country_all
      # })
      # 
      # insertUI( selector = '#ci_intel_by_hs_toadd',
      #           ui = dataTableOutput("testHS") )
      
      ### 4.4 and 4.5 generating value and percent line plots --------------
      output$CIExportImportValueLine <-
         renderHighchart({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            highchart() %>%
               hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
               hc_xAxis( categories = c( unique( rv_intelHS$tmp_dtf_key_line$Year) ) ) %>%
               hc_yAxis( title = list(text = "$ million, NZD"),
                         labels = list( format = "${value:,.0f} m")  ) %>%
               hc_plotOptions(line = list(
                  dataLabels = list(enabled = F),
                  #stacking = "normal",
                  enableMouseTracking = T #,
                  #series = list(events = list(legendItemClick = sharelegend)) ,
                  #showInLegend = T
               )
               )%>%
               hc_tooltip(table = TRUE,
                          sort = TRUE,
                          pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                " {series.name}: ${point.y} m"),
                          headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
               ) %>%
               hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
               hc_add_series( data =  rv_intelHS$tmp_dtf_key_line %>% filter( Type_gs == 'Goods' ) ,
                              mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                              type = 'line',
                              marker = list(symbol = 'circle') #,
                              #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
               )
         })
      

      ### plot
      output$CIExportImportPercentLine <-
         renderHighchart({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            highchart() %>%
               hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
               hc_xAxis( categories = c( unique( rv_intelHS$tmp_dtf_percent_line$Year) ) ) %>%
               hc_yAxis( title = list(text = "Percentage (%)"),
                         labels = list( format = "{value:,.1f} %")  ) %>%
               hc_plotOptions(line = list(
                  dataLabels = list(enabled = F),
                  #stacking = "normal",
                  enableMouseTracking = T)
               )%>%
               hc_tooltip(table = TRUE,
                          sort = TRUE,
                          pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                " {series.name}: {point.y:,.1f} %"),
                          headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
               ) %>%
               hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
               hc_add_series( data =  rv_intelHS$tmp_dtf_percent_line %>% filter( Type_gs == 'Goods' ) ,
                              mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                              type = 'line',
                              marker = list(symbol = 'circle') #,
                              #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
               )
         })

      ## !!!!! try UI  commodities selected -----------
      output$H1_title_value_percent <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            paste0( rv_intelHS$ie ," for selected commodities")
         })
      
      output$H1_title_value_percent_note <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            paste0(  "Click on the commodity names in the legend area to show their trends" )
         })
      
      output$H4_title_value <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            paste0( rv_intelHS$ie ," values")
         })
      
      output$H4_title_percent <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            paste0( "As a percent of total ", tolower(rv_intelHS$ie) )
         })
         
      insertUI(
         selector = '#ci_intel_by_hs_toadd',
         ui =   div( id = 'ci_intel_by_hs_line_value_percent',
                     fluidRow(
                        h1( HTML(paste(textOutput("H1_title_value_percent"))) ),
                        p( HTML(paste(textOutput("H1_title_value_percent_note")))  ),
                        column(6, div(id = "ci_intel_by_hs_value",
                                      h4( HTML(paste(textOutput("H4_title_value"))) ),
                                      highchartOutput('CIExportImportValueLine') ) ),
                        column(6, div(id = "ci_intel_by_hs_percent",
                                      h4( HTML(paste(textOutput("H4_title_percent"))) ),
                                      highchartOutput('CIExportImportPercentLine') ) ))
         )
      )
      ## end Try UI insert --------##
      
      
      ### 4.6 Generating commodity change table -------------------
      output$GrowthTabSelected <- 
         renderDataTable({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            datatable( rv_intelHS$tmp_tab,
                       rownames = F,
                       filter = c("top"),
                       extensions = c('Buttons' ),
                       options = list(dom = 'Bfltp', #'Bltp',# 'Bt',
                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1, 
                                      ,scrollX = TRUE
                                      #,fixedColumns = list(leftColumns = 2) 
                                      ,autoWidth = T
                                      ,pageLength = 10
                                      ,lengthMenu = list(c(10,  -1), list('10', 'All')) ,
                                      searchHighlight = TRUE,
                                      search = list(regex = TRUE, caseInsensitive = FALSE )
                                      
                       ) ,
                       colnames = c("HS codes", "Classification","Value ($m)", paste0("Share of total ", tolower(rv_intelHS$ie) ), 'CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10')
             ) %>%
               formatStyle(
                  c('CAGR1', 'CAGR5', 'CAGR10'),
                  background = styleColorBar( c(0, max(c(rv_intelHS$tmp_tab$CAGR1,rv_intelHS$tmp_tab$CAGR5, rv_intelHS$tmp_tab$CAGR10))*2, na.rm=T) , 'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center'
               ) %>%
               formatStyle(c('CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'),
                           color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>%
               formatPercentage( c('Share','CAGR1', 'CAGR5', 'CAGR10'),digit = 1 ) %>%
               formatStyle( columns = c('Name','Value', 'Share', 'CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'), `font-size`= '115%' ) %>%
               formatCurrency( columns = c('Value', 'ABS5', 'ABS10'), mark = ' ', digits = 1)
         })
      
      ## !!!!! try UI insert: Commodity change table ----------- 
      output$H1_title_growth_tab <- 
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            paste0( "Short, medium, and long term growth for the selected commodities" )
         })
      
      output$H1_title_growth_tab_note <- 
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            paste0( "Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years." )
         })
      
      ## insert ui here
      insertUI(
         selector = '#ci_intel_by_hs_toadd',
         ui =   div( id = 'ci_intel_by_hs_toadd_growth_tab',
                     fluidRow( h1( HTML(paste(textOutput("H1_title_growth_tab"))) ),
                               p( HTML(paste(textOutput("H1_title_growth_tab_note"))) ) ,
                               dataTableOutput('GrowthTabSelected')
                     )
         )
      )
      ## end Try UI insert --------##
      
      
      ## 4.7 Build exports/imports by country output groups -------------------
      ## create a selector for each selected commodity 
      output$Commodity_Selector_note <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            paste0( "Please select or search a commodity for its market analysis" )
         })
      
      output$H1_title_Commodity_Selector <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            paste0( gsub("s", "", rv_intelHS$ie) ," markets analysis for selected commodity" )
         })
      
      output$CISelectorByMarkets <- renderUI({
         if( is.null(input$HSCodeTable_rows_selected) )
            return(NULL)
         selectizeInput("select_comodity_for_market_analysis",
                        # tags$p( HTML(paste(textOutput("Commodity_Selector_note "))) ), 
                        # choices = rv_intelHS$tmp_tab$Name[input$GrowthTabSelected_rows_all], # tmp_top_ex, 
                        # selected = NULL, #tmp_top_ex[1], 
                        # width = "500px",
                        # multiple = F
                        tags$p("Please select or search a commodity for its market analysis"), 
                        choices =  c('Please select a commodity' = "" , 
                                     as.character(rv_intelHS$tmp_tab$Name)
                        ), #input$select_comodity_ex,
                        selected = NULL,  width = "500px",
                        multiple = F)
      })
      
      ### selcted commodity and service outputs
      output$Selected <- 
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) )
               return(NULL)
            rv_intelHS$tmp_selected
         })
      
      ## !!!!! try UI insert for the selectors  ---------------
      insertUI(
         selector = '#ci_intel_by_hs_toadd',
         ui =   div( id = 'ci_intel_by_hs_toadd_markets_selector',
                     fluidRow(h1( HTML(paste0(textOutput("H1_title_Commodity_Selector"))) ),
                              uiOutput("CISelectorByMarkets") ),
                     fluidRow( shiny::span(h1( HTML(paste0(textOutput("Selected"))), align = "center" ), style = "color:darkblue" ) )
         )
      )
      ## end Try UI insert -----------##
      
      ## --- show loading message when select a commodity -------
      observe(
         try(
            if( !is.null(input$select_comodity_for_market_analysis) & 
                input$select_comodity_for_market_analysis!= "" ){
               shinyjs::show( id = "ci_intel_hs_loading_message_intl" )
            }
         )
      )

      ## 4.8 Plot Value Line and Percentage line for selected commodities ----------------
      output$CISelectedValueLine <- 
         renderHighchart({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            highchart() %>%
               hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
               hc_xAxis( categories = c( unique( rv_intelHS$tmp_dtf_line_selected$Year) ) ) %>%
               hc_yAxis( title = list(text = "$ million, NZD"),
                         labels = list( format = "${value:,.0f} m")  ) %>%
               hc_plotOptions(line = list(
                  dataLabels = list(enabled = F),
                  #stacking = "normal",
                  enableMouseTracking = T #,
                  #series = list(events = list(legendItemClick = sharelegend)) ,
                  #showInLegend = T
               )
               )%>%
               hc_tooltip(table = TRUE,
                          sort = TRUE,
                          pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                " {series.name}: ${point.y} m"),
                          headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
               ) %>%
               hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
               hc_add_series( data =  rv_intelHS$tmp_dtf_line_selected %>% filter( Type_gs == 'Goods' ) ,
                              mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                              type = 'line',
                              marker = list(symbol = 'circle') #,
                              #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
               )
        })
      
      # ### plot
      output$CISelectedPercentLine <-
         renderHighchart({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            highchart() %>%
               hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
               hc_xAxis( categories = c( unique( rv_intelHS$tmp_dtf_percent_selected_line$Year) ) ) %>%
               hc_yAxis( title = list(text = "Percentage (%)"),
                         labels = list( format = "{value:,.1f} %")  ) %>%
               hc_plotOptions(line = list(
                  dataLabels = list(enabled = F),
                  #stacking = "normal",
                  enableMouseTracking = T)
               )%>%
               hc_tooltip(table = TRUE,
                          sort = TRUE,
                          pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                " {series.name}: {point.y:,.1f} %"),
                          headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
               ) %>%
               hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 ) %>%
               hc_add_series( data =  rv_intelHS$tmp_dtf_percent_selected_line %>% filter( Type_gs == 'Goods' ) ,
                              mapping = hcaes(  x = Year, y = Value, group = HS_group ),
                              type = 'line',
                              marker = list(symbol = 'circle') #,
                              #visible = c(T,rep(F,length(tmp_top_g_ex)-1))
               )
         })
      
      ## !!!!! try UI insert: selected value and percnet line -----------
      output$H2_title_selected_value_percent_line <- 
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            paste0( rv_intelHS$ie , " trends" )
         })
      
      output$H2_title_selected_value_percent_line_note <- 
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            paste0( "Click on the commodity names in the legend area to show their trends" )
         })
      
      output$H4_title_selected_value_line <- 
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            paste0( rv_intelHS$ie , " values" )
         })
      
      output$H4_title_selected_percent_line <- 
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            paste0( "As a percent of total ", tolower(rv_intelHS$ie)  )
         })
      
      ## UI here 
      insertUI(
         selector = '#ci_intel_by_hs_toadd_intl',
         ui =   div( id = 'ci_intel_by_hs_toadd_selected_line_value_percent',
                     fluidRow( h2( HTML(paste0(textOutput("H2_title_selected_value_percent_line"))) ),
                               p( HTML(paste0(textOutput("H2_title_selected_value_percent_line_note"))) ),
                               column(6, div(id = "ci_intel_by_hs_value_selected", h4( HTML(paste0(textOutput("H4_title_selected_value_line"))) ), highchartOutput('CISelectedValueLine') ) ),
                               column(6, div(id = "ci_intel_by_hs_percent_selected", h4( HTML(paste0(textOutput("H4_title_selected_percent_line"))) ), highchartOutput('CISelectedPercentLine') ) )
                               )
         )
      )
      ## end Try UI insert --------##
      
      
      
      ## 4.9 Plot  for build highchart map  ---------------------------
      output$MapMarket <- 
         renderHighchart({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "")
               return(NULL)
            hcmap( data = rv_intelHS$tmp_dtf_market_map ,
                   value = 'Value',
                   joinBy = c('iso-a2','ISO2'), 
                   name= paste0( rv_intelHS$ie, " value"),
                   borderWidth = 1,
                   borderColor = "#fafafa",
                   nullColor = "lightgrey",
                   tooltip = list( table = TRUE,
                                   sort = TRUE,
                                   headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                   pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
            ) %>%
               hc_add_series(data =  rv_intelHS$tmp_dtf_market_map,
                             type = "mapbubble",
                             color  = hex_to_rgba("#f1c40f", 0.9),
                             minSize = 0,
                             name= paste0( rv_intelHS$ie," value"),
                             maxSize = 30,
                             tooltip = list(table = TRUE,
                                            sort = TRUE,
                                            headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                            pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
               ) %>%
               hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
               hc_legend( enabled=FALSE ) %>% 
               hc_mapNavigation(enabled = TRUE) 
         })
      
      ## !!!!! try UI insert: map of importer / exporters ----------- 
      output$H2_title_map_selected <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            paste0("Map of ", gsub("s","", tolower(rv_intelHS$ie)) ," values")
         })
      
      output$H2_title_map_selected_note <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            paste0( "The size of bubble area and color both represent the value of ", tolower(rv_intelHS$ie) ) 
         })
      
      ## Insert ui here
      insertUI(
         selector = '#ci_intel_by_hs_toadd_intl',
         ui =   div( id = 'ci_intel_by_hs_toadd_markets_map',
                     fluidRow(h2( HTML(paste0(textOutput("H2_title_map_selected"))) ) ,
                              p( HTML(paste0(textOutput("H2_title_map_selected_note"))) ),
                              highchartOutput('MapMarket') )
         )
      )
      ## end Try UI insert --------##
      
      
      
      ## 4.10 Plot fro Top markets for selected commodity line chart ----------------
      output$SelectedMarketLine <- renderHighchart({
         if( is.null(input$HSCodeTable_rows_selected)| input$select_comodity_for_market_analysis == "" )
            return(NULL)
         highchart() %>%
            hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
            hc_add_series( data =  rv_intelHS$tmp_dtf_market_line %>%
                              filter( Country %in% as.character( rv_intelHS$tmp_top10_country_selected  ) ),
                           mapping = hcaes(  x = Year, y = Value, group = Country),
                           type = 'line',
                           marker = list(symbol = 'circle'), 
                           visible = c( rep(T,5), rep(F,length( as.character( rv_intelHS$tmp_top10_country_selected  ) )-5) )
            ) %>%
            hc_xAxis( categories = c( unique( rv_intelHS$tmp_dtf_market_line$Year) ) ) %>%
            hc_yAxis( title = list(text = "$ million, NZD"),
                      labels = list( format = "${value:,.0f} m")  ) %>%
            hc_plotOptions(line = list(
               dataLabels = list(enabled = F),
               #stacking = "normal",
               enableMouseTracking = T)
            )%>%
            hc_tooltip(table = TRUE,
                       sort = TRUE,
                       pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                             " {series.name}: ${point.y:,.0f} m"),
                       headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
            ) %>%
            hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
      })
      
      output$SelectedMarketLinePercent <-
         renderHighchart({
            if( is.null(input$HSCodeTable_rows_selected)| input$select_comodity_for_market_analysis == "" )
               return(NULL)
            highchart() %>%
               hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
               hc_add_series( data =  rv_intelHS$tmp_dtf_market_line_percent %>%
                                 filter( Country %in% as.character( rv_intelHS$tmp_top10_country_selected  ) ),
                              mapping = hcaes(  x = Year, y = Value, group = Country),
                              type = 'line',
                              marker = list(symbol = 'circle'), 
                              visible = c( rep(T,5), rep(F,length( as.character( rv_intelHS$tmp_top10_country_selected ) )-5) )
               ) %>%
               hc_xAxis( categories = c( unique( rv_intelHS$tmp_dtf_market_line_percent$Year) ) ) %>%
               hc_yAxis( title = list(text = "Percentage (%)"),
                         labels = list( format = "{value:,.1f} %")  ) %>%
               hc_plotOptions(line = list(
                  dataLabels = list(enabled = F),
                  #stacking = "normal",
                  enableMouseTracking = T)
               )%>%
               hc_tooltip(table = TRUE,
                          sort = TRUE,
                          pointFormat = paste0( '<br> <span style="color:{point.color}">\u25CF</span>',
                                                " {series.name}: {point.y:,.1f} %"),
                          headerFormat = '<span style="font-size: 13px">Year {point.key}</span>'
               ) %>%
               hc_legend( layout = 'vertical', align = 'left', verticalAlign = 'top', floating = T, x = 100, y = -15 )
         })
      
      ## !!!!! try UI insert: top markets value and percent line  ----------- 
      output$H2_title_top_market <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0( "Top 10 ", gsub("s","",tolower(rv_intelHS$ie)) ," markets trends"  )
         })
      
      output$H2_title_top_market_note <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0( "Click on the country names in the legend area to show their trends"  )
         })
      
      output$H4_title_top_market_value <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0( rv_intelHS$ie, " values"  )
         })
      
      output$H4_title_top_market_percent <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0( "As a percent of total ", tolower(rv_intelHS$ie) ," of the selected" )
         })
      
      ## insert ui here    
      insertUI(
         selector = '#ci_intel_by_hs_toadd_intl',
         ui =   div( id = 'ci_intel_by_hs_toadd_markets_top',
                     fluidRow( h2( HTML(paste0(textOutput("H2_title_top_market"))) ),
                               p( HTML(paste0(textOutput("H2_title_top_market_note"))) ),
                               column(6, 
                                      h4( HTML(paste0(textOutput("H4_title_top_market_value"))) ),
                                      highchartOutput("SelectedMarketLine") 
                               ),
                               column(6,
                                      h4( HTML(paste0(textOutput("H4_title_top_market_percent"))) ),
                                      highchartOutput("SelectedMarketLinePercent")
                               )
                     )
         )
      )
      ## end Try UI insert --------##
      
      
      
      ## 4.11 Table for Growth prospective tab ----------------------
      output$SelectedMarketGrowthTab <- renderDataTable({
         if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "")
            return(NULL)
         datatable( rv_intelHS$tmp_tab_growth,
                    rownames = F,
                    extensions = 'Buttons',
                    options = list(dom = 'Bltp',# 'Bt', 
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print') #, pageLength = -1
                                   ,scrollX = TRUE
                                   ,pageLength = 10
                                   ,lengthMenu = list(c(10,  -1), list('10', 'All')) 
                                   #,fixedColumns = list(leftColumns = 2) 
                                   #,autoWidth = T
                    ) ,
                    colnames=c("Markets", "Value ($m)", "Share of world marekt", 'CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10')
         ) %>%
            formatStyle(
               c('CAGR1', 'CAGR5', 'CAGR10'),
               background = styleColorBar( c(0, max(c( rv_intelHS$tmp_tab_growth$CAGR1,
                                                       rv_intelHS$tmp_tab_growth$CAGR5,
                                                       rv_intelHS$tmp_tab_growth$CAGR10))*2, na.rm=T) , 'lightblue'),
               backgroundSize = '100% 90%',
               backgroundRepeat = 'no-repeat',
               backgroundPosition = 'center'
            ) %>%
            formatStyle(c('CAGR1', 'CAGR5', 'CAGR10', 'ABS5', 'ABS10'),
                        color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>%
            formatPercentage( c('Share', 'CAGR1', 'CAGR5', 'CAGR10'),digit = 1 ) %>%
            formatStyle( columns = c('Name', "Value", "Share" ,'CAGR1', 'CAGR5', 'CAGR10'), `font-size`= '115%' ) %>%
            formatCurrency( columns = c("Value", 'ABS5', 'ABS10'), mark = ' ', digits = 1)
      })
      
      
      ## !!!!! try UI insert: growth tab by markets ----------- 
      output$H2_title_growth_market <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0( "Top ", gsub("s","",rv_intelHS$ie) ," markets growth prospective" )
         })
      
      output$H2_title_growth_market_note <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0( "Compound annual growth rate (CAGR) for the past 1, 5, and 10 years. Absolute value change (ABS) for the past 5 and 10 years." )
         })
         
      insertUI(
         selector = '#ci_intel_by_hs_toadd_intl',
         ui =   div( id = 'ci_intel_by_hs_toadd_markets_growth',
                     fluidRow( h2( HTML(paste0(textOutput("H2_title_growth_market"))) ),
                               p( HTML(paste0(textOutput("H2_title_growth_market_note"))) ),
                               dataTableOutput("SelectedMarketGrowthTab")
                     )
         )
      )
      ## end Try UI insert --------##
      
      ## 4.12 UN com Trade data analysis starts here Key facts table ----------
      ## world market size
      print("--------- Building facts value boxes -------------")
      output$Un_comtrade_world_market_size <-
         renderInfoBox({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            infoBox( "World market size",
                     paste0("$", 
                            format(round(rv_intelHS$tmp_global_size_value_now/10^6), big.mark = ","),
                            " m"
                            )
                     , icon = icon('globe', lib = "glyphicon")
               
            )
         })
      
      ## 5 year growth
      output$Un_comtrade_world_market_change <-
         renderInfoBox({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            
            if( is.null(rv_intelHS$tmp_global_size_value_change) )
               infoBox( "CAGR (5 years)",
                        HTML(paste0( "Not available" )), 
                        icon = icon('minus'))
               
            if(rv_intelHS$tmp_global_size_value_change>0 ){
               infoBox( "CAGR (5 years)",
                        HTML(paste0( "<font color='green'> +",
                           round(abs(rv_intelHS$tmp_global_size_value_change)*100,1),
                           "% </font>"
                        )), 
                        icon = icon('arrow-up'), color = 'green')
            }else{
               infoBox( "CAGR (5 years)",
                        HTML(paste0( "<font color='red'> -",
                           round(abs(rv_intelHS$tmp_global_size_value_change)*100,1),
                           "% </font>"
                        )), 
                        icon = icon('arrow-down'), color = 'red')
            }
            
         })
      
      ## 5 yr abs change
      output$Un_comtrade_world_market_change_abs <-
         renderInfoBox({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports'| input$select_comodity_for_market_analysis == "" )
               return(NULL)
            
            if( is.null(rv_intelHS$tmp_global_size_value_change_abs) )
               infoBox( "ABS (5 years)",
                        HTML(paste0( "Not available" )), 
                        icon = icon('minus'))
            
            if(rv_intelHS$tmp_global_size_value_change_abs>0 ){
               infoBox( "ABS (5 years)",
                        HTML(paste0("<font color='green'> +$", 
                               format(round(rv_intelHS$tmp_global_size_value_change_abs/10^6), big.mark = ","),
                               " m </font>"
                        )),
                        icon = icon('arrow-up'), color = 'green')
            }else{
               infoBox( "ABS (5 years)",
                        HTML(paste0("<font color='red'> -$", 
                               format(round(abs(rv_intelHS$tmp_global_size_value_change_abs)/10^6), big.mark = ","),
                               " m </font>"
                        )),
                        icon = icon('arrow-down'), color = 'red')
            }
         })
      
      ## top 3 importer share
      output$Un_comtrade_top3_importers_share <-
         renderInfoBox({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports'| input$select_comodity_for_market_analysis == "" )
               return(NULL)
            infoBox( HTML("Top 3 importers <br> share"),
                     paste0( 
                        round(abs(rv_intelHS$tmp_top3_importers_share)*100,1),
                            "%"
                     ),
                     icon = icon('import', lib = "glyphicon"))
         })
      
      ## top 10 importer share
      output$Un_comtrade_top10_importers_share <-
         renderInfoBox({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            infoBox( HTML("Top 10 importers <br> share"),
                     paste0( 
                        round(abs(rv_intelHS$tmp_top10_importers_share)*100,1),
                        "%"
                     ),
                     icon = icon('import', lib = "glyphicon"))
         })
      
      ##  of top 20 markets -- number of high growth market
      output$Un_comtrade_high_growth_importers <-
         renderInfoBox({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports'| input$select_comodity_for_market_analysis == "" )
               return(NULL)
            infoBox( HTML("Top 20 importers <br> with CAGR>10%"),
                     paste0( rv_intelHS$tmp_number_high_growth_importers) ,
                     icon = icon('import', lib = "glyphicon"))
         })
      
      
      ## top 3 exporter share
      output$Un_comtrade_top3_exporters_share <-
         renderInfoBox({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            infoBox( HTML("Top 3 exporters <br> share"),
                     paste0( 
                        round(abs(rv_intelHS$tmp_top3_exporters_share)*100,1),
                        "%"
                     ),
                     icon = icon('export', lib = "glyphicon"))
         })
      
      ## top 10 exporter share
      output$Un_comtrade_top10_exporters_share <-
         renderInfoBox({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            infoBox( HTML("Top 10 exporters <br> share"),
                     paste0( 
                        round(abs(rv_intelHS$tmp_top10_exporters_share)*100,1),
                        "%"
                     ),
                     icon = icon('export', lib = "glyphicon"))
         })
      
      ## new zealand share
      output$Un_comtrade_nz_share <-
         renderInfoBox({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports'| input$select_comodity_for_market_analysis == "" )
               return(NULL)
            if( rv_intelHS$tmp_nz_share < 0.001 ){
               infoBox( HTML("New Zealand <br> share"),
                        paste0( "Less than 0.1%" ),
                        icon = icon('export', lib = "glyphicon"))
            }else{
               infoBox( HTML("New Zealand <br> share"),
                        paste0( 
                           round(abs(rv_intelHS$tmp_nz_share)*100,1),
                           "%"
                        ),
                        icon = icon('export', lib = "glyphicon"))
            }
            
         })
      
      
      ##!!!!! try UI insert: value box for global market facts ----------- 
      output$H1_title_global_facts <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            paste0( "Global market analysis (", tmp_un_comtrade_max_year ,")" )
         })
      
      output$H1_title_global_facts_note <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            paste0( "All values undner the global market analysis are reported in current US dollar" )
         })
      
      output$H3_title_global_facts_summary <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "" )
               return(NULL)
            paste0( "Key facts and summary" )
         })
      
      ### insert global market key facts and summary value boxe
      insertUI(
         selector = '#ci_intel_by_hs_toadd_intl',
         ui =   div( id = 'ci_intel_by_hs_toadd_global_facts',
                     fluidRow( 
                        h1( HTML(paste0(textOutput("H1_title_global_facts"))) ),
                        p( HTML(paste0(textOutput("H1_title_global_facts_note"))) ),
                        h3( HTML(paste0(textOutput("H3_title_global_facts_summary"))) ),
                        infoBoxOutput("Un_comtrade_world_market_size") ,
                        infoBoxOutput("Un_comtrade_world_market_change" ) ,
                        infoBoxOutput("Un_comtrade_world_market_change_abs" ) 
                     ),
                     fluidRow(
                        infoBoxOutput("Un_comtrade_top3_importers_share" ) ,
                        infoBoxOutput("Un_comtrade_top10_importers_share" ) ,
                        infoBoxOutput("Un_comtrade_high_growth_importers" ) 
                     ),
                     fluidRow(
                        infoBoxOutput("Un_comtrade_top3_exporters_share" ) ,
                        infoBoxOutput("Un_comtrade_top10_exporters_share" ) ,
                        infoBoxOutput("Un_comtrade_nz_share" ) 
                     )
         )
      )
      
      
      ## 4.13 Quick glance at both importers and exporters map --------
      print("--------- Building importer and exporter map -------------")
      output$UN_comtrade_importer_Map <- 
         renderHighchart({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            hcmap( data = rv_intelHS$tmp_un_comtrade_importer_map ,
                   value = 'Value',
                   joinBy = c('iso-a2','ISO2'), 
                   name= paste0( "Import value"),
                   borderWidth = 1,
                   borderColor = "#fafafa",
                   nullColor = "lightgrey",
                   tooltip = list( table = TRUE,
                                   sort = TRUE,
                                   headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                   pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
            ) %>%
               hc_add_series(data =  rv_intelHS$tmp_un_comtrade_importer_map ,
                             type = "mapbubble",
                             color  = hex_to_rgba("#DF1995", 0.9),
                             minSize = 0,
                             name= paste0( "Import value"),
                             maxSize = 30,
                             tooltip = list(table = TRUE,
                                            sort = TRUE,
                                            headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                            pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
               ) %>%
               hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
               hc_legend( enabled=FALSE ) %>% 
               hc_mapNavigation(enabled = TRUE) 
         })
      
      ## exporter map
      output$UN_comtrade_exporter_Map <- 
         renderHighchart({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports'| input$select_comodity_for_market_analysis == "" )
               return(NULL)
            hcmap( data = rv_intelHS$tmp_un_comtrade_exporter_map ,
                   value = 'Value',
                   joinBy = c('iso-a2','ISO2'), 
                   name= paste0( "Export value"),
                   borderWidth = 1,
                   borderColor = "#fafafa",
                   nullColor = "lightgrey",
                   tooltip = list( table = TRUE,
                                   sort = TRUE,
                                   headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                   pointFormat = '{point.name}: <b>${point.value:,.1f} m</b>' )
            ) %>%
               hc_add_series(data =  rv_intelHS$tmp_un_comtrade_exporter_map ,
                             type = "mapbubble",
                             color  = hex_to_rgba("#97D700", 0.9),
                             minSize = 0,
                             name= paste0( "Export value"),
                             maxSize = 30,
                             tooltip = list(table = TRUE,
                                            sort = TRUE,
                                            headerFormat = '<span style="font-size:13px">{series.name}</span><br/>',
                                            pointFormat = '{point.name}: <b>${point.z:,.1f} m</b>')
               ) %>%
               hc_exporting(enabled = TRUE, formAttributes = list(target = "_blank")) %>%
               hc_legend( enabled=FALSE ) %>% 
               hc_mapNavigation(enabled = TRUE) 
         })
      
      ## !!!!! try UI insert: map of importer / exporters ----------- 
      output$H3_title_un_comtrade_map <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0("Global importers and exporters at a glance")
         })
      
      output$H3_title_un_comtrade_map_note <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0( "The size of bubble area and color both represent the value of imports or exports" ) 
         })
      
      output$H4_title_un_comtrade_importer_map <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0("Global IMPORT markets")
         })
      
      output$H4_title_un_comtrade_exporter_map <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports'| input$select_comodity_for_market_analysis == "" )
               return(NULL)
            paste0("Global EXPORT markets")
         })
      
      ## Insert ui here
      insertUI(
         selector = '#ci_intel_by_hs_toadd_intl',
         ui =   div( id = 'ci_intel_by_hs_toadd_un_comtrade_map',
                     fluidRow(h3( HTML(paste0(textOutput("H3_title_un_comtrade_map"))) ) ,
                              p( HTML(paste0(textOutput("H3_title_un_comtrade_map_note"))) ),
                              column(6, div(id = "ci_intel_by_hs_un_comtrade_map_import", h4( HTML(paste0(textOutput("H4_title_un_comtrade_importer_map"))) ), highchartOutput('UN_comtrade_importer_Map') ) ),
                              column(6, div(id = "ci_intel_by_hs_un_comtrade_map_export", h4( HTML(paste0(textOutput("H4_title_un_comtrade_exporter_map"))) ), highchartOutput('UN_comtrade_exporter_Map') ) )
                              )
         )
      )
      ## end Try UI insert --------##
      
      
      
      ## 4.13.1 Sankey plot for a commodity ---------------
      print("--------- Building Sankey data -------------")

      observe({
         if(  input$rbtn_intel_by_hs == 'Exports' & !is.null(rv_intelHS$tmp_hs) ){
            ## check if able to get sankey data
            rv_intelHS$Fail_sankey_data <-
               try(
                  rv_intelHS$sankey_plot_data <-
                     get_data_sankey_uncomtrade( cc = as.character(rv_intelHS$tmp_hs), max_year = tmp_un_comtrade_max_year, eu_internal = "No" )
               )

            if( class(rv_intelHS$Fail_sankey_data) == 'try-error' )
               print("--------- FAIL: building Sankey data !!! -------------")
         }
      })
      
      print("--------- Building Sankey plots -------------")
      output$Sankey_trade_intelHS <-
         renderSankeyNetwork({
            if(  is.null(rv_intelHS$tmp_global_by_country_raw) |
                 length( as.character(rv_intelHS$tmp_hs) )>1 |
                 class(rv_intelHS$Fail_sankey_data) == 'try-error'|
                 input$rbtn_intel_by_hs == 'Imports'){
               return(NULL)
            }else{
               print("--------- Plotting Sankey plots -------------")
               sankey_uncomtrade( cc = as.character(rv_intelHS$tmp_hs), max_year = tmp_un_comtrade_max_year,eu_internal = as.character(input$btn_eu_internal_intelHS)  )
            }
         })
      
      ## !!!!! try UI insert: Sankey plot ----------- 
      output$H3_title_sankey_intelHS <-
         renderText({
            if( input$rbtn_intel_by_hs == 'Imports' ){
               return(NULL)
            }else{
               if( class(rv_intelHS$Fail_sankey_data) == 'try-error' & 
                   input$select_comodity_for_market_analysis != "" ){
                  paste0("Unable to perform global trade flow analyasis due to data query limits. Please wait for a hour.")
               }
               
               if( class(rv_intelHS$Fail_sankey_data) == 'try-error' & 
                   input$select_comodity_for_market_analysis == "" ){
                  return(NULL)
               }
               
               if( class(rv_intelHS$Fail_sankey_data) != 'try-error' &
                   input$select_comodity_for_market_analysis != "" ){
                  paste0( "Global trade flow analysis" )
               }
            }
            
            # if( input$rbtn_intel_by_hs == 'EXports' ){
            #    if( class(rv_intelHS$Fail_sankey_data) == 'try-error' ){
            #       paste0("Unable to perform global trade flow analyasis due to data query limits. Please wait for a hour.")
            #    }else{
            #       #if( is.null(rv_intelHS$tmp_global_by_country_raw) |
            #        #    length( as.character(rv_intelHS$tmp_hs) )>1 )
            #         # {return(NULL)}else{
            #          paste0( "Global trade flow analysis" )
            #          #}
            #       
            #    }
            #    
            #    # if( class(rv_intelHS$Fail_sankey_data) != 'try-error' &
            #    #     is.null(rv_intelHS$tmp_global_by_country_raw) &
            #    #     length ( as.character(rv_intelHS$tmp_hs) )>1 & 
            #    #     input$select_comodity_for_market_analysis == "" ) {
            #    #    return(NULL)
            #    # }
            #    # 
            #    # if( class(rv_intelHS$Fail_sankey_data) != 'try-error' & 
            #    #     !is.null(rv_intelHS$tmp_global_by_country_raw) &
            #    #     length ( as.character(rv_intelHS$tmp_hs) ) == 1 &
            #    #     input$select_comodity_for_market_analysis != ""
            #    #     ){
            #    #    paste0( "Global trade flow analysis" )
            #    #}
            #    
            #    # if( class(rv_intelHS$Fail_sankey_data) != 'try-error' &
            #    #     (is.null(rv_intelHS$tmp_global_by_country_raw) |
            #    #      length( as.character(rv_intelHS$tmp_hs) )>1 )
            #    #     )
            #    #    return(NULL)
            #    # paste0( "Global trade flow analysis" )
            # }
         })

      output$H3_title_sankey_note_intelHS <-
         renderUI({
            if( is.null(rv_intelHS$tmp_global_by_country_raw) |
                length( as.character(rv_intelHS$tmp_hs) )>1 |
                class(rv_intelHS$Fail_sankey_data) == 'try-error' |
                input$rbtn_intel_by_hs == 'Imports' )
               return(NULL)
            tags$p("This sankey plot shows trade flows of the selected commodity from expoters to importers. The displayed markets coverage is equal to or greater than 90% of global exports. The displayed trade flows are equal to or greater than 0.5% of global exports. Different colors are used to distinguish",
                   tags$span( "EXPORTERS", style = "color: #97D700; font-weight: bold" ),
                   ", ",
                   tags$span( "IMPORTERS", style = "color: #CD5B45; font-weight: bold"),
                   ", and ",
                   tags$span( "BOTH", style = "color: #FBE122; font-weight: bold"), "." )

         })

      ## button to choose show/hide EU internal trade
      output$Btn_EU_Internal_intelHS <-
         renderUI({
            if( is.null(rv_intelHS$tmp_global_by_country_raw) |
                length(  as.character(rv_intelHS$tmp_hs) )>1 |
                class(rv_intelHS$Fail_sankey_data) == 'try-error'|
                input$rbtn_intel_by_hs == 'Imports'  )
               return(NULL)
            radioButtons("btn_eu_internal_intelHS",
                         p("Display EU internal trade: " ),
                         choiceNames = list(icon("check"), icon("times")),
                         choiceValues = list( "Yes" , "No"),
                         #c( "Yes" = "Yes", "No" = "No"),
                         inline=T,
                         selected="No")
         })

      output$Btn_EU_Internal_note_intelHS <-
         renderUI({
            if( is.null(rv_intelHS$tmp_global_by_country_raw) |
                length( as.character(rv_intelHS$tmp_hs) )>1 |
                class(rv_intelHS$Fail_sankey_data) == 'try-error'|
                input$rbtn_intel_by_hs == 'Imports' )
               return(NULL)
            tags$p( "You may choose to show or hide EU internal trade in the sankey plot by using the buttons below." )
         })
      
      ## Insert ui here
      insertUI(
         selector = '#ci_intel_by_hs_toadd_intl',
         ui =   div( id = 'ci_intel_by_hs_toadd_un_comtrade_sankey',
                     fluidRow(h3( HTML(paste0(textOutput("H3_title_sankey_intelHS"))) ) ,
                              #p( HTML(paste0(textOutput("H2_title_sankey_note"))) ),
                              uiOutput("H3_title_sankey_note_intelHS"),
                              uiOutput("Btn_EU_Internal_note_intelHS"),
                              uiOutput("Btn_EU_Internal_intelHS"),
                              sankeyNetworkOutput( "Sankey_trade_intelHS" )
                     )
         )
      )
      ## end Try UI insert --------##
      
      ## 4.14 Generating summary tables for both importers and exporters -------
      # container of the table -- importers 
      print("--------- Building importer and exporter tables -------------")
      sketch_uncomtrade_im<-  htmltools::withTags(table(
         class = 'display',
         thead(
            tr(
               th(rowspan = 2, 'Market'),
               th(rowspan = 2, 'Import share'),
               th(colspan = 3, 'Import value'),
               th(colspan = 2, 'Import price')
            ),
            tr( #th('Country'),
               lapply(rep(c('Value ($m)', 'CAGR5', 'ABS5'), 1), th, align = 'center'),
               lapply(rep(c('$/kg (unit)', 'CAGR5' ), 1), th, align = 'center')
            )
         )
      ))
      
      ## table for importers
      output$UN_com_trade_importer_summary <-
         renderDataTable({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            datatable( rv_intelHS$tmp_un_comtrade_import_summary_tab,
                      container = sketch_uncomtrade_im,
                      rownames = FALSE,
                      extensions = 'Buttons',
                      options = list(dom = 'Bltp', 
                                     scrollX = TRUE,
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                     pageLength = 10,
                                     lengthMenu = list(c(10, 30 , -1), list('10','30' ,'All')),
                                     columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(rv_intelHS$tmp_un_comtrade_import_summary_tab)-1) ) )
                      )
            ) %>%
               formatPercentage( c('Share', 'Value_per_change', 'Price_per_change' ) , digit = 1 ) %>%
               formatCurrency( columns = c('Trade.Value..US..','Value_abs_change'), digits = 0 ) %>%
               formatCurrency( columns = c('Price'), digits = 2 ) %>%
               formatStyle(
                  c('Value_per_change' ),
                  background = styleColorBar( c(0,max(rv_intelHS$tmp_un_comtrade_import_summary_tab[1:min(20,nrow(rv_intelHS$tmp_un_comtrade_import_summary_tab)),c('Value_per_change' )],na.rm=T)*2) ,
                                              'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
               ) %>%
               formatStyle(
                  c('Price_per_change' ),
                  background = styleColorBar( c(0,max(rv_intelHS$tmp_un_comtrade_import_summary_tab[1:min(20,nrow(rv_intelHS$tmp_un_comtrade_import_summary_tab)),c('Price_per_change' )],na.rm=T)*2) ,
                                              'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
               ) %>%
               formatStyle(
                  c('Value_abs_change' ),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
               ) %>%
               formatStyle( 1:ncol(rv_intelHS$tmp_un_comtrade_import_summary_tab), 'vertical-align'='center', 'text-align' = 'center' )
         })
      
      ### build export table
      # container of the table -- importers 
      sketch_uncomtrade_ex <-  htmltools::withTags(table(
         class = 'display',
         thead(
            tr(
               th(rowspan = 2, 'Market'),
               th(rowspan = 2, 'Export share'),
               th(colspan = 3, 'Export value'),
               th(colspan = 2, 'Export price')
            ),
            tr( #th('Country'),
               lapply(rep(c('Value ($m)', 'CAGR5', 'ABS5'), 1), th, align = 'center'),
               lapply(rep(c('$/kg (unit)', 'CAGR5' ), 1), th, align = 'center')
            )
         )
      ))
      
      ## table for importers
      output$UN_com_trade_exporter_summary <-
         renderDataTable({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            datatable( rv_intelHS$tmp_un_comtrade_export_summary_tab,
                       container = sketch_uncomtrade_ex,
                       rownames = FALSE,
                       extensions = 'Buttons',
                       options = list(dom = 'Bltp', 
                                      scrollX = TRUE,
                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                      pageLength = 10,
                                      lengthMenu = list(c(10, 30, -1), list('10', '30' ,'All')),
                                      columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(rv_intelHS$tmp_un_comtrade_export_summary_tab)-1) ) )
                       )
            ) %>%
               formatPercentage( c('Share', 'Value_per_change', 'Price_per_change' ) , digit = 1 ) %>%
               formatCurrency( columns = c('Trade.Value..US..','Value_abs_change'), digits = 0 ) %>%
               formatCurrency( columns = c('Price'), digits = 2 ) %>%
               formatStyle(
                  c('Value_per_change' ),
                  background = styleColorBar( c(0,max(rv_intelHS$tmp_un_comtrade_export_summary_tab[1:min(20,nrow(rv_intelHS$tmp_un_comtrade_export_summary_tab)),c('Value_per_change' )],na.rm=T)*2) ,
                                              'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
               ) %>%
               formatStyle(
                  c('Price_per_change' ),
                  background = styleColorBar( c(0,max(rv_intelHS$tmp_un_comtrade_export_summary_tab[1:min(20,nrow(rv_intelHS$tmp_un_comtrade_export_summary_tab)),c('Price_per_change' )],na.rm=T)*2) ,
                                              'lightblue'),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
               ) %>%
               formatStyle(
                  c('Value_abs_change' ),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")
               ) %>%
               formatStyle( 1:ncol(rv_intelHS$tmp_un_comtrade_export_summary_tab), 'vertical-align'='center', 'text-align' = 'center' )
         })
      
      ## Insert ui here: summary tables  ----------------
      output$H3_title_un_comtrade_summary_tab <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0("Summary tables for importers and exporters")
         })
      
      
      output$H4_title_un_comtrade_importer_sum_tab <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0("Global IMPORT markets")
         })
      
      output$H4_title_un_comtrade_exporter_sum_tab <-
         renderText({
            if( is.null(input$HSCodeTable_rows_selected) | is.null(rv_intelHS$tmp_global_by_country_raw) | input$rbtn_intel_by_hs == 'Imports' | input$select_comodity_for_market_analysis == "")
               return(NULL)
            paste0("Global EXPORT markets")
         })
      
      insertUI(
         selector = '#ci_intel_by_hs_toadd_intl',
         ui =   div( id = 'ci_intel_by_hs_toadd_un_comtrade_summary_tab',
                     fluidRow(h3( HTML(paste0(textOutput("H3_title_un_comtrade_summary_tab"))) ) ,
                              #p( HTML(paste0(textOutput("H3_title_un_comtrade_map_note"))) ),
                              column(6, div(id = "ci_intel_by_hs_un_comtrade_import_summary_tab", h4( HTML(paste0(textOutput("H4_title_un_comtrade_importer_sum_tab"))) ), dataTableOutput('UN_com_trade_importer_summary') ) ),
                              column(6, div(id = "ci_intel_by_hs_un_comtrade_export_summary_tab", h4( HTML(paste0(textOutput("H4_title_un_comtrade_exporter_sum_tab"))) ), dataTableOutput('UN_com_trade_exporter_summary') ) )
                     )
         )
      )
      ## end Try UI insert --------##
      
      
      
      ## 4.14.1 Get the leftover quota and reset time ---------
      output$Un_comtrade_msg_intelHS <-
         renderUI({
            if(  is.null(input$HSCodeTable_rows_selected) | 
                 input$select_comodity_for_market_analysis == "" | 
                 input$rbtn_intel_by_hs == 'Imports'){
               return(NULL)
            }else{
               tags$div(
                  tags$hr(),
                  tags$p(paste0( "Note: ",ct_get_remaining_hourly_queries(), 
                                 " number of queries are left for the global analysis section from the UN Comtrade. The reset time will be at ", 
                                 ct_get_reset_time() ,
                                 ", while the current time is ", format(Sys.time()) , "."
                             ))
               )
            }
         })
      
      insertUI( selector = '#ci_intel_by_hs_toadd_intl',
                ui = div( id = 'ci_intel_by_hs_toadd_un_comtrade_msg',
                          fluidRow( uiOutput("Un_comtrade_msg_intelHS") ) )
              )
      
      ## 4.15 Hide generating report message ----------
      observe({
         try(
            #if( !is.null( rv_intelHS$tmp_dtf_market_map ) ){
            if( !is.null(rv_intelHS$tmp_tab) | 
                !is.null(input$select_comodity_for_market_analysis) |
                !is.null(input$HSCodeTable_rows_selected) ){
               shinyjs::hide( id = "ci_intel_hs_loading_message" )
            }
         )
      })
      
      observe({
         try(
            if( !is.null( rv_intelHS$tmp_dtf_market_map ) ){
               #if( !is.null(rv_intelHS$tmp_tab) ){
               shinyjs::hide( id = "ci_intel_hs_loading_message_intl" )
            }
         )
      })
      
      
      ## IV. Appendix .......  Reset buttions -------------------------
      ## 1. reset btn for Commodity intelligence Exports --------------------
      observeEvent( input$btn_reset_ci_ex,
                    {
                       ### try remove UI -- when pre-defined HS
                       removeUI( selector = '#body_ex_line_value_percent')
                       removeUI( selector = '#body_ex_growth_tab')
                       removeUI( selector = '#body_ci_markets_ex_selector')
                       removeUI( selector = '#body_ci_markets_ex_map')
                       removeUI( selector = '#body_ci_markets_ex_top')
                       removeUI( selector = '#body_ci_markets_ex_growth')
                       removeUI( selector = '#body_appendix_hs_ex')
                       removeUI( selector = '#body_ci_markets_ex_fail_msg')
                       removeUI( selector = '#body_ci_markets_ex_global_facts')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_map')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_sankey')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_summary_tab')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_msg')
                       shinyjs::hide( id = "body_ci_market_loading_message" )
                       
                       ### remove UI -- when self_defined HS
                       removeUI( selector = '#body_ex_line_value_percent_self_defined')
                       removeUI( selector = '#body_ex_growth_tab_self_defined')
                       removeUI( selector = '#body_ci_markets_ex_selector_self_defined')
                       removeUI( selector = '#body_selected_ex_line_value_percent_self_defined')
                       removeUI( selector = '#body_ci_markets_ex_map_self_defined')
                       removeUI( selector = '#body_ci_markets_ex_top_self_defined')
                       removeUI( selector = '#body_ci_markets_ex_growth_self_defined')
                       removeUI( selector = '#body_appendix_hs_ex_self_defined')
                       removeUI( selector = '#body_ci_markets_ex_fail_msg_self_define')
                       removeUI( selector = '#body_ci_markets_ex_global_facts_self_define')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_map_self_define')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_sankey_self_define')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_summary_tab_self_define')
                       removeUI( selector = '#body_ci_markets_ex_un_comtrade_msg_self_define')
                       shinyjs::hide( id = "body_ci_market_loading_message_self_define" )
                       
                       ### clear all outputs
                       output$HS_ex <- renderDataTable(NULL)
                       output$HS_pre_ex <- renderDataTable(NULL)
                       output$CIExportValueLine <- renderHighchart(highchart())
                       output$CIExportPercentLine <- renderHighchart(highchart())
                       output$MapEXMarket <- renderHighchart(highchart())
                       output$GrowthTabSelectedEx <- renderDataTable(NULL)
                       output$SelectedExMarketLine <- renderHighchart(highchart())
                       output$SelectedExMarketLinePercent <- renderHighchart(highchart())
                       output$SelectedExMarketGrowthTab <- renderDataTable(NULL)
                       
                       ## hide all ids
                       #shinyjs::hide(selector = '#body_ex')
                       #shinyjs::hide(selector = '#body_value_ex')
                       #shinyjs::hide(selector = '#body_percent_ex')
                       #shinyjs::hide(selector = '#body_growth_ex')
                       #shinyjs::hide(selector = '#body_ci_markets_ex')
                       #shinyjs::hide(selector = '#body_appendix_hs_ex')
                       shinyjs::show(id = 'ci_howto_ex')
                       shinyjs::reset('sidebar_ci_exports')
                       ## disable the buttone ---
                       shinyjs::enable("btn_build_commodity_report_ex")
                       shinyjs::enable("select_comodity_ex")
                       shinyjs::enable("file_comodity_ex")
                       shinyjs::enable("rbtn_prebuilt_diy_ex")
                       
                     }
                    )
      
      ## 2. reset btn for Commodity intelligence Imports --------
      observeEvent( input$btn_reset_ci_im,
                    {
                       ### try remove UI -- when pre-defined HS
                       removeUI( selector = '#body_im_line_value_percent')
                       removeUI( selector = '#body_im_growth_tab')
                       removeUI( selector = '#body_ci_markets_im_selector')
                       removeUI( selector = '#body_ci_markets_im_map')
                       removeUI( selector = '#body_ci_markets_im_top')
                       removeUI( selector = '#body_ci_markets_im_growth')
                       removeUI( selector = '#body_appendix_hs_im')
                       
                       ### remove UI -- when self_defined HS
                       removeUI( selector = '#body_im_line_value_percent_self_defined')
                       removeUI( selector = '#body_im_growth_tab_self_defined')
                       removeUI( selector = '#body_ci_markets_im_selector_self_defined')
                       removeUI( selector = '#body_selected_im_line_value_percent_self_defined')
                       removeUI( selector = '#body_ci_markets_im_map_self_defined')
                       removeUI( selector = '#body_ci_markets_im_top_self_defined')
                       removeUI( selector = '#body_ci_markets_im_growth_self_defined')
                       removeUI( selector = '#body_appendix_hs_im_self_defined')
                       
                       ### clear all outputs
                       output$HS_im <- renderDataTable(NULL)
                       output$HS_pre_im <- renderDataTable(NULL)
                       output$CIImportValueLine <- renderHighchart(highchart())
                       output$CIImportPercentLine <- renderHighchart(highchart())
                       output$MapIMMarket <- renderHighchart(highchart())
                       output$GrowthTabSelectedIm <- renderDataTable(NULL)
                       output$SelectedImMarketLine <- renderHighchart(highchart())
                       output$SelectedImMarketLinePercent <- renderHighchart(highchart())
                       output$SelectedImMarketGrowthTab <- renderDataTable(NULL)
                       
                       ## hide all ids
                       # shinyjs::hide(selector = '#body_im')
                       # shinyjs::hide(selector = '#body_value_im')
                       # shinyjs::hide(selector = '#body_percent_im')
                       # shinyjs::hide(selector = '#body_growth_im')
                       # shinyjs::hide(selector = '#body_ci_markets_im')
                       # shinyjs::hide(selector = '#body_appendix_hs_im')
                       shinyjs::show(id = 'ci_howto_im')
                       shinyjs::reset('sidebar_ci_imports') 
                       ## enable the buttone ---
                       shinyjs::enable("btn_build_commodity_report_im")
                       shinyjs::enable("select_comodity_im")
                       shinyjs::enable("file_comodity_im")
                       shinyjs::enable("rbtn_prebuilt_diy_im")
                       
                     }
                    )
      
      ## 3. reset btn for Country intelligence ------------
      observeEvent( input$btn_reset_cr,
                    {
                       ## remove UIs 
                       removeUI( selector = "#country_name_single_or_multiple" )
                       removeUI( selector = "#country_info_table_map" )
                       removeUI( selector = "#country_trade_summary_all_items" )
                       removeUI( selector = "#country_trade_summary_appendix" )
                       
                       ## clear all output
                       output$CountryTable <- renderDataTable(NULL)
                       output$MapSelectedCountry <- renderHighchart(highchart())
                       output$CountryTradeTableTotal <- renderDataTable(NULL)
                       output$CountryTwowayTradeGraphTotal <- renderHighchart(highchart())
                       output$CountryTradeBalanceGraphTotal <- renderHighchart(highchart())
                       output$CountryExportsGraphTotal <- renderHighchart(highchart())
                       output$CountryExportsGraphTotalPercent <- renderHighchart(highchart())
                       output$CountryImportsGraphTotal <- renderHighchart(highchart())
                       output$CountryImportsGraphTotalPercent <- renderHighchart(highchart())
                       output$KeyExCountryTotalTreeMap <- renderHighchart(highchart())
                       output$KeyImCountryTotalTreeMap <- renderHighchart(highchart())
                       output$KeyExCountryTotalLine <- renderHighchart(highchart())
                       output$KeyExCountryTotalLinePercent <- renderHighchart(highchart())
                       output$KeyImCountryTotalLine <- renderHighchart(highchart())
                       output$CountrySummaryAllExports <- renderDataTable(NULL)
                       output$CountrySummaryAllImports <- renderDataTable(NULL)
                       output$CountrySummaryAllTwowayBalance  <- renderDataTable(NULL)

                       
                       output$SelectedMarketSingle <- renderText(NULL)
                       output$SelectedMarketMultiple <-renderText(NULL)
                          
                       ## hide all ids
                       shinyjs::show(id = 'country_howto')
                       #shinyjs::hide(id = 'country_basic_info')
                       #shinyjs::hide(id = 'country_trade_summary')
                       #shinyjs::hide(id = 'country_trade_summary_individual')
                       #shinyjs::hide(id = "country_trade_summary_appendix")
                       #shinyjs::hide(id = "country_single_name")
                       #shinyjs::hide(id = "country_multiple_name")
                       # shinyjs::hide(id = 'country_trade_single')
                       reset('sidebar_cr')
                       ## ensable a button
                       shinyjs::enable("btn_build_country_report")
                       shinyjs::enable("select_country")
                    }
                  )
      
      # withProgress(message = 'Finishing in about 10s', value = 1, {
      #    # Increment the progress bar, and update the detail text.
      #    incProgress( 1, detail = NULL)
      #    Sys.sleep(3)
      #    
      # })
      
      ## 4. Monthly update ------------------------
      output$MonthlyUpdate <- 
         renderUI({
            tags$iframe(
               src = SNZ_link,
               seamless = "seamless",
               frameborder = 0,
               height="800", width="100%")
         })
   }


















