#### vb style -----------------
VB_style <- function(msg = 'Hello', style="font-size: 100%;"){
   tags$p( msg , style = style )
}

### CAGR function------------------
CAGR <- 
   function (ratio, period, digits = 1) {
      round((exp(log(ratio)/period) - 1) * 100, digits)
      }
      
## rgb to hex function---------------      
GetColorHexAndDecimal <- function(color)
{
  c <- col2rgb(color)
  sprintf("#%02X%02X%02X %3d %3d %3d", c[1],c[2],c[3], c[1], c[2], c[3])
}

## how to text for commodity intelligence report --------------
howto_ci <- function(){
   fluidRow(
      tags$h1('How to:'),
      tags$ol(
         tags$li( "Select pre-defined or self-defined commodities:",
                  tags$ol(type="a",
                          tags$li(tags$b("Pre-defined commodities "), "are defined by StatsNZ"),
                          tags$li(tags$b("Self-defined "), 
                                  "commodity groupings are created by users with the format of a .csv file with the first column containing any of level 2/4/6 HS codes, and the second column containing group names.",
                                  tags$b("You can download such a template from ", tags$a(href="HS_group_template.csv", "here", target = "_blank"), ", and modify it to suit your need." )
                                  )
                  )
         ),
         tags$li("Browse or upload:",
                 tags$ol(type='a',
                         tags$li( "Search and select one or multiple pre-defined commodities" ),
                         tags$li( "Upload your self-defined commodity groups with the correct format" )
                 )
         ),
         tags$li(tags$b("Please click RESET before generating an other report!")),
         tags$li( "For more detailed explanations on how to use this part of the dasbhoard, please visit " ,
                  tags$a( "here",
                          href= "https://nzprimarysectortrade.wordpress.com/2018/10/15/introducing-the-new-zealand-trade-intelligence-dashboard/",
                          target = "_blank"),
                  "."
                  )
      )
   )
}

## how to text for country intelligence report --------------
howto_country <- function(){
   fluidRow(
      tags$h1('How to:'),
      tags$ol(
         tags$li( "Search or select a market or multiple markets"),
         tags$li( "Search or select ONLY ONE of the market groups. We prebuilt several groups including ", 
                  tags$a(href="https://en.wikipedia.org/wiki/Africa","Africa" , target = "_blank"), ", ", 
                  tags$a(href="https://www.apec.org/","APEC" , target = "_blank"), ", ",  
                  tags$a(href="https://www.uneca.org/oria/pages/amu-arab-maghreb-union","Arab Maghreb Union" , target = "_blank"), ", ",  
                  tags$a(href="http://asean.org/","ASEAN" , target = "_blank"), ", ",  
                  tags$a(href="http://china-trade-research.hktdc.com/business-news/article/The-Belt-and-Road-Initiative/The-Belt-and-Road-Initiative-Country-Profiles/obor/en/1/1X000000/1X0A36I0.htm","BRI (Belt and Road Initiative) countries" , target = "_blank"), ", ",  
                  tags$a(href="https://en.wikipedia.org/wiki/Central_Africa","Central Africa" , target = "_blank"), ", ",  
                  tags$a(href="https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-concluded-but-not-in-force/cptpp/","CPTPP" , target = "_blank"), ", ",  
                  tags$a(href="https://en.wikipedia.org/wiki/East_Africa","Eastern Africa" , target = "_blank"), ", ",  
                  tags$a(href="https://www.eac.int/","Eastern African Community" , target = "_blank"), ", ",  
                  tags$a(href="http://www.ecowas.int/","Economic Community of West African States" , target = "_blank"), ", ",  
                  tags$a(href="https://europa.eu/european-union/about-eu/countries_en","EU28" , target = "_blank"), ", ",  
                  tags$a(href="https://www.gcsb.govt.nz/about-us/ukusa-allies/","Five Eyes" , target = "_blank"), ", ",  
                  tags$a(href="https://www.mfat.govt.nz/en/trade/free-trade-agreements/free-trade-agreements-in-force/","FTA in force" , target = "_blank"), ", ",  
                  tags$a(href="https://en.wikipedia.org/wiki/Group_of_Seven","G7" , target = "_blank"), ", ",  
                  tags$a(href="http://www.gcc-sg.org/en-us/Pages/default.aspx","GCC" , target = "_blank"), ", ",  
                  tags$a(href="https://en.wikipedia.org/wiki/Latin_America","Latin America" , target = "_blank"), ", ",  
                  tags$a(href="https://en.wikipedia.org/wiki/Middle_East","Middle East" , target = "_blank"), ", ",  
                  tags$a(href="https://en.wikipedia.org/wiki/North_Africa","Northern Africa" , target = "_blank"), ", ", 
                  tags$a(href="http://www.oecd.org/", "OECD" , target = "_blank"), ", ", 
                  tags$a(href="http://www.opec.org/", "OPEC" , target = "_blank"), ", ", 
                  tags$a(href="https://www.forumsec.org/", "Pacific Islands Forum" , target = "_blank"), ", ", 
                  tags$a(href="https://en.wikipedia.org/wiki/Southern_Africa", "Southern Africa" , target = "_blank"), ", ", 
                  tags$a(href="https://www.sadc.int/", "Southern African Development Community" , target = "_blank"), ", and ", 
                  tags$a(href="https://en.wikipedia.org/wiki/West_Africa", "Western Africa" , target = "_blank"), ". ", 
                  "Please contact ",
                  tags$a( href = "mailto:TR_SharedMailbox@mbie.govt.nz", "TR_SharedMailbox@mbie.govt.nz"), ', if you need extra groups built.' ),
         tags$li(tags$b("Please click RESET before generating an other report!")),
         tags$li( "For more detailed explanations on how to use this part of the dasbhoard, please visit " ,
                  tags$a( "here",
                          href= "https://nzprimarysectortrade.wordpress.com/2018/10/15/introducing-the-new-zealand-trade-intelligence-dashboard/",
                          target = "_blank"),
                  "."
         )
      )
   )
}

## how to text for hs finder tab ----------------
howto_hs_finder <- function(){
   #fluidRow(
      tags$h1('How to:')
      tags$ol(
         tags$li( "Search for either HS code or commodity names in the search box or the filter box above each column. Note that regular expression rules are built in all search boxes."),
         tags$li( "Show either the first 10 entries or the entire table."),
         tags$li( "Click one or multiple rows to generate a report."),
         tags$li( "The report is default to be based on exports but can be changed to imports by using the radio button on the left." ),
         tags$li( "For more detailed explanations on how to use this part of the dasbhoard, please visit " ,
                  tags$a( "here",
                          href= "https://nzprimarysectortrade.wordpress.com/2018/10/15/introducing-the-new-zealand-trade-intelligence-dashboard/",
                          target = "_blank"),
                  "."
         )
         #tags$li( "Copy to clipboard or export as a CSV file for later use")
      )
   #)
}

## contact for help ----------------
contact <- function(){
   fluidRow(
      h2( "Contact" ),
      tags$p("Your suggestions, feedback, complaints or compliments are highly valued and will guide us to improve the dashboard continuously. Please email them to ", 
             tags$a( href="mailto:TR_SharedMailbox@mbie.govt.nz",
                     "TR_SharedMailbox@mbie.govt.nz",
                     target = '_blank'),
             "."
      )
   )
}

## data_source of the report ---------------------
data_source <- function(){
   fluidRow(
      h2(paste0('What are the data sources?')),
      #tags$ol(
         tags$li( "Total goods and services exports and imports are sourced from ", 
                  tags$a(tags$i("BPM6 Quarterly, Balance of payments major components (Qrtly-Mar/Jun/Sep/Dec)"),
                         href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=aa7f4009-2651-404c-b3b6-e24ea781d803",
                         target = "_blank"), 
                  ", a table under Economic indicators and Balance of Payments - BOP from Inforshare Statistics New Zealand."
         ),
         tags$li("Goods exports and imports by country and commodity are sourced and compiled from ",
                 tags$a("the overseas merchiandise trade datasets",
                        href = "http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS10-by-country.aspx",
                        target = "_blank"),
                 " from Statistics New Zealand."
         ),
         tags$li("Services exports and imports by country are sourced from ",
                 tags$a(tags$i("BPM6 Services by country, year ended in quarter (Qrtly-Mar/Jun/Sep/Dec)") ,
                        href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                        target = "_blank"),
                 ", a table under Economic indicators and Balance of Payments - BOP from Inforshare Statistics New Zealand. For countries whose data are not available from this source, ",
                 tags$a( tags$i("Goods and services trade by country: Year ended Qtr Year map data CSV"),
                         href = "https://www.stats.govt.nz/information-releases/goods-and-services-trade-by-country-year-ended-june-2018",
                         target = "_blank" ),
                 "is then used."
                 ),
      tags$li("Data used in the global trade analysis are sourced from ",
              tags$a(tags$i("UN Comtrade, International Trade Statistics Database") ,
                     href = "https://comtrade.un.org/",
                     target = "_blank"),
              ", by using its ", 
              tags$a(tags$i("API"),
                     href = "https://comtrade.un.org/data/dev/portal",
                     target = "_blank"),
              " via an R package called ",
              tags$a(tags$i("comtradr"),
                     href = "https://cran.r-project.org/web/packages/comtradr/index.html",
                     target = "_blank"),
              ". Please note that the maximum number of queries is 100 per hour."
              ),
      tags$li("Directional basis stock of direct investment are sourced from ",
              tags$a(tags$i("BPM6 Annual, Directional basis stock of direct investment by country (Annual-Mar)") ,
                     href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                     target = "_blank"),
              ", a table under Economic indicators and International Investment Position - IIP from Inforshare Statistics New Zealand."),
      tags$li("New Zealand visitor travelling overseas data is sourced from ",
              tags$a(tags$i("NZ-resident traveller departures by EVERY country of main dest and purpose (Qrtly-Mar/Jun/Sep/Dec)") ,
                     href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                     target = "_blank"),
              ", a table under Tourism and International Travel and Migration - ITM from Inforshare Statistics New Zealand."),
      tags$li("Foreign visitor travelling to New Zealand data is sourced from ",
              tags$a(tags$i("Visitor arrivals by EVERY country of residence and purpose (Qrtly-Mar/Jun/Sep/Dec)") ,
                     href = "http://archive.stats.govt.nz/infoshare/SelectVariables.aspx?pxID=e17bdb70-8fff-4f11-baf1-eb732a963099",
                     target = "_blank"),
              ", a table under Tourism and International Travel and Migration - ITM from Inforshare Statistics New Zealand.")
      #)
   )
}


## when the dashobard will be updated ---------------------
when_update <- function() {
   fluidRow(
      h2(paste0('When will the dashboard be updated?')),
      tags$p(
         "The dashboard will be updated quarterly with around two and half months lag. For example, March quarter data will be updated around the second week in June."
      )
   )
}

## hs_code_explain of the report ---------------------
hs_code_explain <- function() {
   fluidRow(
      h2(paste0('What is HS code?')),
      tags$p(
         "The Harmonized Commodity Description and Coding System, also known as the Harmonized System (HS) of tariff nomenclature is an internationally standardized system of names and numbers to classify traded products. It came into effect in 1988 and has since been developed and maintained by the World Customs Organization (WCO) (formerly the Customs Co-operation Council), an independent intergovernmental organization based in Brussels, Belgium, with over 200 member countries."
         ),
      tags$p("More information on New Zealand harmonised system classification can be found ", 
             tags$a( href="http://archive.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS2017.aspx",
                     "here.",
                     target = '_blank') 
             ),
      tags$p("In addition, the Customs Service are responsible for the classification of goods at the border. More information can be found ", 
             tags$a( href="https://www.customs.govt.nz/business/tariffs/tariff-classifications-and-rates/",
                     "here.",
                     target = '_blank') )
   )
}

### trade terms explained-----------------
trade_terms <- function(){
   fluidRow(
      h2(paste0('What do these trade terms mean?')),
      #tags$ol(
         tags$li( tags$b("Trade balance, trade surplus or trade deficit:"), tags$br(),
                  "The balance of trade, commercial balance, or net exports (sometimes symbolized as NX), is the difference between the monetary value of a nation's exports and imports over a certain period. Sometimes a distinction is made between a balance of trade for goods versus one for services. If a country exports a greater value than it imports, it has a trade surplus, if a country imports a greater value than it exports, it has a trade deficit."
         ),
         tags$li( tags$b("Two-way trade:"), tags$br(),
                 "The sum of total exports and imports."
         )
      #)
   )
}

### confidential trade data --------------------
confidential_trade_data <- function(){
   fluidRow(
      h2(paste0('What is confidential data?')),
      tags$p(
         "International Merchandise Trade Statistics confidentiality policy can be found ",
         tags$a( href="http://archive.stats.govt.nz/about_us/legisln-policies-protocols/trade-confidentiality/international-merchandise-trade-confidentiality-policy.aspx",
                 "here. ",
                 target = '_blank') ,
         "Confidential items in overseas trade and cargo statistics can be found ",
          tags$a( href="http://archive.stats.govt.nz/about_us/legisln-policies-protocols/trade-confidentiality.aspx",
                 "here, ",
                 target = '_blank'),
         "where you also find the confidential items for both exports and imports."
      )
   )
}

### Urgent updates explained-----------------
urgent_updates <- function(){
   fluidRow(
      h2(paste0('Urgent updates:')),
      tags$li( "The dashboard with the latest data upto the year ended June 2018 is updated on 4 October 2018. This is due to Stats NZ's correction on its goods and services by country data for the year ended June 2018 on 3 October 2018. Stats NZ's media release on the correction can be found ",
              tags$a( href="https://www.stats.govt.nz/news/correction-to-goods-and-services-trade-by-country-year-ended-june-2018",
                      "here.",
                      target = '_blank'))
   )
}



# var conall = $(this.chart.container).parents(".shinyjs-hide).find("#CIExportPercentLine");
## shared legend for highchager ---------------------
sharelegend = JS('function(event){
                  var vis = this.visible;
                 var conall = $(this.chart.container).parents(".row").find("div.highchart");
                 for(var i = 0; i < conall.length; i++){
                    var hc = $(conall[i]).highcharts();
                    var series = hc.get(this.options.id);
                    if(series){
                       if(vis){
                          series.hide();
                        } else{
                          series.show();
                        }
                     }
                 }
                 return false;
                 }')

sharelegend = JS("function (event) {
                    var visibility = this.visible ? 'visible' : 'hidden';
                 if (!confirm('The series is currently ' +
                 visibility + '. Do you want to change that?')) {
                 return false;
                 }
                 }")


## produce summary country table -------------------
sum_selected_country <- function( arg_countries ){
   ## for selected countries
   tmp_country_all_base <- 
      dtf_shiny_country_gs %>%
      filter( Country %in% arg_countries,
              Year >= 2007 ) 
   
   tmp_country_all_twoway <-
      tmp_country_all_base %>%
      group_by( Year ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Two-way trade' ) 
   
   tmp_country_all_balance <- 
      tmp_country_all_base %>%
      group_by( Year, Type_ie ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      group_by( Year ) %>%
      #summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
      do( Value = .$Value[.$Type_ie=='Exports'] - .$Value[.$Type_ie=='Imports'] ) %>%
      ungroup %>%
      mutate( Value = as.numeric(Value) ) %>%
      mutate( Name = 'Trade balance' ) 
   
   tmp_country_all_balance_g <- 
      tmp_country_all_base %>%
      filter( Type_gs == 'Goods' ) %>%
      group_by( Year, Type_ie ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      group_by( Year ) %>%
      #summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
      do( Value = .$Value[.$Type_ie=='Exports'] - .$Value[.$Type_ie=='Imports'] ) %>%
      ungroup %>%
      mutate( Value = as.numeric(Value) ) %>%
      mutate( Name = 'Goods balance' ) 
   
   tmp_country_all_balance_s <- 
      tmp_country_all_base %>%
      filter( Type_gs == 'Services' ) %>%
      group_by( Year, Type_ie ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      group_by( Year ) %>%
      #summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
      do( Value = .$Value[.$Type_ie=='Exports'] - .$Value[.$Type_ie=='Imports'] ) %>%
      ungroup %>%
      mutate( Value = as.numeric(Value) ) %>%
      mutate( Name = 'Services balance' ) 
   
   tmp_country_all_tot_ex <- 
      tmp_country_all_base %>%
      filter( Type_ie == 'Exports' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Total exports' ) 
   
   tmp_country_all_gs_ex <- 
      tmp_country_all_base %>%
      filter( Type_ie == 'Exports' ) %>%
      group_by( Year, Type_gs) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = paste0(Type_gs,' exports') ) %>%
      dplyr::select( -Type_gs )
   
   tmp_country_all_tot_im <- 
      tmp_country_all_base %>%
      filter( Type_ie == 'Imports' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Total imports' ) 
   
   tmp_country_all_gs_im <- 
      tmp_country_all_base %>%
      filter( Type_ie == 'Imports' ) %>%
      group_by( Year, Type_gs) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = paste0(Type_gs,' imports') ) %>%
      dplyr::select( -Type_gs )
   
   tmp_dtf_country_all <-
      tmp_country_all_twoway %>%
      bind_rows(tmp_country_all_balance) %>%
      bind_rows(tmp_country_all_balance_g) %>%
      bind_rows(tmp_country_all_balance_s) %>%
      bind_rows( tmp_country_all_tot_ex ) %>%
      bind_rows( tmp_country_all_gs_ex ) %>%
      bind_rows( tmp_country_all_tot_im ) %>%
      bind_rows( tmp_country_all_gs_im ) %>%
      mutate( Country = 'Total selected countries' )
   
   ## for world
   tmp_world_base <- 
      dtf_shiny_country_gs %>%
      filter( Country %in% 'World',
              Year >= 2007 ) 
   
   tmp_world_twoway <-
      tmp_world_base %>%
      group_by( Year ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Two-way trade' ) 
   
   tmp_world_balance <- 
      tmp_world_base %>%
      group_by( Year, Type_ie ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      group_by( Year ) %>%
      summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
      ungroup %>%
      mutate( Name = 'Trade balance' ) 
   
   tmp_world_balance_g <- 
      tmp_world_base %>%
      filter( Type_gs == 'Goods') %>%
      group_by( Year, Type_ie ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      group_by( Year ) %>%
      summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
      ungroup %>%
      mutate( Name = 'Goods balance' ) 
   
   tmp_world_balance_s <- 
      tmp_world_base %>%
      filter( Type_gs == 'Services') %>%
      group_by( Year, Type_ie ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      group_by( Year ) %>%
      summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
      ungroup %>%
      mutate( Name = 'Services balance' ) 
   
   tmp_world_tot_ex <- 
      tmp_world_base %>%
      filter( Type_ie == 'Exports' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Total exports' ) 
   
   tmp_world_gs_ex <- 
      tmp_world_base %>%
      filter( Type_ie == 'Exports' ) %>%
      group_by( Year, Type_gs) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = paste0(Type_gs,' exports') ) %>%
      dplyr::select( -Type_gs )
   
   tmp_world_tot_im <- 
      tmp_world_base %>%
      filter( Type_ie == 'Imports' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Total imports' ) 
   
   tmp_world_gs_im <- 
      tmp_world_base %>%
      filter( Type_ie == 'Imports' ) %>%
      group_by( Year, Type_gs) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = paste0(Type_gs,' imports') ) %>%
      dplyr::select( -Type_gs )
   
   tmp_dtf_world <-
      tmp_world_twoway %>%
      bind_rows(tmp_world_balance) %>%
      bind_rows(tmp_world_balance_g) %>%
      bind_rows(tmp_world_balance_s) %>%
      bind_rows( tmp_world_tot_ex ) %>%
      bind_rows( tmp_world_gs_ex ) %>%
      bind_rows( tmp_world_tot_im ) %>%
      bind_rows( tmp_world_gs_im ) %>%
      mutate( Country = 'World' )
   
   ### produce summary
   tmp_all <-
      tmp_dtf_country_all %>%
      bind_rows( tmp_dtf_world ) 
   
   tmp_value <-
      tmp_all %>%
      filter( Country != 'World', Year == max(Year) ) %>%
      dplyr::select( -Country )
      
      
   tmp_share <- 
      tmp_all %>%
      group_by( Year, Name ) %>%
      #summarise( 'Share' = Value[Country=='Total selected countries']/Value[Country=='World'] ) %>%
      do( Share = .$Value[.$Country=='Total selected countries']/.$Value[.$Country=='World'] ) %>%
      ungroup %>%
      mutate( Share = as.numeric(Share) ) %>%
      mutate( Share = ifelse( Name %in% c('Trade balance','Goods balance','Services balance'), NA, Share ) ) %>%
      filter( Year == max(Year) )
   
   tmp_cagr <-
      tmp_all %>%
      filter( Country != 'World' ) %>%
      group_by( Name, Country ) %>%
      do( CAGR1 = CAGR(.$Value[.$Year==max(.$Year)]/
                          .$Value[.$Year== (max(.$Year)-1) ], 1)/100,
          CAGR5 = CAGR(.$Value[.$Year==max(.$Year)]/
                          .$Value[.$Year== (max(.$Year)-5) ], 5)/100,
          CAGR10 = CAGR(.$Value[.$Year==max(.$Year)]/
                          .$Value[.$Year== (max(.$Year)-10) ], 10)/100
          ) %>%
      mutate( CAGR1 = ifelse(length(CAGR1)==0,NA, as.numeric( CAGR1)),
              CAGR5 =  ifelse(length(CAGR5)==0,NA, as.numeric( CAGR5)) ,
              CAGR10 = ifelse(length(CAGR10)==0,NA, as.numeric( CAGR10)) 
              ) %>%
      mutate( CAGR1 = ifelse( Name %in% c('Trade balance','Goods balance','Services balance'), NA, CAGR1 ),
              CAGR5 = ifelse( Name %in% c('Trade balance','Goods balance','Services balance'), NA, CAGR5 ),
              CAGR10 = ifelse( Name %in% c('Trade balance','Goods balance','Services balance'), NA, CAGR10 )
              )
      
   tmp_tab <-
      tmp_value %>%
      left_join( tmp_share ) %>%
      left_join( tmp_cagr ) %>%
      mutate( Name = factor(Name, levels = c('Total exports', 'Goods exports', 'Services exports',
                                             'Total imports','Goods imports', 'Services imports',
                                             'Two-way trade', 'Trade balance','Goods balance', 'Services balance') ),
              Value = Value/10^6) %>%
      dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10) %>%
      arrange( Name )
   
   return(tmp_tab)
}

## produce summary country table individually ---------------
sum_selected_country_individual <- function( arg_countries){
   required_name <- c("Total exports", "Goods exports", 'Services exports',
                      "Total imports", 'Goods imports', 'Services imports',
                      "Two-way trade", "Trade balance", "Goods balance", "Services balance")
   tab_list <- 
      lapply( arg_countries,
              function(i_country){
                 print(i_country)
                 tmp_tab <- sum_selected_country( i_country )
                 ## check if any missing
                 missing_name <- setdiff( required_name, tmp_tab$Name)
                 
                 if( length(missing_name) > 0 ){
                    for(i_name in missing_name){
                       tmp_tab <- 
                          tmp_tab %>%
                          bind_rows( data.frame(Name = i_name) )
                    }
                 }
                 return(tmp_tab)
              })
   
   names(tab_list) <- arg_countries

   
   ex_tab <-
      do.call( 'rbind', 
               lapply( arg_countries,
                       function(i_country){
                          tmp_tab <- tab_list[[i_country]]
                          tmp_tab_ex <-
                             data.frame( Country = i_country,
                                         TotExValue = tmp_tab$Value[tmp_tab$Name=='Total exports'],
                                         TotExShare = tmp_tab$Share[tmp_tab$Name=='Total exports'],
                                         TotExCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Total exports'],
                                         GExValue = tmp_tab$Value[tmp_tab$Name=='Goods exports'],
                                         GExShare = tmp_tab$Share[tmp_tab$Name=='Goods exports'],
                                         GExCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Goods exports'],
                                         SExValue = tmp_tab$Value[tmp_tab$Name=='Services exports'],
                                         SExShare = tmp_tab$Share[tmp_tab$Name=='Services exports'],
                                         SExCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Services exports']
                             )
                          return( tmp_tab_ex) 
                       }) )
   
   ex_tab %<>% arrange(-TotExValue)
   
   
   im_tab <- 
      do.call( 'rbind', 
               lapply( arg_countries,
                       function(i_country){
                          tmp_tab <- tab_list[[i_country]]
                          tmp_tab_im <-
                             data.frame( Country = i_country,
                                         TotImValue = tmp_tab$Value[tmp_tab$Name=='Total imports'],
                                         TotImShare = tmp_tab$Share[tmp_tab$Name=='Total imports'],
                                         TotImCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Total imports'],
                                         GImValue = tmp_tab$Value[tmp_tab$Name=='Goods imports'],
                                         GImShare = tmp_tab$Share[tmp_tab$Name=='Goods imports'],
                                         GImCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Goods imports'],
                                         SImValue = tmp_tab$Value[tmp_tab$Name=='Services imports'],
                                         SImShare = tmp_tab$Share[tmp_tab$Name=='Services imports'],
                                         SImCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Services imports']
                                        )
                          return(tmp_tab_im ) }) )
   im_tab %<>% arrange(-TotImValue)
   
   twoway_balance_tab <- 
      do.call( 'rbind', 
               lapply( arg_countries,
                       function(i_country){
                          tmp_tab <- tab_list[[i_country]]
                          tmp_tab_tb <-
                             data.frame( Country = i_country,
                                         TwowayValue = tmp_tab$Value[tmp_tab$Name=='Two-way trade'],
                                         TwowayShare = tmp_tab$Share[tmp_tab$Name=='Two-way trade'],
                                         TwowayCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Two-way trade'],
                                         BalanceValue = tmp_tab$Value[tmp_tab$Name=='Trade balance'],
                                         BalanceValue_g = tmp_tab$Value[tmp_tab$Name=='Goods balance'],
                                         BalanceValue_s = tmp_tab$Value[tmp_tab$Name=='Services balance']
                                         
                             )
                          return( tmp_tab_tb  ) }) )
   
   twoway_balance_tab %<>% arrange(-TwowayValue)
   
   return( list(Ex = ex_tab, Im = im_tab, TB = twoway_balance_tab ))
}

## function to production SNZ defined commodity by country ------------------
get_snz_gs_country <- function( Exports_or_imports = 'Exports', selected_country ){
   ## Exports ---------------------------
   if( Exports_or_imports == 'Exports' ){
      tmp_dtf_commodity_tot <- 
         dtf_shiny_full %>%
         filter( Type_ie == Exports_or_imports ,
                 Country %in% selected_country,
                 Type_gs == 'Goods',
                 Commodity %in% c("Total goods"),
                 Year >= 2007 ) %>%
         rename( SNZ_commodity = Commodity ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
         summarise( Value = sum(Value, na.rm=T) ) %>%
         ungroup %>%
         mutate( Country = 'The selected countries' )
      
      tmp_dtf_commodity_country <- 
         dtf_shiny_full %>%
         filter( Type_ie == Exports_or_imports ,
                 Country %in% selected_country,
                 !Commodity %in% c("Total goods", "Total services"),
                 Type_gs == 'Goods', Year>=2007) %>%
         left_join( concord_snz_eg, by = c('Commodity' = 'HS_codes') ) %>%
         filter( !is.na(SNZ_commodity) ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note) %>%
         summarise( Value = sum(Value, na.rm=T) ) %>%
         ungroup %>%
         mutate( Country = 'The selected countries' ) %>%
         bind_rows( tmp_dtf_commodity_tot )
      
      tmp_dtf_other_commodity <-
         tmp_dtf_commodity_country %>%
         group_by( Year, Type_ie, Type_gs, Note ) %>%
         do( Value = .$Value[.$SNZ_commodity=='Total goods'] - sum(.$Value[.$SNZ_commodity!='Total goods']) ) %>%
         ungroup %>%
         mutate( Value = unlist(Value) ) %>%
         mutate( SNZ_commodity = 'Other goods' ) %>%
         mutate( Country = 'The selected countries' ) 
      
      ## services
      tmp_dtf_service_tot <- 
         dtf_shiny_full %>%
         filter( Type_ie == Exports_or_imports ,
                 Country %in% selected_country,
                 Type_gs == 'Services',
                 Commodity %in% c("Total services"),
                 Year >= 2007 ) %>%
         rename( SNZ_commodity = Commodity ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
         summarise( Value = sum(Value, na.rm=T) ) %>%
         ungroup %>%
         mutate( Country = 'The selected countries' )
      
      tmp_dtf_service_country <- 
         dtf_shiny_full %>%
         filter( Type_ie == Exports_or_imports ,
                 Country %in% selected_country,
                 !Commodity %in% c("Total goods", "Total services"),
                 Type_gs == 'Services', Year>=2007) %>%
         rename(SNZ_commodity = Commodity ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
         summarise( Value = sum(Value, na.rm=T) ) %>%
         ungroup %>%
         bind_rows( tmp_dtf_service_tot ) %>%
         mutate( Country = 'The selected countries' )
      
      ## just to balance the offical total service exports and the sum of service exports by country
      tmp_dtf_other_service <-
         tmp_dtf_service_country %>%
         group_by(  Year, Type_ie, Type_gs, Note) %>%
         do( Value = .$Value[.$SNZ_commodity=='Total services'] - sum(.$Value[.$SNZ_commodity!='Total services']) ) %>%
         ungroup %>%
         mutate( Country = 'The selected countries' )
      
      if( nrow(tmp_dtf_other_service) >0 ){
         tmp_dtf_other_service %<>%
            mutate( Value = ifelse(!is.null(Value), unlist(Value), NA ) ) %>%
            mutate( SNZ_commodity = 'Other services' )
         }
      
      ### bind toghter 
      tmp_dtf_commodity_service_ex_country <-
         tmp_dtf_commodity_country %>%
         filter( SNZ_commodity != 'Total goods' ) %>%
         bind_rows( tmp_dtf_other_commodity ) %>%
         bind_rows(
            tmp_dtf_service_country %>%
               filter( SNZ_commodity != 'Total services' ) %>%
               bind_rows( tmp_dtf_other_service  ) %>%
               group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note, Country ) %>%
               summarise( Value = sum(Value, na.rm=T) ) %>%
               ungroup
         ) #%>%
         #bind_rows( tmp_dtf_other_service  ) 
      
      ## calculate CAGR5
      tmp_dtf_commodity_service_ex_country %<>%
         left_join(tmp_dtf_commodity_service_ex_country %>%
                      group_by(  Type_ie, Type_gs, SNZ_commodity, Note) %>%
                      do( CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                           .$Value[.$Year == (max(.$Year)-5) ], 5) ) %>%
                      ungroup %>%
                      mutate( CAGR5 = as.numeric(CAGR5) ) %>%
                      mutate( CAGR5 = ifelse(CAGR5==Inf, NA, CAGR5) )
                   )
      
      #### select world
      tmp_dtf_commodity_service_world <-
         dtf_shiny_commodity_service_ex %>%
         filter( Type_ie == Exports_or_imports ,
                 Country %in% 'World',
                 Year >= 2007  )
      
      ## calculate share
      tmp_dtf_commodity_service_ex_country %<>%
         bind_rows( tmp_dtf_commodity_service_world ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
         mutate( Share = Value/Value[Country=='World']*100 ) %>%
         ungroup %>%
         filter( Country != 'World' ) %>%
         filter( !is.na(CAGR5), Share>=0 )
      
      return(tmp_dtf_commodity_service_ex_country)
   }
   #### Improts ----------------------------
   if( Exports_or_imports == 'Imports' ){
      tmp_dtf_commodity_tot <- 
         dtf_shiny_full %>%
         filter( Type_ie == Exports_or_imports ,
                 Country %in% selected_country,
                 Type_gs == 'Goods',
                 Commodity %in% c("Total goods"),
                 Year >= 2007 ) %>%
         rename( SNZ_commodity = Commodity ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
         summarise( Value = sum(Value, na.rm=T) ) %>%
         ungroup %>%
         mutate( Country = 'The selected countries' )
      
      tmp_dtf_commodity_country <- 
         dtf_shiny_full %>%
         filter( Type_ie == Exports_or_imports ,
                 Country %in% selected_country,
                 !Commodity %in% c("Total goods", "Total services"),
                 Type_gs == 'Goods', Year>=2007) %>%
         left_join( concord_snz_ig, by = c('Commodity' = 'HS_codes') ) %>%
         filter( !is.na(SNZ_commodity) ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note) %>%
         summarise( Value = sum(Value, na.rm=T) ) %>%
         ungroup %>%
         mutate( Country = 'The selected countries' ) %>%
         bind_rows( tmp_dtf_commodity_tot )
      
      tmp_dtf_other_commodity <-
         tmp_dtf_commodity_country %>%
         group_by( Year, Type_ie, Type_gs, Note ) %>%
         do( Value = .$Value[.$SNZ_commodity=='Total goods'] - sum(.$Value[.$SNZ_commodity!='Total goods']) ) %>%
         ungroup %>%
         mutate( Value = unlist(Value) ) %>%
         mutate( SNZ_commodity = 'Other goods' ) %>%
         mutate( Country = 'The selected countries' ) 
      
      ## services
      tmp_dtf_service_tot <- 
         dtf_shiny_full %>%
         filter( Type_ie == Exports_or_imports ,
                 Country %in% selected_country,
                 Type_gs == 'Services',
                 Commodity %in% c("Total services"),
                 Year >= 2007 ) %>%
         rename( SNZ_commodity = Commodity ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
         summarise( Value = sum(Value, na.rm=T) ) %>%
         ungroup %>%
         mutate( Country = 'The selected countries' )
      
      tmp_dtf_service_country <- 
         dtf_shiny_full %>%
         filter( Type_ie == Exports_or_imports ,
                 Country %in% selected_country,
                 !Commodity %in% c("Total goods", "Total services"),
                 Type_gs == 'Services', Year>=2007) %>%
         rename(SNZ_commodity = Commodity ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
         summarise( Value = sum(Value, na.rm=T) ) %>%
         ungroup %>%
         bind_rows( tmp_dtf_service_tot ) %>%
         mutate( Country = 'The selected countries' )
      
      ## just to balance the offical total service exports and the sum of service exports by country
      tmp_dtf_other_service <-
         tmp_dtf_service_country %>%
         group_by(  Year, Type_ie, Type_gs, Note) %>%
         do( Value = .$Value[.$SNZ_commodity=='Total services'] - sum(.$Value[.$SNZ_commodity!='Total services']) ) %>%
         ungroup %>%
         mutate( Country = 'The selected countries' )
      
      if( nrow(tmp_dtf_other_service) >0 ){
         tmp_dtf_other_service %<>%
            mutate( Value = ifelse(!is.null(Value), unlist(Value), NA ) ) %>%
            mutate( SNZ_commodity = 'Other services' )
      }
      
      ### bind toghter 
      tmp_dtf_commodity_service_im_country <-
         tmp_dtf_commodity_country %>%
         filter( SNZ_commodity != 'Total goods' ) %>%
         bind_rows( tmp_dtf_other_commodity ) %>%
         bind_rows(
            tmp_dtf_service_country %>%
               filter( SNZ_commodity != 'Total services' )%>%
               bind_rows( tmp_dtf_other_service  ) %>%
               group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note, Country ) %>%
               summarise( Value = sum(Value, na.rm=T) ) %>%
               ungroup
         ) #%>%
         #bind_rows( tmp_dtf_other_service  ) 
      
      ## calculate CAGR5
      tmp_dtf_commodity_service_im_country %<>%
         left_join(tmp_dtf_commodity_service_im_country %>%
                      group_by(  Type_ie, Type_gs, SNZ_commodity, Note) %>%
                      do( CAGR5 = CAGR( .$Value[.$Year == max(.$Year)]/
                                           .$Value[.$Year == (max(.$Year)-5) ], 5) ) %>%
                      ungroup %>%
                      mutate( CAGR5 = as.numeric(CAGR5) ) %>%
                      mutate( CAGR5 = ifelse(CAGR5==Inf, NA, CAGR5) )
         )
      
      #### select world
      tmp_dtf_commodity_service_world <-
         dtf_shiny_commodity_service_im %>%
         filter( Type_ie == Exports_or_imports ,
                 Country %in% 'World',
                 Year >= 2007  )
      
      ## calculate share
      tmp_dtf_commodity_service_im_country %<>%
         bind_rows( tmp_dtf_commodity_service_world ) %>%
         group_by( Year, Type_ie, Type_gs, SNZ_commodity, Note ) %>%
         mutate( Share = Value/Value[Country=='World']*100 ) %>%
         ungroup %>%
         filter( Country != 'World' ) %>%
         filter( !is.na(CAGR5), Share>=0 )
      
      return(tmp_dtf_commodity_service_im_country)
   }
}


######## map heat map -----------------
hc_add_series_treemap2 = with(environment(hc_add_series_treemap),
                              ## Modified `hc_add_series_treemap`
                              ## names colorValue correctly for connection to `hc_colorAxis`
                              function (hc, tm, ...)
                              {
                                 assertthat::assert_that( is.highchart(hc),is.list(tm))
                                 df <- tm$tm %>% tbl_df() %>% select_("-x0", "-y0", "-w", 
                                                                      "-h", "-stdErr", "-vColorValue") %>% rename_(value = "vSize", 
                                                                                                                   colorValue = "vColor") %>% purrr::map_if(is.factor, as.character) %>% 
                                    data.frame(stringsAsFactors = FALSE) %>% tbl_df()
                                 ndepth <- which(names(df) == "value") - 1
                                 ds <- map_df(seq(ndepth), function(lvl) {
                                    df2 <- df %>% filter_(sprintf("level == %s", lvl)) %>% 
                                       rename_(name = names(df)[lvl]) %>% mutate_(id = "highcharter::str_to_id(name)")
                                    if (lvl > 1) {
                                       df2 <- df2 %>% mutate_(parent = names(df)[lvl - 1], 
                                                              parent = "highcharter::str_to_id(parent)")
                                    }
                                    else {
                                       df2 <- df2 %>% mutate_(parent = NA)
                                    }
                                    df2
                                 })
                                 ds <- list_parse(ds)
                                 ds <- map(ds, function(x) {
                                    if (is.na(x$parent)) 
                                       x$parent <- NULL
                                    x
                                 })
                                 hc %>% hc_add_series(data = ds, type = "treemap", ...)
                              }
)

##### get investment country data ----------------
sum_selected_country_investment <- function( arg_countries ){
   ## for selected countries
   tmp_country_all_base <- 
      dtf_fdi_odi %>%
      filter( Country %in% arg_countries,
              Year >= 2007 ) 
   
   tmp_country_all_twoway <-
      tmp_country_all_base %>%
      group_by( Year ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Two-way direct investment' ) 
   
   tmp_country_all_balance <- 
      tmp_country_all_base %>%
      group_by( Year, Type ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      group_by( Year ) %>%
      #summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
      do( Value = .$Value[.$Type=='ODI'] - .$Value[.$Type=='FDI'] ) %>%
      ungroup %>%
      mutate( Value = as.numeric(Value) ) %>%
      mutate( Name = 'Direct investment balance' ) 
   
   tmp_country_all_tot_fdi <- 
      tmp_country_all_base %>%
      filter( Type == 'FDI' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Foreign direct investment' ) 
   
   tmp_country_all_tot_odi <- 
      tmp_country_all_base %>%
      filter( Type == 'ODI' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Overseas direct investment' ) 
   
   
   tmp_dtf_country_all <-
      tmp_country_all_twoway %>%
      #bind_rows(tmp_country_all_balance) %>%
      bind_rows( tmp_country_all_tot_fdi ) %>%
      #bind_rows( tmp_country_all_gs_ex ) %>%
      bind_rows( tmp_country_all_tot_odi ) %>%
      #bind_rows( tmp_country_all_gs_im ) %>%
      mutate( Country = 'Total selected countries' )
   
   ## for world
   tmp_world_base <- 
      dtf_fdi_odi %>%
      filter( Country %in% 'World',
              Year >= 2007 ) 
   
   tmp_world_twoway <-
      tmp_world_base %>%
      group_by( Year ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Two-way direct investment' ) 
   
   tmp_world_balance <- 
      tmp_world_base %>%
      group_by( Year, Type ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      group_by( Year ) %>%
      summarise( Value = Value[Type=='ODI'] - Value[Type=='FDI'] ) %>%
      ungroup %>%
      mutate( Name = 'Direct investment balance' ) 
   
   tmp_world_tot_fdi <- 
      tmp_world_base %>%
      filter( Type == 'FDI' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Foreign direct investment' ) 
   
   
   tmp_world_tot_odi <- 
      tmp_world_base %>%
      filter( Type == 'ODI' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Overseas direct investment' ) 
   
   
   tmp_dtf_world <-
      tmp_world_twoway %>%
      #bind_rows(tmp_world_balance) %>%
      bind_rows( tmp_world_tot_fdi ) %>%
      #bind_rows( tmp_world_gs_ex ) %>%
      bind_rows( tmp_world_tot_odi ) %>%
      #bind_rows( tmp_world_gs_im ) %>%
      mutate( Country = 'World' )
   
   ### produce summary
   tmp_all <-
      tmp_dtf_country_all %>%
      bind_rows( tmp_dtf_world ) 
   
   tmp_value <-
      tmp_all %>%
      filter( Country != 'World', Year == max(Year) ) %>%
      dplyr::select( -Country )
   
   
   tmp_share <- 
      tmp_all %>%
      group_by( Year, Name ) %>%
      #summarise( 'Share' = Value[Country=='Total selected countries']/Value[Country=='World'] ) %>%
      do( Share = .$Value[.$Country=='Total selected countries']/.$Value[.$Country=='World'] ) %>%
      ungroup %>%
      mutate( Share = as.numeric(Share) ) %>%
      mutate( Share = ifelse( Name == 'Direct investment balance', NA, Share ) ) %>%
      filter( Year == max(Year) )
   
   tmp_cagr <-
      tmp_all %>%
      filter( Country != 'World' ) %>%
      group_by( Name, Country ) %>%
      do( CAGR1 = CAGR(.$Value[.$Year==max(.$Year)]/
                          .$Value[.$Year== (max(.$Year)-1) ], 1)/100,
          CAGR5 = CAGR(.$Value[.$Year==max(.$Year)]/
                          .$Value[.$Year== (max(.$Year)-5) ], 5)/100,
          CAGR10 = CAGR(.$Value[.$Year==max(.$Year)]/
                           .$Value[.$Year== (max(.$Year)-10) ], 10)/100
      ) %>%
      mutate( CAGR1 = ifelse(length(CAGR1)==0,NA, as.numeric( CAGR1)),
              CAGR5 =  ifelse(length(CAGR5)==0,NA, as.numeric( CAGR5)) ,
              CAGR10 = ifelse(length(CAGR10)==0,NA, as.numeric( CAGR10)) 
      ) %>%
      mutate( CAGR1 = ifelse( Name == 'Direct investment balance', NA, CAGR1 ),
              CAGR5 = ifelse( Name == 'Direct investment balance', NA, CAGR5 ),
              CAGR10 = ifelse( Name == 'Direct investment balance', NA, CAGR10 )
      )
   
   tmp_tab <-
      tmp_value %>%
      left_join( tmp_share ) %>%
      left_join( tmp_cagr ) %>%
      mutate( Name = factor(Name, levels = c('Foreign direct investment', 
                                             'Overseas direct investment', 
                                             'Two-way direct investment') 
                            ),
              Value = Value) %>%
      dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10) %>%
      arrange( Name )
   
   return(tmp_tab)
}

## Get individual investment by market -----------------
sum_selected_country_individual_investment <- function( arg_countries){
   
   required_name <- c("Foreign direct investment", 
                      "Overseas direct investment", 
                      'Two-way direct investment' )
   tab_list <- 
      lapply( arg_countries,
              function(i_country){
                 print(i_country)
                 tmp_tab <- sum_selected_country_investment( i_country )
                 ## check if any missing
                 missing_name <- setdiff( required_name, tmp_tab$Name)
                 
                 if( length(missing_name) > 0 ){
                    for(i_name in missing_name){
                       tmp_tab <- 
                          tmp_tab %>%
                          bind_rows( data.frame(Name = i_name) )
                    }
                 }
                 return(tmp_tab)
              })
   
   names(tab_list) <- arg_countries
   
   
   investment_tab <-
      do.call( 'rbind', 
               lapply( arg_countries,
                       function(i_country){
                          tmp_tab <- tab_list[[i_country]]
                          tmp_tab_ex <-
                             data.frame( Country = i_country,
                                         FDIValue = tmp_tab$Value[tmp_tab$Name=='Foreign direct investment'],
                                         FDIShare = tmp_tab$Share[tmp_tab$Name=='Foreign direct investment'],
                                         FDICAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Foreign direct investment'],
                                         ODIValue = tmp_tab$Value[tmp_tab$Name=='Overseas direct investment'],
                                         ODIShare = tmp_tab$Share[tmp_tab$Name=='Overseas direct investment'],
                                         ODICAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Overseas direct investment'],
                                         TwowayDIValue = tmp_tab$Value[tmp_tab$Name=='Two-way direct investment'],
                                         TwowayDIShare = tmp_tab$Share[tmp_tab$Name=='Two-way direct investment'],
                                         TwowayDICAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Two-way direct investment']
                             )
                          return( tmp_tab_ex) 
                       }) )
   
   investment_tab %<>% arrange(-FDIValue)
   
   return( investment_tab )
}




##### get ppl movement country data ---------------------
sum_selected_country_pplmove <- function( arg_countries ){
   ## for selected countries
   tmp_country_all_base <- 
      dtf_in_out %>%
      filter( Country %in% arg_countries,
              Year >= 2007 ) 
   
   tmp_country_all_twoway <-
      tmp_country_all_base %>%
      group_by( Year ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Two-way visitor movement' ) 
   
   tmp_country_all_balance <- 
      tmp_country_all_base %>%
      group_by( Year, Type ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      group_by( Year ) %>%
      #summarise( Value = Value[Type_ie=='Exports'] - Value[Type_ie=='Imports'] ) %>%
      do( Value = .$Value[.$Type=='NZ visitors travelling out'] - .$Value[.$Type=='Foreign visitors travelling in'] ) %>%
      ungroup %>%
      mutate( Value = as.numeric(Value) ) %>%
      mutate( Name = 'Visitor movement balance' ) 
   
   tmp_country_all_tot_in <- 
      tmp_country_all_base %>%
      filter( Type == 'Foreign visitors travelling in' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Foreign visitors travelling in' ) 
   
   tmp_country_all_tot_out <- 
      tmp_country_all_base %>%
      filter( Type == 'NZ visitors travelling out' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'NZ visitors travelling out' ) 
   
   
   tmp_dtf_country_all <-
      tmp_country_all_twoway %>%
      #bind_rows(tmp_country_all_balance) %>%
      bind_rows( tmp_country_all_tot_in ) %>%
      #bind_rows( tmp_country_all_gs_ex ) %>%
      bind_rows( tmp_country_all_tot_out ) %>%
      #bind_rows( tmp_country_all_gs_im ) %>%
      mutate( Country = 'Total selected countries' )
   
   ## for world
   tmp_world_base <- 
      dtf_in_out %>%
      filter( Country %in% 'World',
              Year >= 2007 ) 
   
   tmp_world_twoway <-
      tmp_world_base %>%
      group_by( Year ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Two-way visitor movement' ) 
   
   tmp_world_balance <- 
      tmp_world_base %>%
      group_by( Year, Type ) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      group_by( Year ) %>%
      summarise( Value = Value[Type=='NZ visitors travelling out'] - Value[Type=='Foreign visitors travelling in'] ) %>%
      ungroup %>%
      mutate( Name = 'Visitor movement balance' ) 
   
   tmp_world_tot_in <- 
      tmp_world_base %>%
      filter( Type == 'Foreign visitors travelling in' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'Foreign visitors travelling in' ) 
   
   
   tmp_world_tot_out <- 
      tmp_world_base %>%
      filter( Type == 'NZ visitors travelling out' ) %>%
      group_by( Year) %>%
      summarise( Value = sum( Value, na.rm=T ) ) %>%
      ungroup %>%
      mutate( Name = 'NZ visitors travelling out' ) 
   
   
   tmp_dtf_world <-
      tmp_world_twoway %>%
      #bind_rows(tmp_world_balance) %>%
      bind_rows( tmp_world_tot_in ) %>%
      #bind_rows( tmp_world_gs_ex ) %>%
      bind_rows( tmp_world_tot_out ) %>%
      #bind_rows( tmp_world_gs_im ) %>%
      mutate( Country = 'World' )
   
   ### produce summary
   tmp_all <-
      tmp_dtf_country_all %>%
      bind_rows( tmp_dtf_world ) 
   
   tmp_value <-
      tmp_all %>%
      filter( Country != 'World', Year == max(Year) ) %>%
      dplyr::select( -Country )
   
   
   tmp_share <- 
      tmp_all %>%
      group_by( Year, Name ) %>%
      #summarise( 'Share' = Value[Country=='Total selected countries']/Value[Country=='World'] ) %>%
      do( Share = .$Value[.$Country=='Total selected countries']/.$Value[.$Country=='World'] ) %>%
      ungroup %>%
      mutate( Share = as.numeric(Share) ) %>%
      mutate( Share = ifelse( Name == 'Visitor movement balance', NA, Share ) ) %>%
      filter( Year == max(Year) )
   
   tmp_cagr <-
      tmp_all %>%
      filter( Country != 'World' ) %>%
      group_by( Name, Country ) %>%
      do( CAGR1 = CAGR(.$Value[.$Year==max(.$Year)]/
                          .$Value[.$Year== (max(.$Year)-1) ], 1)/100,
          CAGR5 = CAGR(.$Value[.$Year==max(.$Year)]/
                          .$Value[.$Year== (max(.$Year)-5) ], 5)/100,
          CAGR10 = CAGR(.$Value[.$Year==max(.$Year)]/
                           .$Value[.$Year== (max(.$Year)-10) ], 10)/100
      ) %>%
      mutate( CAGR1 = ifelse(length(CAGR1)==0,NA, as.numeric( CAGR1)),
              CAGR5 =  ifelse(length(CAGR5)==0,NA, as.numeric( CAGR5)) ,
              CAGR10 = ifelse(length(CAGR10)==0,NA, as.numeric( CAGR10)) 
      ) %>%
      mutate( CAGR1 = ifelse( Name == 'Visitor movement balance', NA, CAGR1 ),
              CAGR5 = ifelse( Name == 'Visitor movement balance', NA, CAGR5 ),
              CAGR10 = ifelse( Name == 'Visitor movement balance', NA, CAGR10 )
      )
   
   tmp_tab <-
      tmp_value %>%
      left_join( tmp_share ) %>%
      left_join( tmp_cagr ) %>%
      mutate( Name = factor(Name, levels = c('Foreign visitors travelling in', 
                                             'NZ visitors travelling out', 
                                             'Two-way visitor movement') 
                            ),
      Value = Value/10^3) %>%
      dplyr::select( Name, Value, Share, CAGR1, CAGR5, CAGR10) %>%
      arrange( Name )
   
   return(tmp_tab)
}

## Get individual ppl movement by market ---------------------------
sum_selected_country_individual_pplmove <- function( arg_countries){
   
   required_name <- c("Foreign visitors travelling in", 
                      "NZ visitors travelling out", 
                      'Two-way visitor movement' )
   tab_list <- 
      lapply( arg_countries,
              function(i_country){
                 print(i_country)
                 tmp_tab <- sum_selected_country_pplmove( i_country )
                 ## check if any missing
                 missing_name <- setdiff( required_name, tmp_tab$Name)
                 
                 if( length(missing_name) > 0 ){
                    for(i_name in missing_name){
                       tmp_tab <- 
                          tmp_tab %>%
                          bind_rows( data.frame(Name = i_name) )
                    }
                 }
                 return(tmp_tab)
              })
   
   names(tab_list) <- arg_countries
   
   
   pplmove_tab <-
      do.call( 'rbind', 
               lapply( arg_countries,
                       function(i_country){
                          tmp_tab <- tab_list[[i_country]]
                          tmp_tab_ex <-
                             data.frame( Country = i_country,
                                         InValue = tmp_tab$Value[tmp_tab$Name=='Foreign visitors travelling in'],
                                         InShare = tmp_tab$Share[tmp_tab$Name=='Foreign visitors travelling in'],
                                         InCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Foreign visitors travelling in'],
                                         OutValue = tmp_tab$Value[tmp_tab$Name=='NZ visitors travelling out'],
                                         OutShare = tmp_tab$Share[tmp_tab$Name=='NZ visitors travelling out'],
                                         OutCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='NZ visitors travelling out'],
                                         TwowayMoveValue = tmp_tab$Value[tmp_tab$Name=='Two-way visitor movement'],
                                         TwowayMoveShare = tmp_tab$Share[tmp_tab$Name=='Two-way visitor movement'],
                                         TwowayMoveCAGR5 = tmp_tab$CAGR5[tmp_tab$Name=='Two-way visitor movement']
                             )
                          return( tmp_tab_ex) 
                       }) )
   
   pplmove_tab %<>% arrange(-InValue)
   
   return( pplmove_tab )
}


## get data from un comtrade function https://comtrade.un.org/data/Doc/api/ex/r-------
#outputDir <- "uncomtrade"

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
   string<- paste(url
                  ,"max=",maxrec,"&" #maximum no. of records returned
                  ,"type=",type,"&" #type of trade (c=commodities)
                  ,"freq=",freq,"&" #frequency
                  ,"px=",px,"&" #classification
                  ,"ps=",ps,"&" #time period
                  ,"r=",r,"&" #reporting area
                  ,"p=",p,"&" #partner country
                  ,"rg=",rg,"&" #trade flow
                  ,"cc=",cc,"&" #classification code
                  ,"fmt=",fmt        #Format
                  ,sep = ""
   )
   
   if(fmt == "csv") {
      download.file(string,"uncomtrade.csv", mode = 'w')
      #raw.data<- read.csv( string ,header=TRUE)
      #files <- list.files(tempdir(), full.names = TRUE)
      raw.data<- read.csv( "uncomtrade.csv" ,header=TRUE)
      unlink("uncomtrade.csv")
      return(list(validation=NULL, data=raw.data))
   } else {
      if(fmt == "json" ) {
         #download.file(string, "uncomtrade.txt", mode = 'w')
         raw.data<- rjson::fromJSON(file=string)
         #raw.data<- rjson::fromJSON(file="uncomtrade.txt")
         #unlink("uncomtrade.txt")
         data<- raw.data$dataset
         validation<- unlist(raw.data$validation, recursive=TRUE)
         ndata<- NULL
         if(length(data)> 0) {
            var.names<- names(data[[1]])
            data<- as.data.frame(t( sapply(data,rbind)))
            ndata<- NULL
            for(i in 1:ncol(data)){
               data[sapply(data[,i],is.null),i]<- NA
               ndata<- cbind(ndata, unlist(data[,i]))
            }
            ndata<- as.data.frame(ndata)
            colnames(ndata)<- var.names
         }
         return(list(validation=validation,data =ndata))
      }
   }
}



## Sankey plot for a HS code  --------------
sankey_uncomtrade <- 
   function( cc = '0409',
             global_coverage = 0.9,
             trade_weight = 0.005,
             max_year = 2016,
             center_country = "New Zealand",
             eu_internal = 'No',
             height = 628, width = 1207,
             ...)
   {

      ## 0. work on cc
      if( is.null(cc)|cc==''|is.na(cc) ){
         stop('Commodity code (cc) cannot be empty!')
      }
      
      if( length(cc) > 1 ){
         stop('The current function can only handle ONE HS code at a time.')
      }
      
      # if( length(eu_internal) == 0 ){
      #    eu_internal  = "No"
      # }
      
      ## 1. download world export data
      print("-------------- Download Export data -----------")
      
      tmp_ex_tot_raw_try <- try(
         tmp_ex_tot_raw <- 
            #get.Comtrade( r = 'all', p ='0' , rg = 2 , cc = cc , fmt = 'csv', ps = max_year)$data
             m_ct_search( reporters = "All", partners = 'World', trade_direction = c("exports"), freq = "annual",
                         commod_codes = cc,
                         start_date = max_year,
                         end_date = max_year )  %>%
            dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner,  partner_iso, qty_unit,  qty, trade_value_usd) %>%
            rename( Year = year,
                    `Commodity` = commodity ,
                    `Commodity.Code` = commodity_code ,
                    `Trade.Flow` = trade_flow,
                    Reporter = reporter,
                    `Reporter.ISO` =  reporter_iso,
                    `Reporter.Code` = reporter_code,
                     Partner = partner,
                    `Partner.ISO` = partner_iso,
                    `Qty.Unit` = qty_unit,
                    `Alt.Qty.Unit` = qty,
                    `Trade.Value..US..` = trade_value_usd )
      )
      
      ## meesage
      if( class(tmp_ex_tot_raw_try) == 'try-error' ){
         print("-------------- Download Export data ERROR!! -----------")
         stop("Cannot download data from UN Com Trade!")
      }else{
         print("-------------- FINISH Download Export data -----------")
      }
      
      ## 2. download eu export data
      print("-------------- Download EU28 Export data -----------")
      
      tmp_ex_eu_extra_raw_try <- try(
         tmp_ex_eu_extra_raw <- 
            #get.Comtrade( r = '97', p ='0' , rg = 2 , cc = cc , fmt = 'csv', ps = max_year)$data
             m_ct_search( reporters = "EU-28", partners = 'World', trade_direction = c( "exports"), freq = "annual",
                         commod_codes = cc,
                         start_date = max_year,
                         end_date = max_year )  %>%
            dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner, partner_iso, qty_unit,  qty, trade_value_usd) %>%
            rename( Year = year,
                    `Commodity` = commodity ,
                    `Commodity.Code` = commodity_code ,
                    `Trade.Flow` = trade_flow,
                    Reporter = reporter,
                    `Reporter.ISO` =  reporter_iso,
                    `Reporter.Code` = reporter_code,
                    Partner = partner,
                    `Partner.ISO` = partner_iso,
                    `Qty.Unit` = qty_unit,
                    `Alt.Qty.Unit` = qty,
                    `Trade.Value..US..` = trade_value_usd )
      )
      
      ## meesage
      if( class(tmp_ex_eu_extra_raw_try ) == 'try-error' ){
         print("-------------- Download EU28 Export data ERROR!! -----------")
         stop("Cannot download data from UN Com Trade!")
      }else{
         print("-------------- FINISH Download EU28 Export data -----------")
      }
      
      ## if no EU data
      if( #!tmp_ex_eu_extra_raw$Classification %in% c('H1',"H2", "H4","H6")
          is.na(tmp_ex_eu_extra_raw$`Trade.Value..US..`) ){
         print("------------- EU28 export data NOT available ----------------")
         tmp_ex_eu_extra_raw$Year <- unique(tmp_ex_tot_raw$Year )
         tmp_ex_eu_extra_raw$Trade.Flow <- unique(tmp_ex_tot_raw$Trade.Flow)
         tmp_ex_eu_extra_raw$Reporter <- "EU-28"
         tmp_ex_eu_extra_raw$Reporter.ISO <- "EU2"
         tmp_ex_eu_extra_raw$Reporter.Code <- 97
         tmp_ex_eu_extra_raw$Partner <- "World"
         tmp_ex_eu_extra_raw$Partner.ISO <- "WLD"
         tmp_ex_eu_extra_raw$Commodity.Code <- unique(tmp_ex_tot_raw$Commodity.Code)
         tmp_ex_eu_extra_raw$Commodity <- unique(tmp_ex_tot_raw$Commodity)[1]
         tmp_ex_eu_extra_raw$Qty.Unit <- unique(tmp_ex_tot_raw$Qty.Unit)[1]
      }
      
      ## 3. all countries export data 
      tmp_ex_tot <-
         tmp_ex_tot_raw %>%
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         mutate( Reporter = as.character(Reporter), 
                 Reporter.ISO = as.character(Reporter.ISO),
                 Partner = as.character( Partner ),
                 Partner.ISO = as.character( Partner.ISO )
         )
      
      ## 3.1 get total export value and quantity
      tmp_tot_ex_value <- sum( as.numeric( tmp_ex_tot$`Trade.Value..US..`), na.rm=T )
      tmp_tot_ex_qty <- sum(  as.numeric(tmp_ex_tot$Alt.Qty.Unit), na.rm=T )
      
      ## 4. put all EU28 countries together and later no there will be EU28 internal trade shown
      if( #tmp_ex_eu_extra_raw$Classification %in% c('H1',"H2", "H4","H6")
         !is.na(tmp_ex_eu_extra_raw$`Trade.Value..US..`)  ){
         tmp_ex_tot_withEU <- 
            tmp_ex_tot %>%
            mutate( Reporter = ifelse( Reporter.ISO %in% concord_eu28$ISO3, "EU-28", Reporter ) ) %>%
            mutate( Reporter.ISO = ifelse( Reporter == "EU-28", "EU2" , Reporter.ISO )  ) %>%
            mutate( Reporter.Code = ifelse( Reporter == "EU-28", 97 , Reporter.Code )   ) %>%
            mutate( Partner = ifelse( Reporter == "EU-28", "World" , Partner )   ) %>%
            mutate( Partner.ISO = ifelse( Reporter == "EU-28", "WLD" , Partner.ISO )   ) %>%
            #group_by(  Year, Reporter,Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity ,Trade.Flow, Qty.Unit ) %>%
            group_by(  Year, Reporter,Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity ,Trade.Flow ) %>%
            summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
                        `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
            ) %>%
            ungroup 
      }else{
         tmp_ex_tot_withEU <- 
            tmp_ex_tot 
         # %>%
         #    mutate( Reporter = ifelse( Reporter.ISO %in% concord_eu28$ISO3, "EU-28", Reporter ) ) %>%
         #    mutate( Reporter.ISO = ifelse( Reporter == "EU-28", "EU2" , Reporter.ISO )  ) %>%
         #    mutate( Reporter.Code = ifelse( Reporter == "EU-28", 97 , Reporter.Code )   ) %>%
         #    mutate( Partner = ifelse( Reporter == "EU-28", "World" , Partner )   ) %>%
         #    mutate( Partner.ISO = ifelse( Reporter == "EU-28", "WLD" , Partner.ISO )   ) %>%
         #    group_by(  Year, Reporter,Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity ,Trade.Flow, Qty.Unit ) %>%
         #    summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
         #                `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
         #    ) %>%
         #    ungroup 
      }
      
      
      ## 5. get eu export to world
      tmp_ex_eu_extra <-
         tmp_ex_eu_extra_raw %>%
         #dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO,Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  )
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO,Commodity.Code, Commodity, Alt.Qty.Unit, `Trade.Value..US..`  )
      
      ## 6. Split EU exporto WLD and Intra-EU
      tmp_ex_eu_all <- 
         tmp_ex_tot_withEU %>%
         filter( Reporter == "EU-28" )
      
      ## 7. split into eu intra trade and extra trade
      tmp_ex_eu_intra <- 
         tmp_ex_eu_all %>%
         left_join( tmp_ex_eu_extra, 
                    #by = c("Year", "Reporter", "Reporter.ISO", "Reporter.Code", "Partner", "Partner.ISO", "Trade.Flow", "Qty.Unit", "Commodity.Code", "Commodity" )
                    by = c("Year", "Reporter", "Reporter.ISO", "Reporter.Code", "Partner", "Partner.ISO", "Trade.Flow", "Commodity.Code", "Commodity" )
         ) %>%
         mutate( `Alt.Qty.Unit` = Alt.Qty.Unit.x - Alt.Qty.Unit.y, 
                 `Trade.Value..US..` =  `Trade.Value..US...x` - `Trade.Value..US...y` ) %>%
         dplyr::select( -Alt.Qty.Unit.x, -Alt.Qty.Unit.y, 
                        -`Trade.Value..US...x`,  -`Trade.Value..US...y`) %>%
         mutate( Partner = "EU-28", 
                 Partner.ISO = "EU2")
      
      ## 8. Eu export now splited into Extra-EU (to the world) and Intra-EU (within EU)
      tmp_ex_eu_split <- 
         tmp_ex_eu_intra %>%
         bind_rows( tmp_ex_eu_extra )
      
      
      ## 9. Replace the Eu total export by the EU trade splited
      tmp_ex_tot_withEU %<>%
         filter( Reporter != 'EU-28' ) %>%
         bind_rows( tmp_ex_eu_split ) %>%
         mutate( Share = as.numeric(`Trade.Value..US..`)/ sum( as.numeric(`Trade.Value..US..`), na.rm= T ) ) %>%
         mutate( Share_noIntraEU = as.numeric(`Trade.Value..US..`)/ sum( as.numeric(`Trade.Value..US..`[Partner!='EU-28']), na.rm= T )  ) %>%
         mutate( Share_noIntraEU = ifelse( Partner=='EU-28', NA, Share_noIntraEU ) ) %>%
         arrange( -Share )
      
      
      ## 10. find out how many countries cover 90% share
      tmp_target_share <- 0
      tmp_country_i <- 0
      while( tmp_target_share <= global_coverage ){
         tmp_country_i <- tmp_country_i + 1
         tmp_target_share  <- sum(tmp_ex_tot_withEU[ which(tmp_ex_tot_withEU$Partner != 'EU-28'),]$Share_noIntraEU[1:tmp_country_i], na.rm=T)
      }
      
      tmp_target_exporter <- unique(tmp_ex_tot_withEU$Reporter[1:tmp_country_i])
      tmp_target_exporter_code <- unique(tmp_ex_tot_withEU$Reporter.Code[1:tmp_country_i])
      
      
      ## 11. loop to donwload
      print("-------------- Download Export data by key exporters -----------")
      tmp_list_ex_by_country <- NULL
      tmp_list_ex_by_country <- 
         lapply( as.character(tmp_target_exporter_code),
                 function(i_country_code) {
                    i_country <- unique(tmp_ex_tot_withEU$Reporter[which(tmp_ex_tot_withEU$Reporter.Code == i_country_code)] )
                    print( paste0("----- Extracting data for ", i_country , " -----") )
                    tmp_fail <- 
                       try(
                          tmp_raw_ex_country <-
                             #get.Comtrade( r = i_country_code , p = 'all' , rg = 2 , cc = cc , fmt = 'csv', ps = max_year)$data
                              m_ct_search( reporters = i_country, partners = 'All', trade_direction = c( "exports"), freq = "annual",
                                       commod_codes = cc,
                                       start_date = max_year,
                                       end_date = max_year )  %>%
                             dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner, partner_iso, qty_unit,  qty, trade_value_usd) %>%
                             rename( Year = year,
                                     `Commodity` = commodity ,
                                     `Commodity.Code` = commodity_code ,
                                     `Trade.Flow` = trade_flow,
                                     Reporter = reporter,
                                     `Reporter.ISO` =  reporter_iso,
                                     `Reporter.Code` = reporter_code,
                                     Partner = partner,
                                     `Partner.ISO` = partner_iso,
                                     `Qty.Unit` = qty_unit,
                                     `Alt.Qty.Unit` = qty,
                                     `Trade.Value..US..` = trade_value_usd )
                       )
                    
                    if( class(tmp_fail) == "try=error" ){
                       stop( paste0("Error occured when extracting data for ", i_country) )
                       next
                    }else if(class(tmp_fail) != "try=error" ) {
                       try(return(tmp_raw_ex_country))
                    }
                 })
      
      tmp_dtf_ex_by_country  <-
         do.call( rbind, tmp_list_ex_by_country  )
      
      ## 12. create a datafrme where EU28 interal trade is not counted
      tmp_dtf_ex_by_country_withEU <-
         tmp_dtf_ex_by_country %>%
         filter( Partner != 'World' ) %>%
         #dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity,  Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         mutate( Reporter = as.character(Reporter), 
                 Reporter.ISO = as.character(Reporter.ISO),
                 Partner = as.character( Partner ),
                 Partner.ISO = as.character( Partner.ISO )
         ) %>%
         mutate( Partner = ifelse( Partner.ISO %in% concord_eu28$ISO3, 'EU-28', Partner ),
                 Partner.ISO = ifelse( Partner == 'EU-28', "EU2" ,Partner.ISO )) %>%
         #group_by( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit ) %>%
         group_by( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity ) %>%
         summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
                     `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
         ) %>%
         ungroup %>%
         arrange(  -`Trade.Value..US..`, Reporter  )
      
      ## if selected to display internal EU trade
      try(
      if( eu_internal %in% c("Yes",'YES', "yes") ){
         tmp_dtf_ex_by_country_withEU %<>%
            bind_rows(tmp_ex_eu_intra ) %>%
            arrange(  -`Trade.Value..US..`, Reporter  ) #### adding EU28 internal trade
      }else{
         next;
      })
      
      ## 13. create data for sankey plot 
      tmp_dtf_plot_sankey <- 
         tmp_dtf_ex_by_country_withEU %>%
         filter( Partner != 'World' ) %>%
         #dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner, Commodity.Code, Commodity,  Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         group_by( Year, Trade.Flow, Reporter, Commodity.Code  ) %>%
         arrange( -`Trade.Value..US..` ) %>%
         ungroup %>%
         group_by(Year ,Trade.Flow, Commodity.Code ) %>%
         #mutate( Weight = `Trade.Value..US..` / sum( `Trade.Value..US..`, na.rm=T  )  ) %>%
         mutate( Weight = `Trade.Value..US..` / tmp_tot_ex_value  ) %>%
         ungroup %>%
         mutate( Price = `Trade.Value..US..` / Alt.Qty.Unit ) %>%
         filter( Weight >= trade_weight * tmp_target_share )
      
      
      ## 14. use Sankey plot to visulize
      tmp_nz_partner <- tmp_dtf_plot_sankey$Partner[tmp_dtf_plot_sankey$Reporter== center_country ]
      
      tmp_nodes <- 
         data.frame( name = unique(c( unique(tmp_dtf_plot_sankey$Reporter),
                                      unique(tmp_dtf_plot_sankey$Partner) ) ) 
         )
      
      tmp_nodes %<>%
         mutate( id = 0: (nrow(tmp_nodes)-1) ) %>%
         mutate( IS_NZ_node = ifelse( name %in% c(center_country,tmp_nz_partner),'red',NA ) )%>%
         mutate( IS_ex = ifelse( name %in% tmp_dtf_plot_sankey$Reporter, 'Exporter', 'No' ) ) %>%
         mutate( IS_im = ifelse( name %in% tmp_dtf_plot_sankey$Partner, 'Importer', 'No' ) ) %>%
         mutate( IS_ex_im_both = ifelse( IS_ex == 'Exporter' & IS_im == 'Importer', 'Both', IS_ex ) ) %>%
         mutate( IS_ex_im_both = gsub( 'No', 'Importer', IS_ex_im_both)  )
      
      tmp_links <-
         tmp_dtf_plot_sankey %>%
         dplyr::select( source = Reporter, 
                        target = Partner, 
                        value =  Weight #`Trade.Value..US..`
         ) %>%
         #mutate( value = round(value/10^6) ) %>%
         mutate( value = round(value*100,2) ) %>%
         left_join( tmp_nodes,
                    by = c('source' = 'name') ) %>%
         mutate( source = id) %>%
         dplyr::select( -id ) %>%
         left_join( tmp_nodes,
                    by = c('target' = 'name') ) %>%
         mutate( target = id) %>%
         dplyr::select( -id ) %>%
         mutate(IS_NZ_link = NA)
      
      ## hightlight links when NZ is an exporter
      if( length(tmp_nz_partner)!=0 ){
         tmp_links %<>%
            mutate( IS_NZ_link = ifelse( source == tmp_nodes$id[tmp_nodes$name == center_country ], 
                                         'red',  
                                         NA
            ) ) 
      }
      
      ## sankey plot here
      tmp_sankey_color <- 'd3.scaleOrdinal() .domain(["Exporter", "Importer","Both"]) .range(["#97D700", "#CD5B45" , "#FBE122"])'
      sankeyNetwork(Links = tmp_links, Nodes = tmp_nodes, Source = 'source',
                    Target = 'target', Value = 'value', NodeID = 'name',
                    LinkGroup = 'IS_NZ_link', 
                    NodeGroup = "IS_ex_im_both",
                    units = '%', fontSize = 20, nodeWidth = 30,
                    nodePadding = 15,
                    colourScale=tmp_sankey_color)
   }


### get sankey data funtion ----
get_data_sankey_uncomtrade <- 
   function( cc = '0409',
             global_coverage = 0.9,
             trade_weight = 0.005,
             max_year = 2016,
             center_country = "New Zealand",
             eu_internal  = "No" )
   {
      
      ## 0. work on cc
      if( is.null(cc)|cc==''|is.na(cc) ){
         stop('Commodity code (cc) cannot be empty!')
      }
      
      if( length(cc) > 1 ){
         stop('The current function can only handle ONE HS code at a time.')
      }
      
      # if( length(eu_internal) == 0 ){
      #    eu_internal  = "No"
      # }
      
      ## 1. download world export data
      print("-------------- Download Export data -----------")
      
      tmp_ex_tot_raw_try <- try(
         tmp_ex_tot_raw <-
            #get.Comtrade( r = 'all', p ='0' , rg = 2 , cc = cc , fmt = 'csv', ps = max_year)$data
            m_ct_search( reporters = "All", partners = 'World', trade_direction = c("exports"), freq = "annual",
                         commod_codes = cc,
                         start_date = max_year,
                         end_date = max_year ) %>%
            dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner,  partner_iso, qty_unit,  qty, trade_value_usd) %>%
            rename( Year = year,
                    `Commodity` = commodity ,
                    `Commodity.Code` = commodity_code ,
                    `Trade.Flow` = trade_flow,
                    Reporter = reporter,
                    `Reporter.ISO` =  reporter_iso,
                    `Reporter.Code` = reporter_code,
                    Partner = partner,
                    `Partner.ISO` = partner_iso,
                    `Qty.Unit` = qty_unit,
                    `Alt.Qty.Unit` = qty,
                    `Trade.Value..US..` = trade_value_usd )
         
         # tmp_ex_tot_raw <- 
         #    #get.Comtrade( r = 'all', p ='0' , rg = 2 , cc = cc , fmt = 'csv', ps = max_year)$data
         #    value(future({ 
         #       m_ct_search( reporters = "All", partners = 'World', trade_direction = c("exports"), freq = "annual",
         #                    commod_codes = cc,
         #                    start_date = max_year,
         #                    end_date = max_year )
         #       }) )%>%
         #    dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner,  partner_iso, qty_unit,  qty, trade_value_usd) %>%
         #    rename( Year = year,
         #            `Commodity` = commodity ,
         #            `Commodity.Code` = commodity_code ,
         #            `Trade.Flow` = trade_flow,
         #            Reporter = reporter,
         #            `Reporter.ISO` =  reporter_iso,
         #            `Reporter.Code` = reporter_code,
         #            Partner = partner,
         #            `Partner.ISO` = partner_iso,
         #            `Qty.Unit` = qty_unit,
         #            `Alt.Qty.Unit` = qty,
         #            `Trade.Value..US..` = trade_value_usd )
      )
      
      ## meesage
      if( class(tmp_ex_tot_raw_try) == 'try-error' ){
         print("-------------- Download Export data ERROR!! -----------")
         stop("Cannot download data from UN Com Trade!")
      }else{
         print("-------------- FINISH Download Export data -----------")
      }
      
      ## 2. download eu export data
      print("-------------- Download EU28 Export data -----------")
      
      tmp_ex_eu_extra_raw_try <- try(
         tmp_ex_eu_extra_raw <-
            #get.Comtrade( r = '97', p ='0' , rg = 2 , cc = cc , fmt = 'csv', ps = max_year)$data
            m_ct_search( reporters = "EU-28", partners = 'World', trade_direction = c( "exports"), freq = "annual",
                         commod_codes = cc,
                         start_date = max_year,
                         end_date = max_year ) %>%
            dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner, partner_iso, qty_unit,  qty, trade_value_usd) %>%
            rename( Year = year,
                    `Commodity` = commodity ,
                    `Commodity.Code` = commodity_code ,
                    `Trade.Flow` = trade_flow,
                    Reporter = reporter,
                    `Reporter.ISO` =  reporter_iso,
                    `Reporter.Code` = reporter_code,
                    Partner = partner,
                    `Partner.ISO` = partner_iso,
                    `Qty.Unit` = qty_unit,
                    `Alt.Qty.Unit` = qty,
                    `Trade.Value..US..` = trade_value_usd )
         
         # tmp_ex_eu_extra_raw <- 
         #    value(future({
         #       m_ct_search( reporters = "EU-28", partners = 'World', trade_direction = c( "exports"), freq = "annual",
         #                    commod_codes = cc,
         #                    start_date = max_year,
         #                    end_date = max_year )
         #    }) ) %>%
         #    dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner, partner_iso, qty_unit,  qty, trade_value_usd) %>%
         #    rename( Year = year,
         #            `Commodity` = commodity ,
         #            `Commodity.Code` = commodity_code ,
         #            `Trade.Flow` = trade_flow,
         #            Reporter = reporter,
         #            `Reporter.ISO` =  reporter_iso,
         #            `Reporter.Code` = reporter_code,
         #            Partner = partner,
         #            `Partner.ISO` = partner_iso,
         #            `Qty.Unit` = qty_unit,
         #            `Alt.Qty.Unit` = qty,
         #            `Trade.Value..US..` = trade_value_usd )
      )
      
      ## meesage
      if( class(tmp_ex_eu_extra_raw_try ) == 'try-error' ){
         print("-------------- Download EU28 Export data ERROR!! -----------")
         stop("Cannot download data from UN Com Trade!")
      }else{
         print("-------------- FINISH Download EU28 Export data -----------")
      }
      
      ## if no EU data
      if( #!tmp_ex_eu_extra_raw$Classification %in% c('H1',"H2", "H4","H6")
         is.na(tmp_ex_eu_extra_raw$`Trade.Value..US..`) ){
         print("------------- EU28 export data NOT available ----------------")
         tmp_ex_eu_extra_raw$Year <- unique(tmp_ex_tot_raw$Year )
         tmp_ex_eu_extra_raw$Trade.Flow <- unique(tmp_ex_tot_raw$Trade.Flow)
         tmp_ex_eu_extra_raw$Reporter <- "EU-28"
         tmp_ex_eu_extra_raw$Reporter.ISO <- "EU2"
         tmp_ex_eu_extra_raw$Reporter.Code <- 97
         tmp_ex_eu_extra_raw$Partner <- "World"
         tmp_ex_eu_extra_raw$Partner.ISO <- "WLD"
         tmp_ex_eu_extra_raw$Commodity.Code <- unique(tmp_ex_tot_raw$Commodity.Code)
         tmp_ex_eu_extra_raw$Commodity <- unique(tmp_ex_tot_raw$Commodity)[1]
         tmp_ex_eu_extra_raw$Qty.Unit <- unique(tmp_ex_tot_raw$Qty.Unit)[1]
      }
      
      ## 3. all countries export data 
      tmp_ex_tot <-
         tmp_ex_tot_raw %>%
         #dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         mutate( Reporter = as.character(Reporter), 
                 Reporter.ISO = as.character(Reporter.ISO),
                 Partner = as.character( Partner ),
                 Partner.ISO = as.character( Partner.ISO )
         )
      
      ## 3.1 get total export value and quantity
      tmp_tot_ex_value <- sum( as.numeric( tmp_ex_tot$`Trade.Value..US..`), na.rm=T )
      tmp_tot_ex_qty <- sum(  as.numeric(tmp_ex_tot$Alt.Qty.Unit), na.rm=T )
      
      ## 4. put all EU28 countries together and later no there will be EU28 internal trade shown
      if( #tmp_ex_eu_extra_raw$Classification %in% c('H1',"H2", "H4","H6")
         !is.na(tmp_ex_eu_extra_raw$`Trade.Value..US..`)  ){
         tmp_ex_tot_withEU <- 
            tmp_ex_tot %>%
            mutate( Reporter = ifelse( Reporter.ISO %in% concord_eu28$ISO3, "EU-28", Reporter ) ) %>%
            mutate( Reporter.ISO = ifelse( Reporter == "EU-28", "EU2" , Reporter.ISO )  ) %>%
            mutate( Reporter.Code = ifelse( Reporter == "EU-28", 97 , Reporter.Code )   ) %>%
            mutate( Partner = ifelse( Reporter == "EU-28", "World" , Partner )   ) %>%
            mutate( Partner.ISO = ifelse( Reporter == "EU-28", "WLD" , Partner.ISO )   ) %>%
            #group_by(  Year, Reporter,Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity ,Trade.Flow, Qty.Unit ) %>%
            group_by(  Year, Reporter,Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity ,Trade.Flow  ) %>%
            summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
                        `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
            ) %>%
            ungroup 
      }else{
         tmp_ex_tot_withEU <- 
            tmp_ex_tot 
         # %>%
         #    mutate( Reporter = ifelse( Reporter.ISO %in% concord_eu28$ISO3, "EU-28", Reporter ) ) %>%
         #    mutate( Reporter.ISO = ifelse( Reporter == "EU-28", "EU2" , Reporter.ISO )  ) %>%
         #    mutate( Reporter.Code = ifelse( Reporter == "EU-28", 97 , Reporter.Code )   ) %>%
         #    mutate( Partner = ifelse( Reporter == "EU-28", "World" , Partner )   ) %>%
         #    mutate( Partner.ISO = ifelse( Reporter == "EU-28", "WLD" , Partner.ISO )   ) %>%
         #    group_by(  Year, Reporter,Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity ,Trade.Flow, Qty.Unit ) %>%
         #    summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
         #                `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
         #    ) %>%
         #    ungroup 
      }
      
      
      ## 5. get eu export to world
      tmp_ex_eu_extra <-
         tmp_ex_eu_extra_raw %>%
         #dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO,Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  )
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO,Commodity.Code, Commodity,  Alt.Qty.Unit, `Trade.Value..US..`  )
      
      ## 6. Split EU exporto WLD and Intra-EU
      tmp_ex_eu_all <- 
         tmp_ex_tot_withEU %>%
         filter( Reporter == "EU-28" )
      
      ## 7. split into eu intra trade and extra trade
      tmp_ex_eu_intra <- 
         tmp_ex_eu_all %>%
         left_join( tmp_ex_eu_extra, 
                    #by = c("Year", "Reporter", "Reporter.ISO", "Reporter.Code", "Partner", "Partner.ISO", "Trade.Flow", "Qty.Unit", "Commodity.Code", "Commodity" )
                    by = c("Year", "Reporter", "Reporter.ISO", "Reporter.Code", "Partner", "Partner.ISO", "Trade.Flow",  "Commodity.Code", "Commodity" )
         ) %>%
         mutate( `Alt.Qty.Unit` = Alt.Qty.Unit.x - Alt.Qty.Unit.y, 
                 `Trade.Value..US..` =  `Trade.Value..US...x` - `Trade.Value..US...y` ) %>%
         dplyr::select( -Alt.Qty.Unit.x, -Alt.Qty.Unit.y, 
                        -`Trade.Value..US...x`,  -`Trade.Value..US...y`) %>%
         mutate( Partner = "EU-28", 
                 Partner.ISO = "EU2")
      
      ## 8. Eu export now splited into Extra-EU (to the world) and Intra-EU (within EU)
      tmp_ex_eu_split <- 
         tmp_ex_eu_intra %>%
         bind_rows( tmp_ex_eu_extra )
      
      
      ## 9. Replace the Eu total export by the EU trade splited
      tmp_ex_tot_withEU %<>%
         filter( Reporter != 'EU-28' ) %>%
         bind_rows( tmp_ex_eu_split ) %>%
         mutate( Share = as.numeric(`Trade.Value..US..`)/ sum( as.numeric(`Trade.Value..US..`), na.rm= T ) ) %>%
         mutate( Share_noIntraEU = as.numeric(`Trade.Value..US..`)/ sum( as.numeric(`Trade.Value..US..`[Partner!='EU-28']), na.rm= T )  ) %>%
         mutate( Share_noIntraEU = ifelse( Partner=='EU-28', NA, Share_noIntraEU ) ) %>%
         arrange( -Share )
      
      
      ## 10. find out how many countries cover 90% share
      tmp_target_share <- 0
      tmp_country_i <- 0
      while( tmp_target_share <= global_coverage ){
         tmp_country_i <- tmp_country_i + 1
         tmp_target_share  <- sum(tmp_ex_tot_withEU[ which(tmp_ex_tot_withEU$Partner != 'EU-28'),]$Share_noIntraEU[1:tmp_country_i], na.rm=T)
      }
      
      tmp_target_exporter <- unique(tmp_ex_tot_withEU$Reporter[1:tmp_country_i])
      tmp_target_exporter_code <- unique(tmp_ex_tot_withEU$Reporter.Code[1:tmp_country_i])
      
      
      ## 11. loop to donwload
      print("-------------- Download Export data by key exporters -----------")
      tmp_list_ex_by_country <- NULL
      tmp_list_ex_by_country <- 
         lapply( as.character(tmp_target_exporter_code),
                 function(i_country_code) {
                    i_country <- unique(tmp_ex_tot_withEU$Reporter[which(tmp_ex_tot_withEU$Reporter.Code == i_country_code)] )
                    print( paste0("----- Extracting data for ", i_country , " -----") )
                    tmp_fail <- 
                       try(
                          tmp_raw_ex_country <-
                             #get.Comtrade( r = i_country_code , p = 'all' , rg = 2 , cc = cc , fmt = 'csv', ps = max_year)$data
                             m_ct_search( reporters = i_country, partners = 'All', trade_direction = c( "exports"), freq = "annual",
                                          commod_codes = cc,
                                          start_date = max_year,
                                          end_date = max_year ) %>%
                             dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner, partner_iso, qty_unit,  qty, trade_value_usd) %>%
                             rename( Year = year,
                                     `Commodity` = commodity ,
                                     `Commodity.Code` = commodity_code ,
                                     `Trade.Flow` = trade_flow,
                                     Reporter = reporter,
                                     `Reporter.ISO` =  reporter_iso,
                                     `Reporter.Code` = reporter_code,
                                     Partner = partner,
                                     `Partner.ISO` = partner_iso,
                                     `Qty.Unit` = qty_unit,
                                     `Alt.Qty.Unit` = qty,
                                     `Trade.Value..US..` = trade_value_usd )
                          
                          # tmp_raw_ex_country <-
                          #    value(future({
                          #       m_ct_search( reporters = i_country, partners = 'All', trade_direction = c( "exports"), freq = "annual",
                          #                    commod_codes = cc,
                          #                    start_date = max_year,
                          #                    end_date = max_year ) 
                          #    }) ) %>%
                          #    dplyr::select( year, commodity, commodity_code, trade_flow, reporter, reporter_iso, reporter_code, partner, partner_iso, qty_unit,  qty, trade_value_usd) %>%
                          #    rename( Year = year,
                          #            `Commodity` = commodity ,
                          #            `Commodity.Code` = commodity_code ,
                          #            `Trade.Flow` = trade_flow,
                          #            Reporter = reporter,
                          #            `Reporter.ISO` =  reporter_iso,
                          #            `Reporter.Code` = reporter_code,
                          #            Partner = partner,
                          #            `Partner.ISO` = partner_iso,
                          #            `Qty.Unit` = qty_unit,
                          #            `Alt.Qty.Unit` = qty,
                          #            `Trade.Value..US..` = trade_value_usd )
                       )
                    
                    if( class(tmp_fail) == "try=error" ){
                       stop( paste0("Error occured when extracting data for ", i_country) )
                       next
                    }else if(class(tmp_fail) != "try=error" ) {
                       try(return(tmp_raw_ex_country))
                    }
                 })
      
      tmp_dtf_ex_by_country  <-
         do.call( rbind, tmp_list_ex_by_country  )
      
      ## 12. create a datafrme where EU28 interal trade is (not) counted
      tmp_dtf_ex_by_country_withEU <-
         tmp_dtf_ex_by_country %>%
         filter( Partner != 'World' ) %>%
         #dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity,  Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         mutate( Reporter = as.character(Reporter), 
                 Reporter.ISO = as.character(Reporter.ISO),
                 Partner = as.character( Partner ),
                 Partner.ISO = as.character( Partner.ISO )
         ) %>%
         mutate( Partner = ifelse( Partner.ISO %in% concord_eu28$ISO3, 'EU-28', Partner ),
                 Partner.ISO = ifelse( Partner == 'EU-28', "EU2" ,Partner.ISO )) %>%
         #group_by( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity, Qty.Unit ) %>%
         group_by( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner,Partner.ISO, Commodity.Code, Commodity ) %>%
         summarise(  Alt.Qty.Unit = sum( as.numeric(Alt.Qty.Unit), na.rm=T),
                     `Trade.Value..US..` = sum(  as.numeric(`Trade.Value..US..`), na.rm = T )
         ) %>%
         ungroup %>%
         arrange(  -`Trade.Value..US..`, Reporter  )
      
      ## if selected to display internal EU trade
      try(
      if( eu_internal %in% c("Yes",'YES', "yes") ){
         tmp_dtf_ex_by_country_withEU %<>%
            bind_rows(tmp_ex_eu_intra ) %>%
            arrange(  -`Trade.Value..US..`, Reporter  ) #### adding EU28 internal trade
      }else{
         next;
      })
      
      ## 13. create data for sankey plot 
      tmp_dtf_plot_sankey <- 
         tmp_dtf_ex_by_country_withEU %>%
         filter( Partner != 'World' ) %>%
         #dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner, Commodity.Code, Commodity, Qty.Unit, Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         dplyr::select( Year ,Trade.Flow, Reporter, Reporter.ISO,Reporter.Code, Partner, Commodity.Code, Commodity,  Alt.Qty.Unit, `Trade.Value..US..`  ) %>%
         group_by( Year, Trade.Flow, Reporter, Commodity.Code  ) %>%
         arrange( -`Trade.Value..US..` ) %>%
         ungroup %>%
         group_by(Year ,Trade.Flow, Commodity.Code ) %>%
         #mutate( Weight = `Trade.Value..US..` / sum( `Trade.Value..US..`, na.rm=T  )  ) %>%
         mutate( Weight = `Trade.Value..US..` / tmp_tot_ex_value  ) %>%
         ungroup %>%
         mutate( Price = `Trade.Value..US..` / Alt.Qty.Unit ) %>%
         filter( Weight >= trade_weight * tmp_target_share )

      
      ## 14. use Sankey plot to visulize
      tmp_nz_partner <- tmp_dtf_plot_sankey$Partner[tmp_dtf_plot_sankey$Reporter== center_country ]
      
      tmp_nodes <- 
         data.frame( name = unique(c( unique(tmp_dtf_plot_sankey$Reporter),
                                      unique(tmp_dtf_plot_sankey$Partner) ) ) 
         )
      
      tmp_nodes %<>%
         mutate( id = 0: (nrow(tmp_nodes)-1) ) %>%
         mutate( IS_NZ_node = ifelse( name %in% c(center_country,tmp_nz_partner),'red',NA ) ) %>%
         mutate( IS_ex = ifelse( name %in% tmp_dtf_plot_sankey$Reporter, 'Exporter', 'No' ) ) %>%
         mutate( IS_im = ifelse( name %in% tmp_dtf_plot_sankey$Partner, 'Importer', 'No' ) ) %>%
         mutate( IS_ex_im_both = ifelse( IS_ex == 'Exporter' & IS_im == 'Importer', 'Both', IS_ex ) ) %>%
         mutate( IS_ex_im_both = gsub( 'No', 'Importer', IS_ex_im_both)  )
      
      tmp_links <-
         tmp_dtf_plot_sankey %>%
         dplyr::select( source = Reporter, 
                        target = Partner, 
                        value =  Weight #`Trade.Value..US..`
         ) %>%
         #mutate( value = round(value/10^6) ) %>%
         mutate( value = round(value*100,2) ) %>%
         left_join( tmp_nodes,
                    by = c('source' = 'name') ) %>%
         mutate( source = id) %>%
         dplyr::select( -id ) %>%
         left_join( tmp_nodes,
                    by = c('target' = 'name') ) %>%
         mutate( target = id) %>%
         dplyr::select( -id ) %>%
         mutate(IS_NZ_link = NA) 
      
      ## hightlight links when NZ is an exporter
      if( length(tmp_nz_partner)!=0 ){
         tmp_links %<>%
            mutate( IS_NZ_link = ifelse( source == tmp_nodes$id[tmp_nodes$name == center_country ], 
                                         'red',  
                                         NA
            ) ) 
      }
      
      #tmp_nz_in <- ifelse(length(tmp_nz_partner)!=0 , T, F)
      
      return(list(links = tmp_links, nodes = tmp_nodes))
   }



# Function to call in place of dropdownMenu --------------
customSentence <- function(numItems, type) {
   paste("Feedback & suggestions")
}

customSentence_share <- function(numItems, type) {
   paste("Love it? Share it!")
}

##
dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"), 
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence = customSentence) 
{
   type <- match.arg(type)
   if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
   items <- c(list(...), .list)
   lapply(items, shinydashboard:::tagAssert, type = "li")
   dropdownClass <- paste0("dropdown ", type, "-menu")
   if (is.null(icon)) {
      icon <- switch(type, messages = shiny::icon("envelope"), 
                     notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
   }
   numItems <- length(items)
   if (is.null(badgeStatus)) {
      badge <- NULL
   }
   else {
      badge <- tags$span(class = paste0("label label-", badgeStatus), 
                    numItems)
   }
   tags$li(
      class = dropdownClass, 
      a(
         href = "#", 
         class = "dropdown-toggle", 
         `data-toggle` = "dropdown", 
         icon, 
         badge
      ), 
      tags$ul(
         class = "dropdown-menu", 
         tags$li(
            class = "header", 
            customSentence(numItems, type)
         ), 
         tags$li(
            tags$ul(class = "menu", items)
         )
      )
   )
}