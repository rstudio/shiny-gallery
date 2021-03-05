# One Source Indy App
# This R program uses social services data to create an app
# intended to improve community resource utilization and the health of community members
# Users specify options of interest at the start of the app to get what he or she wants.
# Purpose: Use social services data to create one-stop-shop community resource app 
# Significance: improve community resource utilization and the health of community members through easy to use interface
# Limitations and Warnings: 
# 1) Data is incomplete
# Created on: 2019-02-24
# Updated on:  2019-07-31
# Created by: Sam Parmar (https://www.github.com/parmsam)
# Program Flow Description (high level review of the steps of the program)
# 1) 
# X)

#load libraries ---
#source("/Users/Sam/GithubR_Repo/checkload.r")
#setwd("/Users/Sam/Documents/One Source Indy/")

#https://github.com/parmsam/Improving_with_R/blob/master/checkload.r
# checkload<-function(list.of.packages) {
#   new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#   if(length(new.packages)) {
#     install.packages(new.packages)
#   }
#   lapply(list.of.packages, require, character.only = TRUE)
# }
# list<-c("shiny","DT","tidyverse","rio","leaflet","htmltools","shinydashboard","flexdashboard")
# checkload(list)
library(shiny)
library(DT)
library(dplyr)
library(rio)
library(leaflet)
library(htmltools)
#library(shinydashboard)
library(flexdashboard)
library(stringr)



resource_categ <- c("Clothing Pantry",
  "Food Pantry",
  "Healthcare",
  "Wifi",
  "Meals",
  "Employment",
  "Community Centers",
  "Mental Health or Substance Use",
  "Emergency Shelter",
  "School",
  "Domestic Violence",
  "Veterans",
  "Ex-offender Reentry",
  "Gov. Aid",
  "HIV and STDs",
  "Legal",
  "Multi-Service")


resource_images <- c(
  "ComunityEvents.png",
  "Meals.png",
  "Healthcare.png",
  "TechnologyWifi.png",
  "Meals.png",
  "ComunityEvents.png",
  "Shelter.png",
  "Healthcare.png",
  "Shelter.png",
  "Shelter.png",
  "ComunityEvents.png",
  "ComunityEvents.png",
  "ComunityEvents.png",
  "Shelter.png",
  "Healthcare.png",
  "ComunityEvents.png",
  "Shelter.png"
  )

# Data pre-processing ----
#Dat<-import("/Users/Sam/Documents/One Source Indy/Descriptions_Table.xlsx") %>% select(-long,-lat)
Dat<-import("resource_data_chipindy.xlsx") %>% select(-long,-lat)
Full_adds<-import("full_addresses.xlsx") %>% 
  select(-c(business_desc,contact_phone,contact_email,contact_website,wheelchair_access))
# create google map link based on google api 
# (https://developers.google.com/maps/documentation/urls/guide)
Dat <- Dat %>% mutate(Google_map=paste0("https://www.google.com/maps/search/?api=1&query=",str_replace(str_replace(Resource_name, "&","and"), " ", "+"))) 
#Dat <- Dat %>% mutate(Google_map=paste0("https://www.google.com/maps/search/?api=1&query=",Resource_name))
Dat <- Dat %>% left_join(Full_adds,by=c("Resource_name"="business_name")) %>% mutate(All="Y")

Dat2 <- import("event_data.xlsx")
#Dat %>% filter(get(c("Clothing","Shelter")) %in% "Y") %>% View()

# Define UI for One Source app ----
ui <- fluidPage(
    navbarPage("One Source Indy", id="inTabset",
    #landing page
    tabPanel("Welcome", 
    div(h3(tags$i("Community Resource Guide")), style="vertical-align: middle; text-align:center;"),
    HTML('<center><img src="https://www.publicdomainpictures.net/pictures/300000/velka/homeless-people.jpg" width="600"></center>'),
    # tags$button(
    #   id = "do",
    #   class = "btn action-button",
    #   tags$img(src = "https://www.publicdomainpictures.net/pictures/300000/velka/homeless-people.jpg",
    #       width = "600"),
    #   style="text-align:center"
    # ),
    tags$br(),
    div(tags$b("Welcome"), style="text-align:center "),
    div(actionButton("do",tags$em( "Enter Here")), style="text-align:center "),
    tags$br(),
    # img(src = "ComunityEvents_white.png", width="60", style="display: block; margin-left: auto; margin-right: auto;"),
    # img(src = "Healthcare_white.png", width="60", style="display: block; margin-left: auto; margin-right: auto;"),
    # img(src = "Meals_white.png", width="60", style="display: block; margin-left: auto; margin-right: auto;"),
    # img(src = "Shelter_white.png", width="60", style="display: block; margin-left: auto; margin-right: auto;"),
    # img(src = "TechnologyWifi_white.png", width="60", style="display: block; margin-left: auto; margin-right: auto;"),
    # img(src = "Transport.png", width="60", style="display: block; margin-left: auto; margin-right: auto;"),
    tags$hr(),
    h5(tags$i("Created with")),
    tags$img(src = "https://www.rstudio.com/wp-content/uploads/2016/09/RStudio-Logo-Blue-Gray-250.png", width = "100px")
    ),       
    #first tab called Application tab
    tabPanel(title="Application", value="panel1",
    # App title ----
    titlePanel(h3(tags$a("One Source Indy",href="https://www.onesourceindy.org"))),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        icon("fas fa-globe americas"),"Quick Info and Updates: ", br(),
        tags$li("One Source Indy is a Community Resource Tool for Homeless or Unstably Housed Individuals living in Indianapolis."),
        tags$li("This is a volunteer project that uses Indianapolis community resource data. All data is provisional and based on 2019 Community Resource Data."),
        tags$li("There are currently",tags$u(length(unique(Dat$Resource_name))),"unique resources in this app."),
        tags$li("As an alternative, please use the free", tags$a("IN 211 Resource Tool",href="https://in211.communityos.org/"),"to connect with resources across all of Indiana or dial 2-1-1 to connect with an IN 211 Community Navigator." ),
       
        #tags$li("Map available below"),
        br(),
        icon("fas fa-question-circle"),"Step 1: ",
        # Input 1: Selector for variable to plot against category of interest ----
        radioButtons("variable_1", "What resource are you looking for?",
                           choiceNames = mapply(resource_categ, resource_images, FUN = function(name, imageURL) {
                             tagList(
                               tags$img(src=imageURL, width=30, height=30),
                               name
                             )
                           }, SIMPLIFY = FALSE, USE.NAMES = FALSE),
                           choiceValues = resource_categ
                           #choices =c()
                           ),
        # checkboxGroupInput("variable_1", "Select what kinds of resource you are looking for?",
        #                choices=c(
        #                          "Clothing"= "Clothing",
        #                          "Food Pantry" = "Food Pantry",
        #                          "Health Services"= "Health Services",
        #                          "Public Wifi" = "Public Wifi",
        #                          "Shelter" = "Shelter",
        #                          "Technology"= "Technology",
        #                          "Transportation"= "Transportation"
        #                          #"All"="All"
        #                ),selected="All"
        # ),
        # selectizeInput("variable_1", "Select what kinds of resource you are looking for?",
        #             choices=c("Choose one"="",
        #               "Public Wifi" = "Public Wifi",
        #               "Shelter" = "Shelter",
        #                "Food Pantry" = "Food Pantry",
        #                "Clothing"= "Clothing",
        #                "Transportation"= "Transportation",
        #                "Health Services"= "Health Services",
        #                "Technology"= "Technology",
        #                 "All"="All"
        #               ),selected="All",
        #             multiple=FALSE,
        #             options = list(
        #               placeholder = 'Choose one or more')
        #             ),
      icon("fas fa-question-circle"),"Step 2: ", 
      # Input 2: Selector for variable to select column ----
      checkboxGroupInput("variable_2", label= "What info would you like?",
                     choices=c(
                              #"Google Map"="Google_map",
                               "Address",
                               "Email",
                               "Fees",
                               "Eligibility",
                               "Hours",
                               "Phone Number",
                               "Website" = "Website"
                               
                               # "Lat"="lat",
                               # "Lon"="lon"
                     ),
                     selected=c("lat","lon","Google_map")
      ),
      # selectizeInput("variable_2", "Select what info you would like?",
      #               choices=c("Choose one"="",
      #                 "Website" = "Website",
      #                 "Phone Number",
      #                 "Email",
      #                 "Google Map"="Google_map"
      #                 # "Lat"="lat",
      #                 # "Lon"="lon"
      #                 ),
      #               selected=c("lat","lon","Google_map"),
      #               multiple = TRUE,
      #               options = list(
      #               placeholder = 'Choose one or more')
      #               ),
      # Input 3: Selector for Resource name variable to filter by ----
      icon( "fas fa-globe americas" ),"Search ", 
      selectInput("variable_3", "Optional: Pick a single resource you want more info on.",
                   #choices=list("",name = c("Cho"="",unique(Dat$Resource_name))),
                   choices=c("Choose one"="","All",sort(unique(Dat$Resource_name))),
                   #options = list(placeholder = 'Please select an option below'),
                  selected = "All"
                  #verbatimTextOutput("selected")
      )
      #, Input: Checkbox for whether outliers should be included ---
      #,checkboxInput("outliers", "", TRUE)
      ),
      # Main panel for displaying outputs ----
        mainPanel(position="left",
        # Output: Formatted text for caption ----
        h3(textOutput("caption"),style = "color: #7eaed3;"),
        # Output: Plot of the requested variable against mpg ----
        #plotOutput("mpgPlot")
        # Output: data table ---
        #tableOutput("data")
        DT::dataTableOutput("data"),
        p(),
        h3("Map for Directions",
           style = "color: #a1bb96;"),
        leafletOutput("mymap")
        ))
    ),
    #second tab called about tab
    tabPanel("Events",   # Information on project,
             h4("Real-time info on upcoming events will go here"),
             DT::dataTableOutput("data_events")
             ),
    #second tab called about tab
      tabPanel("About",   # Information on project,
               h4("About this app:"),
               div(
                 tags$p("This R shiny app uses Indianapolis community resource data to create an open source app to better inform in-need homeless or unstably-housed individuals 
                        living in Indianapolis on resources available in their community.",style = "color:black"),
                 tags$p("The goal was to better inform homeless or unstably housed individuals in-need living in Indianapolis on what is available in our communities. Also, the prototype 
                 was created to show how publicly available resource data can be used with R shiny, to potentially collaborate with Indianapolis homeless outreach organizations, and to encourage others to develop similar applications for social 
                        good."),
                 tags$p("This app holds info on the following:"),
                 tags$ul(
                   tags$li("Shelters"),
                   tags$li("Local food banks"),
                   tags$li("Local medical service providers"),
                   tags$li("Mental health services"),
                   tags$li("Public libraries"),
                   tags$li("And many other local resources")
                 ),
                 tags$p("Use the box on the left of the 'application' tab to choose what is shown. info can be selected by following the steps."),
               #p("Please visit", tags$a("https://www.onesourceindy.org/", href="https://www.onesourceindy.org/"),
                 #"for more info. All information is provisional."),
               hr(),
               h4("Data Sources and Source Code:"),
               p("Data was taken from an assessment and webscraping of community resource information."#,
                 #tags$a("One Source Indy.",href="https://www.onesourceindy.org")
                 ),
               tags$p("The following public resources were consulted:"),
               tags$ul(
                 tags$li(tags$a("CHIP 2019 Handbook of Help",href="https://www.chipindy.org/"))#,
                 #tags$li(tags$a("FSSA DDRS Community Resource Guide",
                #         href="https://www.in.gov/fssa/files/Indianapolis%20Lynhurst%20Area%20Office%20Resource%20Guide.pdf"))
               ), 
               p("All data is provisional and based on 2019 Community Resource Data."),
               p("Source code available on my github:",tags$a("https://github.com/parmsam", href="https://github.com/parmsam")),
               hr(),
               h4("Team:"),
               #first author:
               tags$p("Sam Parmar " , tags$a("", href="mailto:")),
               #second author:
               tags$p("Derris Ross " , tags$a("", href="mailto:")),
               #tags$p("Sam Parmar, " , tags$a("", href="mailto:parmartsam@gmail.com"))
               #third author:
               tags$p("Sherri Huang " , tags$a("", href="mailto:")),
               tags$p("Joshua Elkins " , tags$a("", href="mailto:")),
               hr()
                 ))
))

# Define server logic  ----
server <- function(input, output, session) {
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  observeEvent(input$do, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
    
  })

  formulaText <- reactive({
    if(input$variable_3==""){
     #paste0("One Source1", " ~ ", "Step 1", " ~ ", "Step 2", " ~ ", "Step 3")
    paste0("Available Community Resources")
    }
    #else paste("One Source ~", input$variable_1, "~", input$variable_2, "~", input$variable_3)
    else #paste0("One Source", " ~ ", "Step 1", " ~ ", "Step 2", " ~ ", "Step 3")
      paste0("Available Community Resources")
  })
  # Return the formula text for printing the caption ----
  output$caption <- renderText({
    formulaText()
  })
  # Generate a table of the requested variable  ----
  # and only exclude outliers if requested
  
  filterData <- reactive({
    filtered<-Dat %>%  mutate(Website = paste0("<a href='", Website,"' target='_blank'>", Website,"</a>"),
                    Google_map=paste0("<a href='", Google_map,"' target='_blank'>", "Click Here For Google Directions","</a>"),
                    Email=paste0("<a href='mailto:", Email,"'target='_blank'>", Email,"</a>")) %>% 
      select(c("Resource_name","Required Items", "Description", "lat","lon", "Google_map", input$variable_2, input$variable_1)) %>%
    filter_all(any_vars(. == "Y"))
    #filter(get(input$variable_1) %in% "Y") %>% 
    #Dat1%>% filter(Resource_name==!!input$variable_3) 
    if (!is.null(input$variable_3) & !input$variable_3=="" &  !input$variable_3=="All") {
      #filtered <- filtered %>% filter(Resource_name == input$variable_3)
      filtered<-Dat %>%  mutate(Website = paste0("<a href='", Website,"' target='_blank'>", Website,"</a>"),
                                Google_map=paste0("<a href='", Google_map,"' target='_blank'>", 
                                                  "Click Here For Google Directions","</a>"),
                                Email=paste0("<a href='mailto:", Email,"'target='_blank'>", Email,"</a>")) %>% 
      select(c("Resource_name","Required Items", "Description", "lat","lon", "Google_map", input$variable_2)) %>%
      filter(Resource_name == input$variable_3)
    }
    filtered
  })
  
  output$data <- DT::renderDataTable({ 
   out<-filterData() %>% select(-"lat",-"lon")
   DT::datatable(out,list(#mode = "single",
                          #target = "cell", 
     pageLength=3),selection="none", escape = FALSE)
  })
  
  output$data_events <- DT::renderDataTable({ 
    out2<-Dat2
    DT::datatable(out2,list(#mode = "single",
                            #target = "cell", 
                            pageLength=3),selection="none",escape = FALSE)
  })
  
  Map_Dat<- reactive({
    if("lon" %in% colnames(filterData()) & "lat" %in% colnames(filterData())){
    filterData() %>% filter(!is.na(lon) & !is.na(lon))
    }
    else(
      data.frame(lon=-86.1581, lat=39.7684,Resource_name="Select Long and Lat from Step 2 for Map and Directions",
                 Google_map="https://www.google.com/maps/search/?api=1&query=Soldiers+and+Sailors+Monument+Indianapolis")
    )
  })
  
  output$leaf=renderUI({
    leafletOutput('myMap', width = "200%", height =100)#input$Height)
  })
  output$mymap <- renderLeaflet({
        leaflet(Map_Dat()) %>% setView(-86.1581, 39.7684, zoom = 10) %>% 
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(data=Map_Dat(),lng= ~lon,lat= ~lat, popup=~paste(Resource_name,"<br>",
          Google_map
          )
         )})
}

#popup=~htmlEscape(Resource_name)
#Need shinyApp function that uses ui object and server function ---
##we def'd to build Shiny app ---
shinyApp(ui,server)
