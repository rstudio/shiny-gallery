library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(ggridges)
library(treemap)
library(wordcloud2)
library(scales)
library(StatMeasures)
library(ggpubr)

source("read_data.R")
source("texts.R")
source("funs.R")

# UI -------------------------------
wb_icon <- fixedPanel(
  tags$a(img(src = "globe.png", style = "width: 32px; margin-bottom: 4px; "), href = "https://www.worldbank.org/en/home"),
  tags$a(img(src = "gfdrr_analytics.png", style = "width: 130px; "), href = "https://www.gfdrr.org/en"),
  left = 25, top = 85, width = "32px"
)

panel_icon <- fixedPanel(
  tags$a(img(src = "GitHub-Mark.png", style = "width: 32px; margin-bottom: 3px; "), href = "https://github.com/ccsuehara/bolivia_labor"),
  tags$a(img(src = "cloud-computing.png", style = "width: 32px; "),
         href = "https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/bases-de-datos-encuestas-sociales/"),
  right = 25, top = 85, width = "32px"
)

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tags$style(type = "text/css",
             "h1, h2, h3, h4 { text-align: center; }",
             "p { text-align: center; color: grey; }",
             "hr { margin-top: 2em; margin-bottom: 2em; }",
             "#children_madlib { color: white; }"),
  
  navbarPage("Living a life of labor in Bolivia",
             id = "main",
             collapsible = T, position = "fixed-top",
             
             # Tab panel: home -----------------
             tabPanel("Home",
                      wb_icon,
                      fluidRow(width = 12, hr(), hr(),
                               imageOutput("landing",
                                           width = "99%",
                                           height = "90%",
                                           click = "landing_cl"))),
             
             # Tab panel: children under 18 --------------------
             tabPanel("Children under 18",
                      value = "children", hr(), hr(),
                      
                      fluidRow(style = 'background-image: url("children.jpg"); background-size: cover;',
                               column(12, style = "padding: 80px 50px;", 
                                      fluidRow(
                                        column(8,
                                               offset = 2,
                                               h3(textOutput("children_madlib")))))),
                      hr(),
                      fluidRow(
                        column(3,
                               wb_icon
                               ),
                        column(6,
                               # offset = 3,
                               textOutput("children_intro"),
                               h3("Overview: children in school and at work"),
                               plotOutput("children_1"),
                               
                               hr(),
                               textOutput("children_t2"),
                               h3("Reasons for not enrolling in school"),
                               # d3tree2Output("children_edu3"),
                               plotOutput("children_edu4"),
                               hr(),
                               
                               textOutput("children_t5"),
                               p(""),
                               textOutput("children_t8"),
                               p(""),
                               textOutput("children_t9"),
                               p(""),
                               textOutput("children_t10"),
                               p(""),
                               textOutput("children_t11"),
                               h3("Socioeconomic determinants of children's work in relation to the household"),
                               selectInput("ses",
                                           label = "",
                                           choices = c("Sex" = "sex",
                                                       "Area (rural/urban)" = "area",
                                                       "Department" = "depto",
                                                       "Indigenous identity" = "indi",
                                                       "Household income" = "inc")),
                               plotOutput("children_ses1"),
                               plotOutput("children_ses2"),
                               hr(),
                               
                               textOutput("children_t4"),
                               h3("Average work hours and income"),
                               plotOutput("children_lfp1"),
                               plotOutput("children_lfp2")
                        ),
                        column(3,
                               panel_icon,
                               fixedPanel(
                                 actionButton("to_youth", label = "youth >"),
                                 right = 10, bottom = 10
                               ))
                      )),
             
             # Tab panel: youth 18-24 --------------------
             tabPanel("Youth 18-24",
                      value = "youth",hr(),hr(),
                      
                      fluidRow(
                        column(3,
                               wb_icon,
                               fixedPanel(
                                 actionButton("to_children", label = "< children"),
                                 left = 10, bottom = 10
                               )),
                        
                        column(6,
                               
                               # Youth: intro ----------------------
                               textOutput("youth_intro"),
                               h3("Youth overview"),
                               plotOutput("youth_overview"),
                               hr(),
                               
                               # Youth: education ------------------
                               fluidRow(style = 'background-image: url("education_bw.jpg"); background-size: cover;',
                                        column(12, style = "padding: 80px 50px;",
                                               h3("EDUCATION", style = "color: white;"))),
                               hr(),
                               textOutput("youth_edu_t1"),
                               p(""),
                               h3("Which socioeconomic characteristics affect youths' education the most?"),
                               plotOutput("youth_edu_imp"),
                               hr(),
                               textOutput("youth_edu_marital"),
                               p(""),
                               textOutput("youth_edu_internet"),
                               p(""),
                               textOutput("youth_edu_area"),
                               p(""),
                               textOutput("youth_edu_depto"),
                               p(""),
                               textOutput("youth_edu_indi"),
                               p(""),
                               textOutput("youth_edu_lang"),
                               p(""),
                               selectInput("youth_edu_ses",
                                           label = "What affects youths' education?",
                                           choices = c("Marital status" = "marital",
                                                       "Internet access" = "internet",
                                                       "Area (rural/urban)" = "area",
                                                       "Department" = "depto",
                                                       "Indigenous identity" = "indi",
                                                       "Primary language" = "lang")),
                               plotOutput("youth_edu1"),
                               hr(),
                               textOutput("youth_edu_t2"),
                               h3("Educational attainment"),
                               plotOutput("youth_edu2"),
                               hr(),
                               
                               # Youth: employment -------------------------
                               fluidRow(style = 'background-image: url("employment_bw.jpg"); background-size: cover;',
                                        column(12, style = "padding: 80px 50px;",
                                               h3("EMPLOYMENT", style = "color: white;"))),
                               hr(),
                               textOutput("youth_emp_overview"),
                               fluidRow(
                                 column(6,
                                        h4("Gender ratio of youths who are students"),
                                        plotOutput("youth_edu_sex")),
                                 column(6,
                                        h4("Gender ratio of youths doing paid or unpaid work"),
                                        plotOutput("youth_emp_sex"))
                               ),
                               hr(),
                               
                               textOutput("youth_emp_t1"),
                               p(""),
                               h3("Which socioeconomic characteristics affect youths' employment the most?"),
                               plotOutput("youth_emp_imp"),
                               hr(),
                               textOutput("youth_emp_stu"),
                               p(""),
                               textOutput("youth_emp_edu"),
                               p(""),
                               textOutput("youth_emp_marital"),
                               p(""),
                               textOutput("youth_emp_area"),
                               p(""),
                               selectInput("youth_emp_ses",
                                           label = "What affects youths' employment?",
                                           choices = c("Student status" = "stu",
                                                       "Education" = "edu",
                                                       "Marital status" = "marital",
                                                       "Area (rural/urban)" = "area")),
                               plotOutput("youth_emp1"),
                               hr(),
                               
                               # Youth: income -------------------------------
                               fluidRow(style = 'background-image: url("income_bw.jpg"); background-size: cover;',
                                        column(12, style = "padding: 80px 50px;",
                                               h3("INCOME", style = "color: white; text-shadow: 0px 0px 6px black;"))),
                               hr(),
                               
                               textOutput("youth_inc_t1"),
                               h3("Proportion of unpaid workers among all workers"),
                               plotOutput("youth_inc_paid"),
                               hr(),
                               
                               textOutput("youth_inc_t2"),
                               h3("Average monthly labor income by sex and age"),
                               plotOutput("youth_inc_inc"),
                               p("")),
                        
                        column(3,
                               panel_icon,
                               fixedPanel(
                                 actionButton("to_employment", label = "adults - entering the job market >"),
                                 right = 10, bottom = 10
                               ))
                      )),
             
             # Tab panel: adults 25-60 --------------------
             navbarMenu("Adults 25-60",
                        
                        # Entering the job market -------------------------
                        tabPanel("Entering the job market",
                                 value = "employment",hr(),hr(),
                                 
                                 fluidRow(
                                   column(3,
                                          wb_icon,
                                          fixedPanel(
                                            actionButton("to_youth2", label = "< youth"),
                                            left = 10, bottom = 10
                                          )),
                                   column(6,
                                          textOutput("lmarket_intro"),
                                          h3("Education overview of adult population"),
                                          textOutput("labor_t_1"),
                                          plotOutput("adult_educ"),
                                          hr(),
                                          
                                          fluidRow(style = 'background-image: url("street_food_1.jpeg"); background-size: cover;',
                                                   column(12, style = "padding: 80px 50px;",
                                                          h3("PARTICIPATION", style = "color: white;"))),
                                          hr(),
                                          h3("Adult population by employment status"),
                                          textOutput("labor_t_2"),
                                          plotOutput("wfl_labor"),
                                          hr(),
                                          
                                          textOutput("labor_t_4"),
                                          plotOutput("labor_hrs"),
                                          hr(),
                                          
                                          textOutput("labor_t_3"),
                                          plotOutput("labor_rf")
                                   ),
                                   column(3,
                                          panel_icon,
                                          fixedPanel(
                                            actionButton("to_pay", label = "paid/unpaid labor >"),
                                            right = 10, bottom = 10
                                          ))
                                 )),
                        
                        # Paid and unpaid labor ------------------------------
                        tabPanel("Paid and unpaid labor",
                                 value = "pay", hr(),hr(),
                                 
                                 fluidRow(
                                   column(3,
                                          wb_icon,
                                          fixedPanel(
                                            actionButton("to_employment2", label = "< entering the job market"),
                                            left = 10, bottom = 10
                                          )),
                                   
                                   column(6,
                                          
                                          textOutput("pay_intro"),
                                          h3("Paid and unpaid labor throughout the lifetime"),
                                          plotOutput("pay_age"),
                                          hr(),
                                          textOutput("pay_t1"),
                                          h3("Average income by age and sex"),
                                          plotOutput("pay_lab_inc1"),
                                          h3("Average income among paid workers by age and sex"),
                                          plotOutput("pay_lab_inc2"),
                                          hr(),
                                          textOutput("pay_t2"),
                                          h3("Economic contribution of unpaid labor on household level (unweighted)"),
                                          plotOutput("pay_worth"),
                                          hr(),
                                          textOutput("pay_t3"),
                                          h3("Aggregate contribution of unpaid labor per month (weighted)"),
                                          fluidRow(
                                            column(6,
                                                   p("men"),
                                                   h2(textOutput("pay_worth_tot1")),
                                                   p("bolivianos")),
                                            column(6,
                                                   p("women"),
                                                   h2(textOutput("pay_worth_tot2")),
                                                   p("bolivianos"))
                                          ),
                                          hr(),
                                          textOutput("pay_t4"),
                                          h3("Presence of unpaid workers by household labor income (unweighted)"),
                                          plotOutput("pay_ru1"),
                                          h3("Aggregate contribution of unpaid labor (weighted)"),
                                          fluidRow(
                                            column(6,
                                                   h2(textOutput("pay_ru2")),
                                                   p("of total rural household productivity")),
                                            column(6,
                                                   h2(textOutput("pay_ru3")),
                                                   p("of total urban household productivity"))
                                          ),
                                          hr(),
                                          textOutput("pay_t5"),
                                          h3("Unpaid workers by department and rural/urban area"),
                                          plotOutput("pay_depto1"),
                                          hr(),
                                          textOutput("pay_t6"),
                                          h3("Unpaid workers by marital status and sex"),
                                          plotOutput("pay_marital1"),
                                          hr(),
                                          textOutput("pay_t7"),
                                          h3("Unpaid labor by cellphone access and household income (unweighted)"),
                                          plotOutput("pay_cell1")
                                   ),
                                   column(3,
                                          panel_icon,
                                          fixedPanel(
                                            actionButton("to_neet", label = "neet population >"),
                                            right = 10, bottom = 10
                                          )))),
                        
                        # NEET -------------------------
                        tabPanel("NEET population",
                                 value = "neet",hr(),hr(),
                                 
                                 fluidRow(
                                   column(3,
                                          wb_icon,
                                          fixedPanel(
                                            actionButton("to_pay2", label = "< paid/unpaid labor"),
                                            left = 10, bottom = 10
                                          )),
                                   column(6,
                                          textOutput("neet_t1"),
                                          h3("Distribution of study-labor activities by age and gender"),
                                          plotOutput("neet_p1"),
                                          hr(),
                                          textOutput("neet_t2"),
                                          h3("Neet population aged 14-30, by gender"),
                                          plotOutput("neet_p2"),
                                          hr(),
                                          textOutput("neet_t3"),
                                          h3("Why the NEETs won't work"),
                                          plotOutput("neet_p3"),
                                          hr(),
                                          textOutput("neet_t4"),
                                          h3("Why the NEETs won't study"),
                                          plotOutput("neet_p4"),
                                          
                                          textOutput("neet_t5"),
                                          h3("Which socioeconomic characteristics are related to NEET?"),
                                          plotOutput("neet_p5"),
                                          
                                          selectInput("neet_vars",
                                                      label = "What affects the chances of NEET?",
                                                      choices = c("Marital status" = "marital",
                                                                  "Internet access" = "internet",
                                                                  "Area (rural/urban)" = "area",
                                                                  "Department" = "depto",
                                                                  "Indigenous identity" = "indi",
                                                                  "Primary language" = "lang")),
                                          plotOutput("neets_dec_graph")
                                          
                                   ),
                                   column(3,
                                          panel_icon,
                                          fixedPanel(
                                            actionButton("to_older", label = "older adults >"),
                                            right = 10, bottom = 10
                                          ))
                                 ))),
             
             # Tab panel: older adults 60+ --------------------
             tabPanel("Older adults 60+",
                      value = "older", hr(), hr(),
                      
                      fluidRow(
                        column(3,
                               wb_icon,
                               fixedPanel(
                                 actionButton("to_neet2", label = "< adults - neet population"),
                                 left = 10, bottom = 10
                               )),
                        column(6,
                               textOutput("older_t1"),
                               h3("Social protection income among older adults"),
                               plotOutput("older_p1"),
                               hr(),
                               
                               textOutput("older_t2"),
                               h3("Which socioeconomic characteristics affect older adults' work status the most?"),
                               plotOutput("older_p2"),
                               hr(),
                               h3("The impact of select socioeconomic characteristics on older adults' work status"),
                               selectInput("older_job",
                                           label = "",
                                           choices = c("Non-labor income" = "nonlab",
                                                       "Family members' income" = "rest_of_hh",
                                                       "Area" = "area",
                                                       "Sex" = "sex")),
                               plotOutput("older_job_p"),
                               hr(),
                               
                               textOutput("older_t3"),
                               h3("Which socioeconomic characteristics affect whether one does paid or unpaid work?"),
                               plotOutput("older_p3"),
                               hr(),
                               h3("The impact of select socioeconomic characteristics on the nature of one's work"),
                               selectInput("older_pay",
                                           label = "",
                                           choices = c("Sex" = "sex",
                                                       "Family members' income" = "rest_of_hh",
                                                       "Area" = "area",
                                                       "Marital status" = "marital",
                                                       "Household size" = "size")),
                               plotOutput("older_pay_p"),
                               hr(),
                               
                               textOutput("older_t4"),
                               h3("Income distribution among working older adults (unweighted)"),
                               plotOutput("older_p4"),
                               p("")),
                        
                        column(3,
                               panel_icon,
                               fixedPanel(
                                 actionButton("to_sum", label = "summary >"),
                                 right = 10, bottom = 10
                               ))
                      )),
             
             # Tab panel: summary ----------------------
             tabPanel("Summary",
                      value = "sum", hr(), hr(),
                      
                      fluidRow(
                        column(2,
                               wb_icon,
                               fixedPanel(
                                 actionButton("to_older2", label = "< older adults"),
                                 left = 10, bottom = 10
                               )),
                        
                        column(8,
                               textOutput("sum_t1"),
                               hr(),
                               
                               fluidRow(
                                 column(2,
                                        p("pillars of oppression")),
                                 column(10,
                                        column(3, style = "text-align: center;",
                                               strong("gender")),
                                        column(3, style = "text-align: center;",
                                               strong("geography")),
                                        column(3, style = "text-align: center;",
                                               strong("social class")),
                                        column(3, style = "text-align: center;",
                                               strong("marriage and family norms")))
                               ),
                               
                               fluidRow(
                                 column(10, offset = 2,
                                        column(3, style = "padding-top: 2.5em;", 
                                               p("Compared to a boy, a girl born in Bolivia has a",
                                                 style = "font-size: 0.8em; ")),
                                        column(3, style = "padding-top: 1.5em;", 
                                               p("Compared to a person born in urban La Paz, a person born in rural Potosi has a",
                                                 style = "font-size: 0.8em; ")),
                                        column(3,
                                               p("Compared to a person born in a family in the highest income decile, a person born into the lowest income decile has a",
                                                 style = "font-size: 0.8em; ")),
                                        column(3, style = "padding-top: 2em;", 
                                               p("Compared to a single woman, a married woman has a",
                                                 style = "font-size: 0.8em; ")))
                               ),
                               hr(style = "margin: 0.5em 0em;"),
                               
                               fluidRow(
                                 column(2, style = "padding-top: 1.5em;",
                                        p("likelihood to be working at the age of 17",
                                          style = "font-size: 0.8em; ")),
                                 column(10, style = "height: 7em; ",
                                        column(3,
                                               plotOutput("sum_m11", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m12", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m13", height = "7em")),
                                        column(3))
                               ),
                               hr(style = "margin: 0em;"),
                               
                               fluidRow(
                                 column(2, style = "padding-top: 1.5em;",
                                        p("likelihood to be in school at the age of 23",
                                          style = "font-size: 0.8em; ")),
                                 column(10,
                                        column(3,
                                               plotOutput("sum_m21", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m22", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m23", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m24", height = "7em")))
                               ),
                               hr(style = "margin: 0em;"),
                               
                               fluidRow(
                                 column(2, style = "padding-top: 1.5em;",
                                        p("likelihood to be doing paid work at the age of 35",
                                          style = "font-size: 0.8em; ")),
                                 column(10,
                                        column(3,
                                               plotOutput("sum_m31", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m32", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m33", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m34", height = "7em")))
                               ),
                               hr(style = "margin: 0em;"),
                               
                               fluidRow(
                                 column(2,
                                        p("likelihood to be making 3,000+ bolivianos (~PPP$37/day) per month at the age of 45",
                                          style = "font-size: 0.8em; ")),
                                 column(10,
                                        column(3,
                                               plotOutput("sum_m41", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m42", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m43", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m44", height = "7em")))
                               ),
                               hr(style = "margin: 0em;"),
                               
                               fluidRow(
                                 column(2, style = "padding-top: 1.5em;",
                                        p("likelihood to be retired at the age of 65",
                                          style = "font-size: 0.8em; ")),
                                 column(10,
                                        column(3,
                                               plotOutput("sum_m51", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m52", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m53", height = "7em")),
                                        column(3,
                                               plotOutput("sum_m54", height = "7em")))
                               ),
                               hr(style = "margin: 0em;"),
                               
                               p("unit: percentage point", style = "font-style: italic; font-size: 0.8em; text-align: right; padding-bottom: 3em; ")
                        ),
                        column(2,
                               panel_icon
                        )))
  )
)

# Server -----------------------------
server <- function(input, output, session) {
  # Tab panel: home --------------------------
  output$landing <- renderImage(list(src = "www/landing_page1.png", width = "100%"), deleteFile = F)
  observeEvent(input$landing_cl, {
    updateNavbarPage(session, "main", selected = {
      if (between(input$landing_cl$x, 161, 421) & between(input$landing_cl$y, 19, 277)) {"children"}
      else if (between(input$landing_cl$x, 380, 647) & between(input$landing_cl$y, 214, 490)) {"youth"}
      else if (between(input$landing_cl$x, 603, 870) & between(input$landing_cl$y, 14, 271)) {"employment"}
      else if (between(input$landing_cl$x, 843, 1118) & between(input$landing_cl$y, 200, 472)) {"older"}
    })
  })
  
  
  # Tab panel: children --------------------------
  output$children_madlib <- renderText({
    invalidateLater(10000)
    
    madlib_df <- child_worker[sample(nrow(child_worker), 1), ]
    sex_test <- startsWith(madlib_df$sex, "1")
    madlib_name <- ifelse(sex_test, sample(names_m, 1), sample(names_w, 1))
    madlib_pron1 <- ifelse(sex_test, "He", "She")
    madlib_pron2 <- ifelse(sex_test, "he", "she")
    madlib_pron3 <- ifelse(sex_test, "his", "her")
    madlib_lang <- paste0(substring(madlib_df$language_1, 1, 1), tolower(substring(madlib_df$language_1, 2, nchar(madlib_df$language_1))))
    
    paste(
      madlib_name, "is", round(madlib_df$age, 0), "years old.",
      madlib_pron1, "lives in", ifelse(madlib_df$area == "Rural", "rural", "urban"), madlib_df$depto, "and primarily speaks", paste0(madlib_lang, "."),
      case_when(startsWith(madlib_df$in_school, "1") & startsWith(madlib_df$in_attendance, "1") ~ paste(madlib_pron1, "goes to school every day."),
                startsWith(madlib_df$in_school, "1") & startsWith(madlib_df$in_attendance, "2") ~ paste(madlib_pron1, "is enrolled in school but is not always able to attend it."),
                startsWith(madlib_df$in_school, "2") ~ paste(madlib_pron1, "is not going to school.")),
      case_when(!is.na(madlib_df$primary_job) & startsWith(madlib_df$sec_job, "2") ~ paste(madlib_pron1, "works as a", tolower(madlib_df$primary_job), "for", madlib_df$tot_work_week_hr, "hours per week."),
                !is.na(madlib_df$primary_job) & startsWith(madlib_df$sec_job, "1") ~ paste(madlib_pron1, "mainly works as a", tolower(madlib_df$primary_job), "for", madlib_df$primary_work_week_hr, "hours per week, but", madlib_pron2, "also has a second job for another", madlib_df$sec_work_week_hr, "weekly hours."),
                is.na(madlib_df$primary_job) ~ paste(madlib_pron1, "does not have a job.")),
      ifelse(round(madlib_df$lab_monthly_inc, 0) == 0,
             paste(madlib_pron1, "does not earn any income from", madlib_pron3, "work."),
             paste("In total,", madlib_pron2, "makes", round(madlib_df$lab_monthly_inc, 0), "Bolivianos every month."))
    )
  })
  
  output$children_intro <- renderText(children_intro1)
  output$children_t2 <- renderText(children_t21)
  
  output$children_t4 <- renderText(children_t41)
  output$children_t5 <- renderText(children_t51)
  
  output$children_t6 <- renderText(children_t61)
  output$children_t8 <- renderText(children_t81)
  output$children_t9 <- renderText(children_t91)
  output$children_t10 <- renderText(children_t101)
  output$children_t11 <- renderText(children_t111)
  
  output$children_1 <- renderPlot(children_1_p(children))
  output$children_edu4 <- renderPlot(children_edu4_p(why_not_in_school_df))
  
  children_var <- reactive(input$ses)
  
  output$children_ses1 <- renderPlot(
    if (children_var() == "sex") {
      ses1_p(children, "sex", c("boys", "girls"))
    } else if (children_var() == "area") {
      ses1_p(children, "area", c("rural", "urban"))
    } else if (children_var() == "indi") {
      ses1_p(children %>% filter(!startsWith(indigenous, "3")), "indigenous", c("indigenous", "not indigenous"))
    } else if (children_var() == "depto") {
      children_ses_depto(children_depto, "cw", "% children with a job")
    } else if (children_var() == "inc") {
      children_ses1_inc(children)
    }
  )
  
  output$children_ses2 <- renderPlot(
    if (children_var() == "sex") {
      ses2_p(children, "sex", c("boys", "girls"))
    } else if (children_var() == "area") {
      ses2_p(children, "area", c("rural", "urban"))
    } else if (children_var() == "indi") {
      ses2_p(children %>% filter(!startsWith(indigenous, "3")), "indigenous", c("indigenous", "not indigenous"))
    } else if (children_var() == "depto") {
      children_ses_depto(children_depto, "school", "% children not in school")
    } else if (children_var() == "inc") {
      children_ses2_inc(children)
    }
  )
  
  output$children_lfp1 <- renderPlot(children_lfp1_p(children))
  output$children_lfp2 <- renderPlot(children_lfp2_p(children))
  
  
  # Tab panel: youth ------------------------------------------------
  # Youth: intro ----------------------------------
  output$youth_intro <- renderText(youth_intro1)
  
  output$youth_overview <- renderPlot(youth_overview_p(youth))
  
  # Youth: education -------------------------------------
  output$youth_edu_t1 <- renderText(youth_edu_t11)
  output$youth_edu_marital <- renderText(youth_edu_marital1)
  output$youth_edu_internet <- renderText(youth_edu_internet1)
  output$youth_edu_area <- renderText(youth_edu_area1)
  output$youth_edu_indi <- renderText(youth_edu_indi1)
  output$youth_edu_depto <- renderText(youth_edu_depto1)
  output$youth_edu_lang <- renderText(youth_edu_lang1)
  output$youth_edu_t2 <- renderText(youth_edu_t21)
  
  output$youth_edu_imp <- renderPlot(youth_edu_imp_p)
  
  youth_edu1_var <- reactive(input$youth_edu_ses)
  
  output$youth_edu1 <- renderPlot(
    if (youth_edu1_var() == "internet") {
      youth_edu1_internet(youth)
    } else if (youth_edu1_var() == "area") {
      youth_edu1_p(youth, "area", c("rural", "urban"))
    } else if (youth_edu1_var() == "indi") {
      youth_edu1_p(youth %>% mutate(indigenous = ifelse(startsWith(indigenous, "3"), "2. No pertenece", indigenous)),
                   "indigenous", c("indigenous", "not indigenous"))
    } else if (youth_edu1_var() == "depto") {
      youth_edu1_depto(youth)
    } else if (youth_edu1_var() == "lang") {
      youth_edu1_lang(youth)
    } else if (youth_edu1_var() == "marital") {
      youth_edu1_p(youth %>% filter(str_detect(marital, "^[1-3]")), "marital", c("single", "married", "cohabiting"))
    }
  )
  
  output$youth_edu2 <- renderPlot(youth_edu2_p(youth))
  
  # Youth: employment --------------------
  output$youth_emp_overview <- renderText(youth_emp_ov)
  output$youth_emp_stu <- renderText(youth_emp_stu1)
  output$youth_emp_edu <- renderText(youth_emp_edu1)
  output$youth_emp_marital <- renderText(youth_emp_marital1)
  output$youth_emp_area <- renderText(youth_emp_area1)
  
  output$youth_edu_sex <- renderPlot(youth_edu_emp_sex_p(youth %>% filter(in_school == "1. Si")))
  output$youth_emp_sex <- renderPlot(youth_edu_emp_sex_p(youth %>% filter(!is.na(primary_job))))
  output$youth_emp_imp <- renderPlot(youth_emp_imp_p)
  
  youth_emp1_var <- reactive(input$youth_emp_ses)
  
  output$youth_emp1 <- renderPlot(
    if (youth_emp1_var() == "marital") {
      youth_emp1_p(youth %>% filter(str_detect(marital, "^[1-3]")), "marital", c("single", "married", "cohabiting"))
    } else if (youth_emp1_var() == "edu") {
      youth_emp1_p(youth %>% filter(education != "Less than Primary"), "education", c("primary", "secondary", "tertiary"))
    } else if (youth_emp1_var() == "area") {
      youth_emp1_p(youth, "area", c("rural", "urban"))
    } else if (youth_emp1_var() == "stu") {
      youth_emp1_p(youth, "in_school", c("in school", "not in school"))
    }
  )
  
  # Youth: income ------------------------
  output$youth_inc_t1 <- renderText(youth_inc_t11)
  output$youth_inc_t2 <- renderText(youth_inc_t21)
  
  output$youth_inc_paid <- renderPlot(youth_inc_paid_p(youth))
  output$youth_inc_inc <- renderPlot(youth_inc_inc_p(youth))
  
  
  # Tab panel: entering the job market --------------------------
  
  output$lmarket_intro <- renderText(labor_intro)
  
  output$adult_educ <- renderPlot(area_chart_sex(adults))
  # output$wfl_labor <- renderPlot(waffl_work(emp_per))
  output$wfl_labor <- renderImage(list(src = "www/wfl_labor.png", width = "100%"), deleteFile = F)
  
  output$labor_rf <- renderPlot(emp_rfplot)
  
  output$labor_hrs <- renderPlot(hours_worked_graph(adults))
  
  output$labor_t_1 <- renderText(labor_txt_1)
  output$labor_t_2 <- renderText(labor_txt_2)
  output$labor_t_3 <- renderText(labor_txt_3)
  output$labor_t_4 <- renderText(labor_txt_4)
  
  # Tab panel: paid and unpaid labor --------------------------------
  output$pay_intro <- renderText(pay_intro1)
  output$pay_t1 <- renderText(pay_t11)
  output$pay_t2 <- renderText(pay_t21)
  output$pay_t3 <- renderText(pay_t31)
  output$pay_t4 <- renderText(pay_t41)
  output$pay_t5 <- renderText(pay_t51)
  output$pay_t6 <- renderText(pay_t61)
  output$pay_t7 <- renderText(pay_t71)
  
  output$pay_age <- renderPlot(pay_age_p(adults))
  output$pay_lab_inc1 <- renderPlot(pay_lab_inc1_p(adults))
  output$pay_lab_inc2 <- renderPlot(pay_lab_inc2_p(adults))
  output$pay_worth <- renderPlot(pay_worth_p(adults))
  
  output$pay_worth_tot1 <- renderText(comma(adults_unpaid_worth$sum[1]))
  output$pay_worth_tot2 <- renderText(comma(adults_unpaid_worth$sum[2]))
  
  output$pay_ru1 <- renderPlot(pay_ru1_p(adults))
  
  output$pay_ru2 <- renderText(paste0(adults_unpaid_worth_ru$unpaid_pct[1], "%"))
  output$pay_ru3 <- renderText(paste0(adults_unpaid_worth_ru$unpaid_pct[2], "%"))
  
  output$pay_depto1 <- renderPlot(pay_depto1_marital1_p(adults_pay_depto, "Urbana", "Rural", "depto", depto_ru, 25, 2.2, 27, "rural", 1.5, "urban"))
  output$pay_marital1 <- renderPlot(pay_depto1_marital1_p(adults_marital, "Hombre", "Mujer", "marital", marital_sex, 16, 2, 17, "men", 1.5, "women"))
  output$pay_cell1 <- renderPlot(pay_cell1_p(adults))
  
  # Tab panel: neet  --------------------------------
  output$neet_t1 <- renderText(neet_txt_1)
  output$neet_t2 <- renderText(neet_txt_2)
  output$neet_t3 <- renderText(neet_txt_3)
  output$neet_t4 <- renderText(neet_txt_4)
  output$neet_t5 <- renderText(neet_txt_5)

  output$neet_p1 <- renderPlot(area_neet_cat_sex(ages_neet))
  # output$neet_p2 <- renderPlot(waffl_neet(neets_waff))
  output$neet_p2 <- renderImage(list(src = "www/neet_p2.png", width = "100%"), deleteFile = F)
  output$neet_p3 <- renderPlot(plot_bars_neet(why_neet_no_work))
  output$neet_p4 <- renderPlot(plot_bars_neet_study(why_neet_no_study))
  output$neet_p5 <- renderPlot(neets_rfplot)
  
  neet_decision_var <- reactive(input$neet_vars)
  
  output$neets_dec_graph <- renderPlot(
    if (neet_decision_var() == "internet") {
      neets_dec_graph_internet(ages_neet)
    } else if (neet_decision_var() == "area") {
      neet_pop_p(ages_neet, "area", c("rural", "urban"))
    } else if (neet_decision_var() == "indi") {
      neet_pop_p(ages_neet %>% mutate(indigenous = ifelse(startsWith(indigenous, "3"), "2. No pertenece", indigenous)),
                 "indigenous", c("indigenous", "not indigenous"))
    } else if (neet_decision_var() == "depto") {
      neets_dec_graph_depto(ages_neet)
    } else if (neet_decision_var() == "lang") {
      neets_dec_graph_lang(ages_neet)
    } else if (neet_decision_var() == "marital") {
      neet_pop_p(ages_neet %>% filter(str_detect(marital, "^[1-3]")), "marital", c("single", "married", "cohabiting"))
    }
  )
  
  
  # Tab panel: older adults -----------------------------------------
  output$older_t1 <- renderText(older_t11)
  output$older_t2 <- renderText(older_t21)
  output$older_t3 <- renderText(older_t31)
  output$older_t4 <- renderText(older_t41)
  
  output$older_p1 <- renderPlot(older_p1_p(older))
  output$older_p2 <- renderPlot(older_p2_p)
  
  older_job_var <- reactive(input$older_job)
  
  output$older_job_p <- renderPlot(
    if (older_job_var() == "nonlab") {
      older_f1("nonlab_monthly_inc", "monthly non-labor income (BOB)")
    } else if (older_job_var() == "rest_of_hh") {
      older_f1("rest_of_hh", "monthly per capita income for rest of household (BOB)")
    } else if (older_job_var() == "area") {
      older_f2("area", c("rural", "urban"))
    } else if (older_job_var() == "sex") {
      older_f2("sex", c("men", "women"))
    }
  )
  
  output$older_p3 <- renderPlot(older_p3_p)
  
  older_pay_var <- reactive(input$older_pay)
  
  output$older_pay_p <- renderPlot(
    if (older_pay_var() == "sex") {
      older_pay_p_sex(older)
    } else if (older_pay_var() == "area") {
      older_f3("area", c("rural", "urban"))
    } else if (older_pay_var() == "marital") {
      older_f3("marital", c("married/cohabiting", "single/separated/divorced/widowed"))
    } else if (older_pay_var() == "rest_of_hh") {
      older_pay_p_rest_of_hh(older)
    } else if (older_pay_var() == "size") {
      older_pay_p_size(older)
    }
  )
  
  output$older_p4 <- renderPlot(older_p4_p(personas))
  
  
  # Tab panel: summary -----------------------
  output$sum_t1 <- renderText(summ1)

  output$sum_m11 <- renderPlot(m_plot(m1(a1, 0.5, 1)))
  output$sum_m12 <- renderPlot(m_plot(m1(a1, 0.5, 2)))
  output$sum_m13 <- renderPlot(m_plot(m1(a1, 0.5, 3)))
  # output$sum_m14 <- renderText("-")
  
  output$sum_m21 <- renderPlot(m_plot(m1(a2, 1, 1)))
  output$sum_m22 <- renderPlot(m_plot(m1(a2, 1, 2)))
  output$sum_m23 <- renderPlot(m_plot(m1(a2, 1, 3)))
  output$sum_m24 <- renderPlot(m_plot(m1(a2, 1, 4)))
  
  output$sum_m31 <- renderPlot(m_plot(m1(a3, 3, 1)))
  output$sum_m32 <- renderPlot(m_plot(m1(a3, 3, 2)))
  output$sum_m33 <- renderPlot(m_plot(m1(a3, 3, 3)))
  output$sum_m34 <- renderPlot(m_plot(m1(a3, 3, 4)))
  
  output$sum_m41 <- renderPlot(m_plot(m1(a4, 3, 1)))
  output$sum_m42 <- renderPlot(m_plot(m1(a4, 3, 2)))
  output$sum_m43 <- renderPlot(m_plot(m1(a4, 3, 3)))
  output$sum_m44 <- renderPlot(m_plot(m1(a4, 3, 4)))
  
  output$sum_m51 <- renderPlot(m_plot(m1(a5, 3, 1)))
  output$sum_m52 <- renderPlot(m_plot(m1(a5, 3, 2)))
  output$sum_m53 <- renderPlot(m_plot(m1(a5, 3, 3)))
  output$sum_m54 <- renderPlot(m_plot(m1(a5, 3, 4)))
  
  
  # Page navigation ---------------------------------
  # Children
  observeEvent(input$to_youth, updateNavbarPage(session, "main", selected = "youth"))
  
  # Youth
  observeEvent(input$to_children, updateNavbarPage(session, "main", selected = "children"))
  observeEvent(input$to_employment, updateNavbarPage(session, "main", selected = "employment"))
  
  # Adults - entering the job market
  observeEvent(input$to_youth2, updateNavbarPage(session, "main", selected = "youth"))
  observeEvent(input$to_pay, updateNavbarPage(session, "main", selected = "pay"))
  
  # Adults - paid and unpaid labor
  observeEvent(input$to_employment2, updateNavbarPage(session, "main", selected = "employment"))
  observeEvent(input$to_neet, updateNavbarPage(session, "main", selected = "neet"))
  
  # Adults - neet
  observeEvent(input$to_pay2, updateNavbarPage(session, "main", selected = "pay"))
  observeEvent(input$to_older, updateNavbarPage(session, "main", selected = "older"))
  
  # Older adults
  observeEvent(input$to_neet2, updateNavbarPage(session, "main", selected = "neet"))
  observeEvent(input$to_sum, updateNavbarPage(session, "main", selected = "sum"))
  
  # Summary
  observeEvent(input$to_older2, updateNavbarPage(session, "main", selected = "older"))
}

shinyApp(ui = ui, server = server)
