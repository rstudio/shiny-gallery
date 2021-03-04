

library(shiny)
library(bslib)
library(waiter)
library(bsplus)
library(shinyjs)

bs_theme_new(version = "4+3", bootswatch = "slate")


bs_theme_accent_colors(secondary = "#f8f8f8e6")
bs_theme_add_variables(black = "#88837d")


ui = tagList(
  
  ## functions required to be used in the ui
  bootstrap(),
  use_waiter(include_js = FALSE),
  use_bs_popover(),
  useShinyjs(),
  withMathJax(),
  
  tags$footer(
    
    fluidRow(
      column(
        width = 4
      ),
      
      column(
        width = 8,
        tagList(
          tags$img(src = "RStudio-Logo-White.png", width = "80px", height = "30px"),
          tags$img(src = "pipe.png", width = "60px", height = "67px"),
          tags$img(src = "shiny.png", width = "60px", height = "67px"),
          tags$img(src = "tidyverse.png", width = "60px", height = "67px"),
          tags$img(src = "gganimate.png", width = "60px", height = "67px"),
          tags$img(src = "waiter.png", width = "60px", height = "67px"),
          tags$img(src = "ggforce.png", width = "60px", height = "67px"),
          tags$img(src = "glue.png", width = "60px", height = "67px")
        )
      )
    ),
    style = "position:absolute; bottom:0; width:95%;
    height:77px; /* Height of the footer */
    color: white; padding: 10px;
    background-color: #272b30; z-index: 1000;"
  ),
  
  ## color of the action button, height of the input n box
  tags$head(
    tags$style(HTML("#run{color:#272727; height: 47px}
                     #n{background:#272b30;}
                     .irs-bar {background:#272b30; border-top: #272b30; border-bottom: #272b30}
                     .irs-bar-edge {background:#272b30; border: #272b30;}
                     .irs-single {background:#272b30;}
                     .irs-max {background:#E7553C; color: #f8f8f8}
                     .irs-min {background:#f8f8f8}
                     .irs-grid-pol {background: #272b30}
                     #go_to_work{color:#e9ecef; height: 45px; background-color:#447099"))
  ),
  
  ## the actual page starts here!
  navbarPage(
    id = "navbar",
    windowTitle = "Shiny app: Life of Pi",
    
    ## title
    title = tagList(
      fluidRow(
        div(
          tags$img(src = "ba.png", width = "32px", height = "26px"), HTML("&nbsp;"),
          "Life of Pi: A Monte Carlo Simulation", HTML("&nbsp;"), style = "color:#aaa;"
        ),
        div("BY ZAUAD SHAHREER ABEER", HTML("&nbsp;"),
            tags$a(href = "https://github.com/shahreyar-abeer", tags$img(src = "GitHub-Mark-Light-120px-plus.png", width = "26px", height = "26px")),
            tags$a(href = "https://www.linkedin.com/in/zauad-shahreer/", tags$img(src = "LI-In-Bug.png", width = "30px", height = "26px")),
            style = "color:#aaa; position:absolute; float:right; right: 10px;")
      ) 
    ),
    #############################################
    ## Home tab
    tabPanel(
      title = "Home",
      fluidRow(
        column(
          width = 1,
          br(),
          br(),
          tags$a(href = "https://github.com/shahreyar-abeer/life_of_pi", img(src = "life_of_pi_hex.png", width = "100px", height = "110px", style = "position: absolute; top: 200px; left:10px;"))
          
        ),
        
        column(
          width = 9,
          
          p("This app is designed to run a Monte Carlo Simulation to
            estimate the value of \\(\\pi\\). I'm sorry to disappoint some of you
            who might have though it had something to do with the movie.
            But I can share a bit of history though."),
          p("Pi wasn’t always known as pi. Before the 1700s,
            people referred to the number we know as pi as
            'the quantity which when the diameter is multiplied by it, yields the circumference'.
            Not surprisingly, people got tired of saying so much whenever they wanted
            to talk about Pi. The Welsh mathematician William Jones,
            a friend of Sir Isaac Newton, began using the symbol for \\(\\pi\\) in 1706."),
          # 
          # p("This app shows a Monte Carlo estimation of pi 
          # Those who thought this app had something to do the movie, I'm sorry to disappoint.
          # It has less to do with the movie and more to do with mathematician's favorite number, \\(\\pi\\)"),
          # p("\\(\\pi\\) has been around since the inception of the earth,
          #   since it's the ratio of cirumference to the diamtere of a circle.
          #   So it's always been there! Waiting to be discovered by someone. "),
          
          div(h4("The Algorithm"), align = "center"),
          p("At first, we inscribe a circle with unit radius (\\(r = 1)\\) in a square (\\(length = 2 r\\)),
            note down the area of the circle and the square. Then we let some points fall
            freely on the canvass. The points are independent and may fall at any place within 
            the square. We then take a note of the poportion \\((p)\\) of points that have fallen inside
            the circle to the total number of points. This proportion gives 1/4 th of \\(\\pi\\).
            We then multiply the result with 4 to get an estimate of \\(\\pi\\)."),
          p("The mathematics working behind is: "),
          div(p("Area of the circle, \\(A = \\pi r^2 = \\pi\\)"), align = "center"),
          div(p("Area of square, \\((2r)^2 = 4\\)"), align = "center"),
          div(p("\\(P(a \\ point \\ falling \\ inside \\ the \\ circle) = \\pi/4\\)"), align = "center"),
          div(p("\\(p = \\pi/4 \\implies \\pi = 4 p\\)"), align = "center"),
          p("We shall estimate \\(\\pi\\) using this equation. If you are interested to know more, you should find some good stuff ",
            tags$a("here.", href = 'http://www.science.smith.edu/dftwiki/images/b/b9/MonteCarloBookChapter.pdf', target = '_blank'),
            "And no, there are no tigers there!"),
          br(),
          div(actionButton("go_to_work", "Enough chit-chat, let's get to work!", status = "success"), align = "center")
        ),
        
        column(
          width = 2,
          tags$a(href = "https://www.upwork.com/o/profiles/users/~01a42a4a2859568446/", tags$img(src = "logo.png", width = "200px", height = "100px", style = "position: absolute; top: 200px; right:10px;"))
        )
      ),
    ),
    
    #############################################
    ## Work tab
    tabPanel(
      title = "Work",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          shinyWidgets::sliderTextInput("n", "Number of points", slider_vals, 1000, F, T)%>%
            shinyInput_label_embed(
              shiny_iconlink() %>%
                bs_embed_popover(
                  title = "range: (100-10002)",
                  content = "Values that are multiples of 100 load quite fast. Special value: 10001.
                  Other values will take some time to render and the animation won't be quite as smooth.",
                  placement = "right"
                )
            ),
          
          div(actionButton("run", "Let's run!", icon = icon("walking"), width = "40%"), align = "center"),
          br(),
          p("Just a note: We will never be able to find all the digits of pi because of its very definition as an
            irrational number. Babylonian civilization used the fraction 3 ⅛, the Chinese used
            the integer 3. By 1665, Isaac Newton calculated pi to 16 decimal places."),
          p("In 2017, a Swiss scientist computed more than 22 trillion digits of pi!
            The calculation took over a hundred days."),
          p("Oh, I hope you have tried the special number, it's 10002."),
          p("And if you feel down, have some pi!"),
          hr()
        ),
        
        mainPanel(
          width = 9,
          fluidRow(
            column(
              width = 6,
              imageOutput("anim1")
            ),
            column(
              width = 6,
              imageOutput("anim2")
            )
          )
        )
      )
    )
  ),
  waiter_show_on_load(html = spin_loaders(42, color = "#aaa"), color = "#272b30", logo = "logo.png")
)