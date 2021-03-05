spins <- list(
  spin_1(),
  spin_2(),
  spin_3(),
  spin_3circles(),
  spin_3k(),
  spin_4,
  spin_5,
  spin_6,
  spin_atebits(),
  spin_balance(),
  spin_ball(),
  spin_chasing_dots(),
  spin_circle(),
  spin_circle_square(),
  spin_circles(),
  spin_clock(),
  spin_cube_grid(),
  spin_dots(),
  spin_double_bounce(),
  spin_dual_circle(),
  spin_dual_ring(),
  spin_ellipsis(),
  spin_epic(),
  spin_facebook(),
  spin_fading_circles(),
  spin_fill(),
  spin_flower(),
  spin_flowers(),
  spin_folding_cube(),
  spin_gauge(),
  spin_google(),
  spin_half(),
  spin_heart(),
  spin_heartbeat(),
  spin_hexdots(),
  spin_hourglass(),
  spin_kit(),
  spin_loader(),
  spin_loaders(),
  spin_orbit(),
  spin_orbiter(),
  spin_pixel(),
  spin_plus(),
  spin_pong(),
  spin_pulsar(),
  spin_pulse(),
  spin_pushing_shapes(),
  spin_puzzle(),
  spin_refresh(),
  spin_rhombus(),
  spin_ring(),
  spin_ripple(),
  spin_rotate(),
  spin_rotating_plane(),
  spin_seven_circle(),
  spin_solar(),
  spin_square_circle(),
  spin_squares(),
  spin_terminal(),
  spin_three_bounce(),
  spin_throbber(),
  spin_timer(),
  spin_wandering_cubes(),
  spin_wave(),
  spin_whirly(),
  spin_wobblebar()
)
set.seed(Sys.time())
spinner <- spins[[sample(1:66, 1)]]
fluidPage(
  title = "Covid-19 Timeline",
  tags$head(
    tags$style(
      css
    )
  ),
  tags$style(
    HTML("
  .tabbable > .nav > li > a {
  background-color: #00bb8b;  color:#161616;
  border:none;
  }
  .tabbable > .nav > li > a[data-value='Timeline of the outbreak'] {
  background-color: #161616;   color:#00bb8b;
  border:none;
  }
  .tabbable > .nav > li > a[data-value='Worldwide cases'] {
  background-color: #161616;  color:#00bb8b;
  border:none;
  }
  .tabbable > .nav > li[class=active]    > a {
  background-color: #00bb8b; color:#161616;
  border:none;
  }
  ")
  ),
  withAnim(),
  use_waiter(),
  useShinyjs(),
  waiter_show_on_load(spinner, color = "#161616"),
  add_busy_bar(color = "#00bb8b"),
  tabsetPanel(
    id = "tabset",
    tabPanel(
      "Timeline of the outbreak",
      HTML('<a name="top">'),
      absolutePanel(
        draggable = FALSE,
        fixed = TRUE,
        top = 1200,
        left = 10,
        HTML(
          '<a href = "https://github.com/nicoFhahn/covid_shiny"
        target="_blank">
        <button class="github" id = "test1"
        title="GitHub repository">GitHub</button></a>'
        ),
        HTML('
        <button class="chapter" id = "chapter_sel"
        title="Select chapter">Chapter</button></a>'),
        HTML(
          '<a href = "#top"><button class="myBtn"
        title="jump to top">jump to top</button></a>'
        ),
        style = "z-index: 420; border:1px solid red;"
      ),
      fluidRow(
        includeHTML("html_files/landing.html")
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_1.html")
        )
      ),
      HTML('<a name="chapter1"></a>'),
      fluidRow(
        includeHTML("html_files/chapter1.html")
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_1_1.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "lf1", leafletOutput("map_wuhan", height = "80vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_2.html")
        )
      ),
      HTML('<a name="chapter2"></a>'),
      fluidRow(
        includeHTML("html_files/chapter2.html")
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text2_1.html")
        )
      ),
      fluidRow(
        column(
          width = 2
        ),
        column(
          width = 8,
          div(id = "lf2", leafletOutput("map_emergency", height = "80vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_3.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc1", highchartOutput("highcharter_1", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_4.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc2", highchartOutput("highcharter_2", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_5.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc3", highchartOutput("highcharter_3", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_6.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc4", highchartOutput("highcharter_4", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_7.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc5", highchartOutput("highcharter_5", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_8.html")
        )
      ),
      HTML('<a name="chapter3"></a>'),
      fluidRow(
        includeHTML("html_files/chapter3.html")
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_8_1.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc6", highchartOutput("highcharter_6", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_9.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc7", highchartOutput("highcharter_7", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_10.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc14", highchartOutput("highcharter_14", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_13.html")
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/today_1.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc8", highchartOutput("highcharter_8", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(class = "story_2"),
          div(id = "hc9", highchartOutput("highcharter_9", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          div(class = "date_3"),
          includeHTML("html_files/today_2.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc10", highchartOutput("highcharter_10", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(class = "story_2"),
          div(id = "hc11", highchartOutput("highcharter_11", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          div(class = "date_3"),
          includeHTML("html_files/today_3.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc12", highchartOutput("highcharter_12", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(class = "story_2"),
          div(id = "hc13", highchartOutput("highcharter_13", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_11.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc22", highchartOutput("highcharter_22", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/text_12.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc23", highchartOutput("highcharter_23", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/today.html")
        )
      ),
      HTML('<a name="chapter4"></a>'),
      fluidRow(
        includeHTML("html_files/chapter4.html")
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/leaders_1.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc15", highchartOutput("highcharter_15", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/leaders_2.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc16", highchartOutput("highcharter_16", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/leaders_3.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc17", highchartOutput("highcharter_17", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/leaders_4.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc18", highchartOutput("highcharter_18", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/leaders_5.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc19", highchartOutput("highcharter_19", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/leaders_6.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc20", highchartOutput("highcharter_20", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/leaders_7.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc21", highchartOutput("highcharter_21", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/leaders_8.html")
        )
      ),
      HTML('<a name="chapter5"></a>'),
      fluidRow(
        includeHTML("html_files/chapter5.html")
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/lockdown1.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "lf3", leafletOutput("map_lockdown", height = "65vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/lockdown2.html")
        )
      ),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          div(id = "hc24", highchartOutput("highcharter_24", height = "80vh"))
        )
      ),
      fluidRow(
        column(
          width = 4
        ),
        column(
          width = 4,
          includeHTML("html_files/lockdown3.html")
        )
      )
    ),
    tabPanel(
      "Worldwide cases",
      icon = icon("map"),
      tags$head(
        shiny::includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      use_googlefont("Oswald"),
      use_theme(create_theme(
        theme = "default",
        bs_vars_font(
          family_sans_serif = "'Oswald', cursive"
        )
      )),
      leafletOutput(
        "mymap",
        width = "100%",
        height = "100vh"
      ),
      absolutePanel(
        id = "controls_panel",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 40,
        left = "auto",
        right = 60,
        bottom = "auto",
        width = "18%",
        height = "auto",
        br(),
        HTML(
          '<button data-toggle="collapse"
          class = "clp_btn" data-target="#panel_wrapper">Collapse</button>'
        ),
        tags$div(id = "demo", class = "collapse"),
        br(),
        h2(
          "SARS-CoV-2 Outbreak 2020",
          style = "font-size:1.8em;"
        ),
        div(
          id = "panel_wrapper", class = "panel_wrapper collapse in",
          uiOutput("total_cases_ui"),
          br(),
          fluidRow(
            column(
              width = 4,
              uiOutput("cases_all_ui")
            ),
            column(
              width = 4,
              uiOutput("cases_recovered_ui")
            ),
            column(
              width = 4,
              htmlOutput("cases_death_ui")
            )
          ),
          br(),
          htmlOutput("show_everything"),
          br(),
          uiOutput("date_ui"),
          h2(
            "Cumulative numbers:",
            style = "font-size:1.25em;"
          ),
          plotlyOutput("everything_plot", height = "12em"),
          h2(
            "Daily numbers:",
            style = "font-size:1.25em;"
          ),
          plotlyOutput("daily_plot_confirmed", height = "8em"),
          plotlyOutput("daily_plot_deaths", height = "8em"),
          br(),
          h2(
            "Created by Nico Hahn with data from Johns Hopkins University",
            style = "font-size:0.75em;"
          ),
          h2(
            "Works best in Firefox",
            style = "font-size:0.75em;"
          )
        ),
        style = "z-index: 420;"
      )
    )
  ),
  setBackgroundColor("#161616")
)
