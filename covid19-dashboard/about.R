aboutUI <- function(id) {
  ns <- NS(id)
  material_row(
    material_column(width = 6,
                    h2("The data"),
                    img(src="./img/enterprise-medicine.logo.small.horizontal.white.581be190.png", width=120, style="background-color:#002d72;padding: 0.8em;
"),
                    p("All data is taken from Johns Hopkins CSSE dataset on github"),
                    a(href="https://github.com/CSSEGISandData/COVID-19", "Johns Hopkins Github Repository"),
                    tags$br(),
                    h2("Updates"),
                    tags$ul(
                      tags$li(
                        tags$b("2020-03-25: "),tags$span("Data was moved on CSSE data set, now Recovered Cases are only updated on a daily basis.")
                      )
                    )
    ),
    material_column(width = 6,
                    h2("The author"),
                    material_row(
                      material_column(width = 6,
                                      img(src="./img/zappingseb.jfif", width=60),
                                      p(a(href="https://mail-wolf.de/?page_id=1292", alt="Sebastian Engel-Wolf", "Sebastian Engel-Wolf"),
" is a freelance scientific software developer developing R-shiny apps in a pharmaceutical environment"),
                                      p(a(href="https://www.linkedin.com/in/zappingseb/", "LinkedIn")),
                                      p(a(href="https://www.mail-wolf.de", "Homepage")),
                                      p(a(href="https://twitter.com/zappingseb", "Twitter"))
                      ),
                      material_column(width = 6,
                                      img(src="./img/graphcount.jpg", width=60),
                                      p("My Monkey and Graph Count did some work")          
                      )
                    ),
                    p(a(href="https://github.com/zappingseb/coronashiny",
                        img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/Octicons-mark-github.svg/600px-Octicons-mark-github.svg.png",
                            width = 60)
                    )),
                    p(a(href="https://github.com/zappingseb/coronashiny", "All code for this project"), "can be found on github")
                    
    )
  )
}
about <- function(input, output, session) {
  
}