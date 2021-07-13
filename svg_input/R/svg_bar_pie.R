
svg_bar_pie <- function(inputId, svg) {
  
  svg <- "
    <svg width='100%' viewbox='0 0 90 190' xmlns='http://www.w3.org/2000/svg' xmlns:svg='http://www.w3.org/2000/svg'>
    <g class='layer'>
    <title>Layer 3
    </title>
    <path data-shinyclick='pie' d='m75.98675,145.32408l-29.67501,0l-20.98387,20.2957a30.52199,29.52102 0 0 0 50.65506,-20.2957l0.00382,0zm-53.35626,17.68309a30.52199,29.52102 0 0 1 20.98387,-48.99382l0,28.70181l-20.98387,20.2957l0,-0.00369zm24.79912,-48.99382l0,27.6206l28.55714,0a30.52581,29.52471 0 0 0 -28.55714,-27.6206z' fill='black' id='svg_6'/>
    </g>
    <g class='layer'>
    <title>Layer 1
    </title>
    <g id='svg_3' transform='matrix(3.99792, 0, 0, 3.57357, -581.199, -372.499)'>
    <svg class='bi bi-bar-chart-line-fill' height='16' id='svg_4' viewBox='0 0 16 16' width='16' x='149.08323' xmlns='http://www.w3.org/2000/svg' y='108.57435'>
    <path data-shinyclick='bar' d='m11,2a1,1 0 0 1 1,-1l2,0a1,1 0 0 1 1,1l0,12l0.5,0a0.5,0.5 0 0 1 0,1l-15,0a0.5,0.5 0 0 1 0,-1l0.5,0l0,-3a1,1 0 0 1 1,-1l2,0a1,1 0 0 1 1,1l0,3l1,0l0,-7a1,1 0 0 1 1,-1l2,0a1,1 0 0 1 1,1l0,7l1,0l0,-12z' id='svg_5'/>
    </svg>
    </g>
    </g>
    </svg>
  "
  
  tagList(
    singleton(tags$head(
      tags$script(src = "svg_bar_pie.js")
    )),
    tags$div(
      class = "svg_bar_pie",
      id = inputId,
      `data-input-id` = inputId,
      shiny::HTML(svg)
    )
  )
}
