
svg_titanic <- function(inputId, svg) {
  
  svg <- "
    <svg width='100%' viewbox='0 0 400 150' xmlns='http://www.w3.org/2000/svg' xmlns:svg='http://www.w3.org/2000/svg'>
    <g class='layer'>
    <title>Layer 1
    </title>
    <path d='m6.60135,105.10256c129.32482,-4.20664 258.64962,4.20664 387.97441,0l0,7.57195c-129.3248,4.20664 -258.64959,-4.20664 -387.97441,0l0,-7.57195z' fill='#96aad1' id='svg_1'/>
    <path data-shinyclick='crew' d='m19.33336,92.33332l7.25367,11.33334l345.20993,2l11.86968,-16l-364.33329,2.66666l0.00001,0z' fill='#666666' id='svg_2'/>
    <path data-shinyclick='third' d='m19.33335,90.33332l-5.66669,-13.66666l376,1c0,0 -5.33331,8.66666 -5.33331,9.66666c0,1 -365,3 -365,3z' fill='#666666' id='svg_3'/>
    <path data-shinyclick='second' d='m60.33335,74.33332l10.33331,-9.66666l263,0l8,9l-281.33331,0.66666z' fill='white' id='svg_4' stroke='#666666'/>
    <path data-shinyclick='first' d='m92.33335,62.33332l6.33331,-13.66666l219,0l4,11l-229.33331,2.66666z' fill='white' id='svg_5' stroke='#666666'/>
    <rect fill='#fba061' height='30' id='svg_6' stroke='#666666' width='15' x='129.66666' y='15.33332'/>
    <rect fill='#666666' height='9.2' id='svg_8' stroke='#666666' width='15' x='129.66666' y='5.79999'/>
    <rect fill='#fba061' height='30' id='svg_10' stroke='#666666' width='15' x='172.89999' y='15.56666'/>
    <rect fill='#666666' height='9.2' id='svg_9' stroke='#666666' width='15' x='172.89999' y='6.03333'/>
    <rect fill='#fba061' height='30' id='svg_12' stroke='#666666' width='15' x='223.7' y='16.36666'/>
    <rect fill='#666666' height='9.2' id='svg_11' stroke='#666666' width='15' x='223.7' y='6.83333'/>
    <rect fill='#fba061' height='30' id='svg_14' stroke='#666666' width='15' x='270.5' y='16.76666'/>
    <rect fill='#666666' height='9.2' id='svg_13' stroke='#666666' width='15' x='270.5' y='7.23333'/>
    </g>
    </svg>
  "
  
  tagList(
    singleton(tags$head(
      tags$script(src = "svg_titanic.js")
    )),
    tags$div(
      class = "svg_titanic",
      id = inputId,
      `data-input-id` = inputId,
      shiny::HTML(svg)
    )
  )
}
