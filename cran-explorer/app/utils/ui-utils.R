################################################################################
# Generic UI helper functions
#
# Author: Stefan Schliebs
# Created: 2019-02-21 09:40:56
################################################################################


# Material card -----------------------------------------------------------

material_card <- function(..., header = NULL, bgcolor = "white") {
  div(
    class = "card",
    header, 
    div(class = "card-content", ..., style = sprintf("background-color: %s", bgcolor))
  )
}
