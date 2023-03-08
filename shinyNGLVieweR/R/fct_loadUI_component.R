#' Load UI components
#'
#' @description
#' Function to load UI components 
#'
#' @param data data.frame of saved components
#' @param type type of component. E.g. label, selection, contact
#' 
#' @import stringr
#' @export
loadUI_component <- function(data, type) {
    removeUI(selector = sprintf(".%sholder", type), multiple = TRUE)
    if (!is.null(data)) {
      for (n in 1:nrow(data)) {
        uu_id <- stringr::str_replace(data[n, "id"], sprintf("%s-", type), "")
        insertUI(
          selector = sprintf("#%sPlaceholder", type),
          ui = tags$div(
            style = "display:flex;",
            id = data[n, "id"],
            class = sprintf("%sholder", type),
            actionLink(
              sprintf("%sLink-%s", type, uu_id),
              label = data[n, "name"],
              class = sprintf("%sLink btn-link", type),
            ),
            actionLink(
              sprintf("%sRemove-%s", type, uu_id),
              label = NULL,
              icon = icon("trash"),
              class = sprintf("%sRemove", type),
              style = "color: red;"
            )
          )
        )
      }
  }
}