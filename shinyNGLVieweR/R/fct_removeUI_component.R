#' Remove component from UI
#'
#' @description
#' Function to add a UI once a component has been submitted 
#'
#' @param NGLVieweR viewer component 
#' @param data data.frame of saved components
#' @param type type of component. E.g. label, selection, contact
#' @param id ID of selected remove link
#' 
#' @examples 
#' removeUI_component('selection', input$selectionRemove_id)
#' @import stringr
#' @export
removeUI_component <- function(NGLVieweR, data, type, id) {
  id <- stringr::str_replace(id, sprintf("%sRemove-", type), "")
  type_id <- sprintf("%s-%s", type, id)
  # removeUI
  removeUI(selector = sprintf("#%s", type_id))
  # remove structure component
  NGLVieweR %>% removeSelection(type_id)
  # remove seletion from data.frame
  data <- subset(data, id != type_id)
  return(data)
}

