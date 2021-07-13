#' Insert components to UI
#'
#' @description
#' Function to add UI components on submission 
#'
#' @param type type of components. E.g. label, selection, contact.
#' @param name name of the components. Use \code{insertUI_name()} to generate a unique name. 
#' @param uu_id uniqueID used to name the UI components. 
#' 
#' @examples 
#' \dontrun{
#' insertUI_component('selection', insertUI_name('ligand'), 
#'                    counter = 3, uu_id = "575ceb43-9ff5-40fb-85a4-9a8e9e06ceff")
#' }
#' @export
insertUI_component <- function(type, name, uu_id){
  insertUI(
    selector = sprintf('#%sPlaceholder', type),
    ui = tags$div(
      style = 'display:flex;',
      id = sprintf('%s-%s', type, uu_id),
      class = sprintf("%sholder", type),
      style = 'font-size: 100%; padding: 0;',
      actionLink(
        sprintf('%sLink-%s', type, uu_id),
        label = name,
        class = sprintf("%sLink btn-link", type),
      ),
      actionLink(
        sprintf('%sRemove-%s', type, uu_id),
        label = NULL,
        icon = icon('trash'),
        class = sprintf("%sRemove", type),
        style = 'color: red;'
      )
    )
  )
}

#' component UI name
#'
#' @description
#' Function to add a unique name  
#'
#' @param name name of the selection. Defaults to selection-\code{counter} if input is \code{NULL} or \code{""} 
#' @param counter value to create unique \code{name} if none is provided
#' @param type type of selection. E.g. label, selection, contact.
#' 
#' @examples 
#' \dontrun{
#' insertUI_name('selection', '', 3)
#' "selection-3"
#' }
#' @export
insertUI_name <- function(type, name = NULL, counter = 0) {
  
  if (is.null(name)) {
    name <- type
  } else if (nchar(name) < 1) {
    name <- type
  } else {
    name <- name
  }
  
  if (name == type && counter != 0) {
    name <- sprintf("%s-%s", name, counter)
  }
  return(name)
}

#' components UI selection
#'
#' @description
#' Function to transform ngl.js selection for UI component  
#'
#' @param selection ngl.js query to be transformed. Uses \code{selection_to_ngl()}
#' @param sequence protein sequence in string format.
#' 
#' @examples 
#' \dontrun{
#' insertUI_selection("1-3 OR <GHK>", "NGLSDFGHK")
#' }
#' @export
insertUI_selection <- function(sequence, selection) {
  
  if (nchar(isolate(selection)) > 0) {
    selection <- selection_to_ngl(sequence, selection)
  } else {
    selection <- 'none'
  }
  return(selection)
}