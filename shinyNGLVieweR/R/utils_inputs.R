#' textInput with bs info_modal
#' 
#' @description    
#' Create a textInput with a bs info_modal.
#'
#' @param id The id of the element
#' @param placeholder Input placeholder defaults to \code{NULL}
#' @param id_modal Id of the modal to show.
#' @param label label of textInput.
#' @param value value of textInput.
#' @param width width of textInput.
#' 
#' @importFrom bsplus shinyInput_label_embed shiny_iconlink bs_attach_modal
#' @importFrom shiny textInput 
#' @export
bs_textInput <- function(id, label, placeholder = NULL, value = "", width = NULL, id_modal) {
  textInput(id,
            label = label,
            placeholder = placeholder,
            value = value,
            width = width
  ) %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_attach_modal(id_modal = id_modal)
    )
}

#' modal for bs_Inputs
#' 
#' @description    
#' Create a modal for a bs_input. To activate the modal it needs to be entered in the UI.
#'
#' @param id The id of the element. should be similar as id_modal given in the \code{bs_textInput()} function.
#' @param title Title of the modal.
#' @param body The body of the modal. Can also be a htmlTemplate.
#' @param size Size of the modal.
#' 
#' @importFrom bsplus bs_modal
#' @importFrom shiny htmlTemplate 
#' @export
bs_input_modal <- function(id, title, body, size) {
  bs_modal(
    id = id,
    title = title,
    body = body,
    size = size
  )
}

#' Collapse box on title click
#' 
#' @examples
#' tags$head(  
#' extendShinyjs(text = jsboxCollapse, functions = c("collapse"))
#' )
#' observeEvent(input$sequenceTitle, {
#' js$collapse("sequence")
#' })
#'     
#' @export
jsboxCollapse <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"