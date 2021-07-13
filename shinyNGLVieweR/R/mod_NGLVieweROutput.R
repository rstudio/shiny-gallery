#' NGLVieweROutput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_NGLVieweROutput_ui <- function(id) {
  ns <- NS(id)
  tagList(
    NGLVieweROutput(ns("structure")),
    div(
      id = ns("render-loader"),
      style = "position: absolute; z-index: 999; top: 91vh; padding: 25px; color: grey;",
      HTML('<div class="fa-1x"><i class="fas fa-spinner fa-spin"></i> loading...</div>')
    )
  )
}
    
#' NGLVieweROutput Server Function
#'
#' @noRd 
mod_NGLVieweROutput_server <- function(input, output, session, r) {
  ns <- session$ns

  output$structure <- renderNGLVieweR({
    # Load example by default
    if (is.null(r$fileInput$PDB)) {
      r$fileInput$PDB <- app_sys("app/www/7cid.ngl")
      r$fileInput$fileExt <- "pdb"
      r$fileInput$name <- "7cid"
    }
    viewerOutput <- NGLVieweR(r$fileInput$PDB, format = r$fileInput$fileExt) %>%
      loadStage(r$fileInput$stage) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(FALSE) %>%
      addRepresentation("ball+stick", param = list(
        name = "aa_clicked", visible = TRUE,
        sele = "none", color = "element", colorValue = "#33FF19"
      )) %>%
      
      # Load from .ngl file
      loadLabels(r$fileInput$labels) %>%
      loadSelections(r$fileInput$selections) %>%
      loadLigand(r$fileInput$ligand) %>%
      loadContacts(r$fileInput$contacts) %>%
      loadStructure(r$fileInput$structure, format = r$fileInput$fileExt) %>%
      loadSurface(r$fileInput$surface)
    
    isolate({ 
    r$selection$loaded <- FALSE 
    r$label$loaded <- FALSE 
    r$contact$loaded <- FALSE 
    r$structure$loaded <- FALSE
    r$surface$loaded <- FALSE
    r$ligand$loaded <- FALSE
    r$stage$loaded <- FALSE
    r$structure$structure <- NULL
    r$surface$surface <- NULL
    r$ligand$ligand <- NULL
    r$stage$stage <- NULL
    r$contact$contacts <- NULL
    })
    
    return(viewerOutput)
  })
  
  #Loader
  observeEvent(r$rendering, {
    if(isTRUE(r$rendering)  || is.null(r$rendering)){
      shinyjs::show("render-loader")
    } else {
      shinyjs::hide("render-loader")
    }
  })
} 
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
