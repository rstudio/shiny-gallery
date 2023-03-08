#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  r <- reactiveValues()
  
  observe({
    r$sequence <- input$`NGLVieweROutput_ui_1-structure_sequence`
    r$resno <- input$`NGLVieweROutput_ui_1-structure_resno`
    r$chainname <- input$`NGLVieweROutput_ui_1-structure_chainname`
    r$sequence_df <- sequence_df(r$sequence, r$resno, r$chainname, selchain = r$sequenceOutput$selectedChain)
    r$aa_clicked <- input$aa_clicked
    r$PDB <- input$`NGLVieweROutput_ui_1-structure_PDB`
  })
  
  observe({
    r$rendering <- input$`NGLVieweROutput_ui_1-structure_rendering`
  })

  observe({
    r$sidebarItemExpanded <- input$sidebarItemExpanded #loading of UI_components from .ngl file
  })
  
  #Component handlers
  observe({
    r$selection$selectionRemove_id <- input$selectionRemove_id #handler defined in handlers.js
    r$selection$selectionLink_id <- input$selectionLink_id 
    r$label$labelRemove_id <- input$labelRemove_id #handler defined in handlers.js
    r$label$labelLink_id <- input$labelLink_id 
    r$contact$contactRemove_id <- input$contactRemove_id #handler defined in handlers.js
    r$contact$contactLink_id <- input$contactLink_id 
  })
  
  observe({
    r$examples$example_link_id <- input$example_link_id 
  })
  
  callModule(mod_fileInput_server, "fileInput_ui_1", r=r)
  callModule(mod_fileOutput_server, "fileOutput_ui_1", r=r)
  callModule(mod_examples_server, "examples_ui_1", r=r)
  callModule(mod_structure_server, "structure_ui_1", globalSession = session, r=r) #Pass globalSession to access NGLViewer object
  callModule(mod_surface_server, "surface_ui_1", globalSession = session, r=r)
  callModule(mod_ligand_server, "ligand_ui_1", globalSession = session, r=r)
  callModule(mod_selection_server, "selection_ui_1", globalSession = session, r=r)
  callModule(mod_label_server, "label_ui_1", globalSession = session, r=r)
  callModule(mod_contact_server, "contact_ui_1", globalSession = session, r=r)
  callModule(mod_stage_server, "stage_ui_1", globalSession = session, r=r)
  callModule(mod_snapshot_server, "snapshot_ui_1", globalSession = session)
  callModule(mod_sidebarcontrols_server, "sidebarcontrols_ui_1", globalSession = session, r=r)
  callModule(mod_labelcontrols_server, "labelcontrols_ui_1", globalSession = session, r=r)
  callModule(mod_sequenceOutput_server, "sequenceOutput_ui_1", globalSession = session, r=r)
  callModule(mod_NGLVieweROutput_server, "NGLVieweROutput_ui_1", r=r)

}
