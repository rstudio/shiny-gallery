#' fileInput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fileInput_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menuItem("load",
      tabName = "loadTab", icon = icon("file-upload"),
      textInput(ns("codePDB"), label = NULL, placeholder = "PDB code"),
      actionButton(ns("loadCode"), "Load"),
      fileInput(ns("inputPDB"), "Choose PDB File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".mmcif", ".cif", ".mcif", ".pdb", ".ent", ".pqr",
          ".gro", ".sdf", ".sd", ".mol2", ".mmtf", ".ngl"
        )
      ),
      actionButton(ns("loadStructure"), "Load", style="margin-top:-15px;")
    )
  )
}
    
#' fileInput Server Function
#'
#' @noRd 
mod_fileInput_server <- function(input, output, session, r) {
  ns <- session$ns
  
  #Load example when nothing loaded
  #r$fileinput$PDB <- "www/examples/7cid.ngl" 


  observeEvent(input$loadCode, {
    
    r$rendering <- TRUE
    r$fileInput <- readFile(input$codePDB)
    r$fileInput$name <- input$codePDB
    r$stage$fileColor <- "black" #Needed to change theme
  })

  observeEvent(input$loadStructure, {
    r$rendering <- TRUE
    r$fileInput <- readFile(input$inputPDB$datapath)
    r$stage$fileColor <- r$fileInput$stage$backgroundColor #Needed to change theme
    r$fileInput$name <- tools::file_path_sans_ext(input$inputPDB$name)
  })
}
