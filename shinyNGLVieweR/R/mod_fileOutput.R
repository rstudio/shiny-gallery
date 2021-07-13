#' fileOutput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fileOutput_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menuItem("download",
      tabName = "downloadTab", icon = icon("file-download"),
      fluidRow(
        style = "padding: 15px 5px 0 15px; margin: 0;",
        downloadLink(ns("downloadNGL"), "PDBVieweR session", class = "btn-link")
      )
    )
  )
}
#' fileOutput Server Function
#'
#' @noRd 
mod_fileOutput_server <- function(input, output, session, r) {
  ns <- session$ns
  
  NGL_file <- reactive({
    if(is.null(r$structure$structure)){ #reset in NGLViewerOuput module
      structure <- r$fileInput$structure
    } else {
      structure <- r$structure$structure
    }
    if(is.null(r$surface$surface)){ #reset in NGLViewerOuput module
      surface <- r$fileInput$structure
    } else {
      surface <- r$surface$surface
    }
    
    if(is.null(r$ligand$ligand)){ #reset in NGLViewerOuput module
      ligand <- r$fileInput$ligand
    } else {
      ligand <- r$ligand$ligand
    }
    if(is.null(r$stage$stage)){ #reset in NGLViewerOuput module
      stage <- r$fileInput$stage
    } else {
      stage <- r$stage$stage
    }
    if(is.null(r$selection$selections)){ #reset in NGLViewerOuput module
      selections <-r$fileInput$selections
    } else {
      selections <- r$selection$selections
    }
    if(is.null(r$contact$contacts)){ #reset in NGLViewerOuput module
      contacts <-r$fileInput$contacts
    } else {
      contacts <- r$contact$contacts
    }
    if(is.null(r$label$labels)){ #reset in NGLViewerOuput module
      labels <-r$fileInput$labels
    } else {
      labels <- r$label$labels
    }
    
    NGL_file <- c(
      "#STRUCTURE\n", if (!is.null(structure)) {
        readr::format_csv(structure)
      },
      "#SURFACE\n", if (!is.null(surface)) {
        readr::format_csv(surface)
      },
      "#LIGAND\n", if (!is.null(ligand)) {
        readr::format_csv(ligand)
      },
      "#STAGE\n", if (!is.null(stage)) {
        readr::format_csv(stage)
      },
      "#SELECTIONS\n", if (!is.null(selections)) {
        readr::format_csv(selections)
      },
      "#LABELS\n", if (!is.null(labels)) {
        readr::format_csv(labels)
      },
      "#CONTACTS\n", if (!is.null(contacts)) {
        readr::format_csv(contacts)
      },
      "#PDB\n", r$PDB
    )
    return(NGL_file)
  })

  output$downloadNGL <- downloadHandler(
    
    filename = function() {
      sprintf("%s.ngl", isolate(r$fileInput$name))
    },
    contentType = "ngl",
    content = function(file) {
      writeLines(NGL_file(), file, sep = "")
    }
  )
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
