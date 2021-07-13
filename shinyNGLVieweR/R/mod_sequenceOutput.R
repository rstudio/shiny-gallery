#' sequenceOutput UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sequenceOutput_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("boxPlaceholder"), style = "z-index:100;",
      box(
        id = "sequence", title = actionLink(ns("sequenceTitle"), "Sequence"), status = "primary", color = "black", solidHeader = TRUE, align = "left",
        width = 12, collapsible = TRUE, collapsed = TRUE,
        uiOutput(ns("chaininput")),
        d3Output(ns("PDBsequence"))
      )
    )
  )
}

#' sequenceOutput Server Function
#'
#' @noRd
mod_sequenceOutput_server <- function(input, output, session, globalSession, r) {
  ns <- session$ns

  # <----------------------------------------Sequence Output --------------------------------------------->
  output$PDBsequence <- renderD3({
    r2d3(
      data = r$sequence_df,
      options = list(
        seqPositions = ngl_to_position(paste(r$sequence_df$AA, collapse = ""), r$selection$selectionInput),
        parentId = "sequence" # needed for tooltip placement
      ),
      d3_version = c("5"),
      container = "div",
      script = app_sys("app/www/sequenceOutput.js")
    )
  })
  # <----------------------------------------UI inputs --------------------------------------------->
  # Show/hide sequence box
  observeEvent(r$sidebarcontrols$showSequence, {
    if (r$sidebarcontrols$showSequence) {
      shinyjs::show("boxPlaceholder")
    } else {
      shinyjs::hide("boxPlaceholder")
    }
  })

  # Unfold box on title click
  observeEvent(input$sequenceTitle, {
    js$collapse("sequence")
  })

  output$chaininput <- renderUI({
    if (length(unique(r$chainname[!is.na(r$chainname)])) != 1) {
      tags$div(
        style = "display: flex;", id = "chainHolder",
        tags$div(
          style = "padding: 5px;",
          tags$label("Sequence of")
        ),
        tags$div(
          style = "width:100px;",
          selectizeInput(ns("selectedChain"), label = NULL, r$chainname, selected = "element", width = "100%")
        )
      )
    } else {
      # Needed such that sequence loads after chain has been registered
      hidden(tags$div(
        style = "display: flex;", id = "chainHolder",
        tags$div(
          style = "padding: 5px;",
          tags$label("Sequence of")
        ),
        tags$div(
          style = "width:100px;",
          selectizeInput(ns("selectedChain"), label = NULL, r$chainname, selected = "element", width = "100%")
        )
      ))
    }
  })
  # <---------------------------------------- Observers --------------------------------------------->
  
  observeEvent(input$selectedChain,{
    r$sequenceOutput$selectedChain <- input$selectedChain 
  })

  Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)

  observe({

    # make reactive
    if (r$aa_clicked == "" || is.null(r$aa_clicked)) {
      Viewer_proxy %>%
        updateSelection("aa_clicked", sele = "none")
    } else {
      Viewer_proxy %>%
        updateSelection("aa_clicked", sele = aa_clicked_to_ngl(r$aa_clicked, r$chainname, input$selectedChain))
    }
  })

  observe({
    Viewer_proxy %>%
      updateRepresentation("aa_clicked",
        param = list(
          colorValue = r$selection$selectionColor,
          colorScheme = r$selection$selectionColorScheme
        )
      )
  })
}

## To be copied in the UI
#

## To be copied in the server
#
