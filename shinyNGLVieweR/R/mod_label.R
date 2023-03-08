#' label UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_label_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menuItem("label",
      icon = icon("sliders-h"),
      selectInput(ns("labelGrouping"), "Grouping", c("residue", "atom")),
      bs_textInput(ns("labelSelection"), "Select", placeholder = 'e.g. "20', id_modal = "select_modal"),
      textInput(ns("labelFormat"), label = "Label", ""),
      actionLink(ns("labelOptions"), "Options",
        class = "btn-info",
        style = "background-color:transparent;"
      ),
      actionButton(ns("addLabel"), "Save"),
      tags$div(id = "labelPlaceholder", style = "padding-bottom: 15px; overflow-y: auto; max-height:200px;")
    )
  )
}
#' label Server Function
#'
#' @noRd 
mod_label_server <- function(input, output, session, globalSession, r) {
  ns <- session$ns
  Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)

  # Input for labelcontrols module
  observeEvent(input$labelOptions, {
    r$label$labelOptions <- input$labelOptions
  })

  # Load UI component from .ngl file and bind to component data.frame
  observeEvent(r$sidebarItemExpanded, {
    if (r$label$loaded == FALSE && (r$sidebarItemExpanded == "label")) { # reset in mod_NGLVieweROutput
      
      #Reset inputs
      updateSelectInput(session, "labelGrouping", selected = "residue")
      updateTextInput(session, "labelSelection", value = "")
      updateTextInput(session, "labelFormat", value = "")
      
      #Load UI components
      loadUI_component(r$fileInput$labels, "label")
      
      #Load data
      r$label$labels <- r$fileInput$labels
      r$label$loaded <- TRUE
    }
  })

  # Update structure
  observe({
    
    req(r$labelcontrols$labelBackground)
    req(r$labelcontrols$textColor)
    req(r$labelcontrols$labelSize)
    req(r$labelcontrols$fixedSize)
    req(r$labelcontrols$xOffset)
    req(r$labelcontrols$yOffset)
    req(r$labelcontrols$zOffset)
    req(r$labelcontrols$labelBackground)
    req(r$labelcontrols$labelbackgroundColor)
    req(r$labelcontrols$labelbackgroundOpacity)
    req(r$label$loaded)

    Viewer_proxy %>% removeSelection("label")

    if (input$labelSelection == "") {
      labelSelection <- "none"
    } else {
      labelSelection <- selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$labelSelection)
    }

    if (r$labelcontrols$labelBackground == "FALSE") {
      labelBackground <- FALSE
    } else {
      labelBackground <- TRUE
    }

    if (r$labelcontrols$fixedSize == "yes") {
      fixedSize <- TRUE
    } else {
      fixedSize <- FALSE
    }

    if (input$labelFormat == "") {
      labelFormat <- "[%(resname)s]%(resno)s"
    } else {
      labelFormat <- input$labelFormat
    }

    Viewer_proxy %>% addSelection("label",
      param = list(
        name = "label",
        sele = labelSelection,
        labelType = "format",
        labelFormat = labelFormat,
        labelGrouping = input$labelGrouping,
        color = r$labelcontrols$textColor,
        showBackground = labelBackground,
        backgroundColor = r$labelcontrols$labelbackgroundColor,
        backgroundOpacity = r$labelcontrols$labelbackgroundOpacity,
        radiusType = 1,
        xOffset = r$labelcontrols$xOffset,
        yOffset = r$labelcontrols$yOffset,
        zOffset = r$labelcontrols$zOffset,
        radiusSize = r$labelcontrols$labelSize,
        fixedSize = fixedSize,
        fontFamily = "sans-serif"
      )
    )
  })

  # Add UI component
  # reactiveCounter to number selections
  r$label <- reactiveValues(counter = 0)

  observeEvent(input$addLabel, {

    selection <- insertUI_selection(paste0(r$sequence_df$AA, collapse = ""), isolate(input$labelSelection))
    if (selection != "none") {
      r$label$counter <- r$label$counter + 1
      name <- insertUI_name("label", isolate(input$labelFormat), counter = r$label$counter)
      uu_id <- uuid::UUIDgenerate()

      if (r$labelcontrols$fixedSize == "yes") {
        fixedSize <- TRUE
      } else {
        fixedSize <- FALSE
      }

      if (r$labelcontrols$labelBackground == "FALSE") {
        labelBackground <- FALSE
      } else {
        labelBackground <- TRUE
      }
      
      if (input$labelFormat == "") {
        labelFormat <- "[%(resname)s]%(resno)s"
      } else {
        labelFormat <- input$labelFormat
      }
      
      # save selection data
      new_lab <- data.frame(
        row = r$label$counter,
        type = "label",
        id = sprintf("label-%s", uu_id),
        name = name,
        labelFormat = labelFormat,
        selection = selection,
        textColor = r$labelcontrols$textColor,
        labelSize = r$labelcontrols$labelSize,
        fixedSize = fixedSize,
        labelGrouping = input$labelGrouping,
        xOffset = r$labelcontrols$xOffset,
        yOffset = r$labelcontrols$yOffset,
        zOffset = r$labelcontrols$zOffset,
        labelBackground = labelBackground,
        labelbackgroundColor = r$labelcontrols$labelbackgroundColor,
        labelbackgroundOpacity = r$labelcontrols$labelbackgroundOpacity,
        stringsAsFactors = FALSE
      )
      
      #add label to data.frame
      r$label$labels <- rbind(r$label$labels, new_lab)

      # Add label to structure
      Viewer_proxy %>% addSelection("label",
        param = list(
          name = sprintf("label-%s", uu_id),
          sele = selection,
          labelType = "format",
          labelFormat = labelFormat,
          labelGrouping = input$labelGrouping,
          color = r$labelcontrols$textColor,
          showBackground = labelBackground,
          backgroundColor = r$labelcontrols$labelbackgroundColor,
          backgroundOpacity = r$labelcontrols$labelbackgroundOpacity,
          radiusType = 1,
          xOffset = r$labelcontrols$xOffset,
          yOffset = r$labelcontrols$yOffset,
          zOffset = r$labelcontrols$zOffset,
          radiusSize = r$labelcontrols$labelSize,
          fixedSize = fixedSize,
          fontFamily = "sans-serif"
        )
      )
      
      insertUI_component("label", name, uu_id = uu_id)
      
      # Add UI component

      # reset inputs
      reset("labelGrouping")
      reset("labelSelection")
      reset("labelFormat")
    }
  })
  
  # Remove UI_component
  observeEvent(r$label$labelRemove_id, {
    r$label$labels <- removeUI_component(Viewer_proxy, r$label$labels, "label", r$label$labelRemove_id)
    reset("labelGrouping")
    reset("labelSelection")
    reset("labelFormat")
  })
  
  # update selection values when selection link is clicked
  observeEvent(r$label$labelLink_id, {
    uu_id <- str_replace(r$label$labelLink_id, "labelLink-", "")
    id <- sprintf("label-%s", uu_id)
    data <- r$label$labels[r$label$labels$id == id, ]
    
    if(data$labelFormat == "[%(resname)s]%(resno)s"){
      labelFormat <- ""
    } else {
      labelFormat <- data$labelFormat 
    }
    
    updateSelectInput(session, "labelGrouping", selected = data$labelGrouping)
    updateTextInput(session, "labelSelection", value = data$selection)
    updateTextInput(session, "labelFormat", value = labelFormat)
    
  })
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
#
 
