#' selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menuItem("selection",
      icon = icon("sliders-h"),
      bs_textInput(ns("selection"), "Select", placeholder = "e.g. 20-30 OR <NG>", id_modal = "select_modal"),
      selectInput(ns("selectionType"), "Type", list(
        "ball+stick", "cartoon", "surface", "ribbon", "backbone", "licorice", "spacefill", "line", "contact",
        "helixorient", "hyperball", "rocket"
      )),
      selectInput(ns("selectionColorScheme"), "Scheme", c("uniform", "element", "resname", "random", "residueindex"), selected = "element"),
      colourpicker::colourInput(ns("selectionColor"), label = "Color", "#00FF00", palette = "limited", closeOnClick = TRUE),
      sliderInput(ns("selectionOpacity"), "Opacity", min = 0, ticks = FALSE, max = 1, value = 1),
      textInput(ns("selName"), label = "Name"),
      actionButton(ns("addSelection"), "Save"),
      tags$div(id = "selectionPlaceholder", style = "padding-bottom: 15px; overflow-y: auto; max-height:200px;")
    )
  )
}
    
#' selection Server Function
#'
#' @noRd 
mod_selection_server <- function(input, output, session, globalSession, r) {
  ns <- session$ns

  Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)

  # Inputs for sequenceOutput module
  observeEvent(input$selection, {
    r$selection$selectionInput <- input$selection
  })
  observeEvent(input$selectionColorScheme, {
    r$selection$selectionColorScheme <- input$selectionColorScheme
  })
  observeEvent(input$selectionColor, {
    r$selection$selectionColor <- input$selectionColor
  })

  # Load UI component from .ngl file and bind to component data.frame
  observeEvent(r$sidebarItemExpanded, {
    if (r$selection$loaded == FALSE && (r$sidebarItemExpanded == "selection")) { # reset in mod_NGLVieweROutput

      # Reset input values
      updateTextInput(session, "selection", value = "")
      updateSelectInput(session, "selectionType", selected = "ball+stick")
      updateSelectInput(session, "selectionColorScheme", selected = "uniform")
      colourpicker::updateColourInput(session, "selectionColor", value = "#00FF00")
      updateSliderInput(session, "selectionOpacity", value = 1)
      updateTextInput(session, "selName", value = "")

      # loadUI components
      loadUI_component(r$fileInput$selections, "selection")

      # load data
      r$selection$selections <- r$fileInput$selections
      r$selection$loaded <- TRUE
    }
  })

  # Update structure
  observe({
    # start observer when selection has loaded
    req(r$selection$loaded)


    Viewer_proxy %>% removeSelection("selection")

    if (input$selection == "") {
      selection <- "none"
    } else {
      selection <- selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$selection)
    }

    Viewer_proxy %>% addSelection(input$selectionType,
      param = list(
        name = "selection",
        sele = selection,
        opacity = input$selectionOpacity,
        colorScheme = input$selectionColorScheme,
        colorValue = input$selectionColor
      )
    )
  })

  # Add UI component
  # reactiveCounter to number selections
  r$selection <- reactiveValues(counter = 0)

  observeEvent(input$addSelection, {
    selection <- insertUI_selection(paste0(r$sequence_df$AA, collapse = ""), isolate(input$selection))

    if (selection != "none") {
      r$selection$counter <- r$selection$counter + 1
      name <- insertUI_name("selection", isolate(input$selName), counter = r$selection$counter)
      uu_id <- uuid::UUIDgenerate()

      # save selection data
      new_sel <- data.frame(
        row = r$selection$counter,
        type = "selection",
        id = sprintf("selection-%s", uu_id),
        name = name,
        selection = selection,
        colorValue = isolate(input$selectionColor),
        colorScheme = isolate(input$selectionColorScheme),
        opacity = isolate(input$selectionOpacity),
        structureType = isolate(input$selectionType),
        stringsAsFactors = FALSE
      )

      # Add selection to structure
      Viewer_proxy %>% addSelection(isolate(input$selectionType),
        param = list(
          name = sprintf("selection-%s", uu_id),
          sele = selection,
          colorScheme = isolate(input$selectionColorScheme),
          colorValue = isolate(input$selectionColor),
          opacity = isolate(input$selectionOpacity)
        )
      )

      # reset inputs
      reset("selection")
      reset("selName")

      # Add UI component
      insertUI_component("selection", name, uu_id = uu_id)
      r$selection$selections <- rbind(r$selection$selections, new_sel)
    }
  })

  # Remove UI_component
  observeEvent(r$selection$selectionRemove_id, {
    r$selection$selections <- removeUI_component(Viewer_proxy, r$selection$selections, "selection", r$selection$selectionRemove_id)
    reset("selection")
    reset("selName")
  })

  # update selection values when selection link is clicked
  observeEvent(r$selection$selectionLink_id, {
    uu_id <- str_replace(r$selection$selectionLink_id, "selectionLink-", "")
    id <- sprintf("selection-%s", uu_id)
    data <- r$selection$selections[r$selection$selections$id == id, ]

    updateTextInput(session, "selection", value = data$selection)
    updateSelectInput(session, "selectionType", selected = data$structureType)
    updateSelectInput(session, "selectionColorScheme", selected = data$colorScheme)
    colourpicker::updateColourInput(session, "selectionColor", value = data$colorValue)
    updateTextInput(session, "selName", value = data$name)
    updateSliderInput(session, "selectionOpacity", value = data$opacity)
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
