#' contact UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_contact_ui <- function(id) {
  ns <- NS(id)
  tagList(
    menuItem("contact",
      id = "contactTab", icon = icon("sliders-h"),
      selectInput(ns("contactLabel"), "Label", c("none", "angstrom", "nm", "no unit")),
      selectizeInput(ns("contactTypes"), "Show", choices = list(
        "hydrogenBond",
        "weakHydrogenBond", "waterHydrogenBond",
        "backboneHydrogenBond", "hydrophobic",
        "halogenBond", "ionicInteraction",
        "metalCoordination", "cationPi",
        "piStacking"
      ), multiple = TRUE),
      bs_textInput(ns("contactFilterA"), "Between", placeholder = "selection A", id_modal = "contact_modal"),
      textInput(ns("contactFilterB"), label = NULL, placeholder = "selection B"),
      textInput(ns("contactName"), label = "Name", placeholder = "optional"),
      actionButton(ns("addContact"), "Save"),
      tags$div(id = "contactPlaceholder", style = "padding-bottom: 15px; overflow-y: auto; max-height:200px;")
    )
  )
}
#' contact Server Function
#'
#' @noRd
mod_contact_server <- function(input, output, session, globalSession, r) {
  ns <- session$ns

  Viewer_proxy <- NGLVieweR_proxy("NGLVieweROutput_ui_1-structure", session = globalSession)

  # Load UI component from .ngl file and bind to component data.frame
  observeEvent(r$sidebarItemExpanded, {
    if (r$contact$loaded == FALSE && (r$sidebarItemExpanded == "contact")) { # reset in mod_NGLVieweROutput

      # Reset input values
      updateSelectInput(session, "contactLabel", selected = "none")
      updateSelectizeInput(session, "contactTypes", selected = "")
      updateTextInput(session, "contactFilterA", value = "")
      updateTextInput(session, "contactFilterB", value = "")
      updateTextInput(session, "contactName", value = "")

      # Load UI components
      loadUI_component(r$fileInput$contacts, "contact")

      # Load data
      r$contact$contacts <- r$fileInput$contacts
      r$contact$loaded <- TRUE
    }
  })

  # Update structure
  observe({

    # Start observer when contacts have loaded
    req(r$contact$loaded)

    if (input$contactFilterA == "" && input$contactFilterB == "") {
      inp_sel <- "none"
    } else {
      inp_sel <- "*"
    }

    if (input$contactLabel == "none") {
      visible <- FALSE
    } else {
      visible <- TRUE
    }

    contactTypes <- list(
      "hydrogenBond",
      "weakHydrogenBond", "waterHydrogenBond",
      "backboneHydrogenBond", "hydrophobic",
      "halogenBond", "ionicInteraction",
      "metalCoordination", "cationPi",
      "piStacking"
    )

    cont_inp <- contactTypes %in% input$contactTypes

    if (is.null(input$contactTypes)) {
      cont_inp[] <- TRUE
    }

    Viewer_proxy %>% removeSelection("contact")

    Viewer_proxy %>% addSelection("contact",
      param =
        list(
          sele = inp_sel,
          filterSele = list(
            selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$contactFilterA),
            selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$contactFilterB)
          ),
          labelVisible = visible,
          labelUnit = input$contactLabel,
          hydrogenBond = cont_inp[1],
          weakHydrogenBond = cont_inp[2],
          waterHydrogenBond = cont_inp[3],
          backboneHydrogenBond = cont_inp[4],
          hydrophobic = cont_inp[5],
          halogenBond = cont_inp[6],
          ionicInteraction = cont_inp[7],
          metalCoordination = cont_inp[8],
          cationPi = cont_inp[9],
          piStacking = cont_inp[10]
        )
    )
  })

  # Add UI component
  # reactiveCounter to number selections
  r$contact <- reactiveValues(counter = 0)

  observeEvent(input$addContact, {
    if (isolate(input$contactFilterA) == "" && isolate(input$contactFilterB == "")) {
      selection <- "none"
    } else {
      selection <- "*"
    }

    if (isolate(input$contactLabel) == "none") {
      visible <- FALSE
    } else {
      visible <- TRUE
    }

    contactTypes <- list(
      "hydrogenBond",
      "weakHydrogenBond", "waterHydrogenBond",
      "backboneHydrogenBond", "hydrophobic",
      "halogenBond", "ionicInteraction",
      "metalCoordination", "cationPi",
      "piStacking"
    )

    cont_inp <- contactTypes %in% isolate(input$contactTypes)

    if (is.null(input$contactTypes)) {
      cont_inp[] <- TRUE
    }

    if (selection != "none") {
      r$contact$counter <- r$contact$counter + 1
      name <- insertUI_name("contact", isolate(input$contactName), counter = r$contact$counter)
      uu_id <- uuid::UUIDgenerate()

      new_contact <- data.frame(
        row = r$contact$counter,
        type = "contact",
        id = sprintf("contact-%s", uu_id),
        name = name,
        selection = selection,
        selectionA = isolate(input$contactFilterA),
        selectionB = isolate(input$contactFilterB),
        labelUnit = isolate(input$contactLabel),
        labelVisible = visible,
        contactTypes = paste(input$contactTypes, collapse = ","),
        stringsAsFactors = FALSE
      )

      # add contact to data.frame
      r$contact$contacts <- rbind(r$contact$contacts, new_contact)

      # Add contact
      Viewer_proxy %>% addSelection("contact",
        param =
          list(
            name = paste0("contact-", uu_id, sep = ""),
            sele = selection,
            filterSele = list(
              selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$contactFilterA),
              selection_to_ngl(paste0(r$sequence_df$AA, collapse = ""), input$contactFilterB)
            ),
            labelVisible = visible,
            labelUnit = isolate(input$contactLabel),
            hydrogenBond = cont_inp[1],
            weakHydrogenBond = cont_inp[2],
            waterHydrogenBond = cont_inp[3],
            backboneHydrogenBond = cont_inp[4],
            hydrophobic = cont_inp[5],
            halogenBond = cont_inp[6],
            ionicInteraction = cont_inp[7],
            metalCoordination = cont_inp[8],
            cationPi = cont_inp[9],
            piStacking = cont_inp[10]
          )
      )

      # reset contact selections
      reset("contactFilterA")
      reset("contactFilterB")
      reset("contactName")

      insertUI_component("contact", name, uu_id = uu_id)
    }
  })

  # Remove UI_component
  observeEvent(r$contact$contactRemove_id, {
    r$contact$contacts <- removeUI_component(Viewer_proxy, r$contact$contacts, "contact", r$contact$contactRemove_id)
    reset("contactFilterA")
    reset("contactFilterB")
    reset("contactName")
  })

  # update selection values when selection link is clicked
  observeEvent(r$contact$contactLink_id, {
    uu_id <- str_replace(r$contact$contactLink_id, "contactLink-", "")
    id <- sprintf("contact-%s", uu_id)
    data <- r$contact$contacts[r$contact$contacts$id == id, ]
    contactTypes <- unlist(str_split(data$contactTypes, ","))

    updateSelectInput(session, "contactLabel", selected = data$labelUnit)
    updateTextInput(session, "contactTypes", value = contactTypes)
    updateTextInput(session, "contactFilterA", value = data$selectionA)
    updateTextInput(session, "contactFilterB", value = data$selectionB)
    updateTextInput(session, "contactName", value = data$name)
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
