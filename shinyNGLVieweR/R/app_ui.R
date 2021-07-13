#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import colourpicker
#' @import bsplus
#' @import shinyWidgets
#' @import shinyjs
#' @import r2d3
#' @import NGLVieweR
#' @import shinyjqui
#' @import uuid
#' @import readr
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
      title = "NGLVieweR",
      skin = "black",
      dashboardHeader(
        title = HTML("NGLVieweR V0.1"),
        tags$li(
          class = "dropdown",
          id = "github_logo",
          actionLink("github",
            label = NULL, icon = icon("fas fa-github fa-lg"),
            onclick = "window.open('https://nvelden.github.io/NGLVieweR/index.html', '_blank')"
          )
        )
      ),
      dashboardSidebar(
        minified = FALSE,
        sidebarMenu(
            mod_fileInput_ui("fileInput_ui_1"),
            mod_fileOutput_ui("fileOutput_ui_1"),
            mod_examples_ui("examples_ui_1"),
            mod_structure_ui("structure_ui_1"),
            mod_surface_ui("surface_ui_1"),
            mod_ligand_ui("ligand_ui_1"),
            mod_selection_ui("selection_ui_1"),
            mod_label_ui("label_ui_1"),
            mod_contact_ui("contact_ui_1"),
            mod_stage_ui("stage_ui_1"),
            mod_snapshot_ui("snapshot_ui_1"),
            mod_sidebarcontrols_ui("sidebarcontrols_ui_1")
        )
      ),
      dashboardBody(
        fillPage(
          mod_labelcontrols_ui("labelcontrols_ui_1"),
          mod_sequenceOutput_ui("sequenceOutput_ui_1"),
          mod_NGLVieweROutput_ui("NGLVieweROutput_ui_1")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
    
  )
  
  tags$head(
    favicon(
      ext='ico'
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "shinyNGLVieweR"
    ),
    useShinyjs(),
    extendShinyjs(text = jsboxCollapse, functions = c("collapse")), #Collapse box when clicking on title
    # Input modals
    bsplus::use_bs_tooltip(),
    bsplus::use_bs_popover(),
    bs_input_modal("select_modal", "Selection Language", htmlTemplate(app_sys("app/www/selectionModal.html")), "medium"),
    bs_input_modal("contact_modal", "Contact Selection", htmlTemplate(app_sys("app/www/contactModal.html")), "medium")
  )
}


