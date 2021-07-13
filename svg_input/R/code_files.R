

write_svg_input_r_lines <- function(svg_lines, svg_input_name, class_name, 
                                    include_css = TRUE) {
  
  lines <- c(
    "",
    paste0(svg_input_name, ' <- function(inputId, svg) {'),
    "  ",
    '  svg <- "',
    paste0("    ", svg_lines),
    '  "',
    "  ",
    "  tagList(",
    '    singleton(tags$head(',
    paste0('      tags$script(src = "', svg_input_name, '.js")', 
           ifelse(include_css, ',', '')),
    if (include_css) {
      paste0('      htmltools::includeCSS(path = "www/', svg_input_name, '.css")')
    },
    '    )),',
    '    tags$div(',
    paste0('      class = "', class_name, '",'),
    '      id = inputId,',
    '      `data-input-id` = inputId,',
    '      shiny::HTML(svg)',
    '    )',
    '  )',
    '}'
  )
  
  lines
}

write_svg_input_r_file <- function(lines, svg_input_name, path = ".") {
  file_path <- file.path(path, paste0(svg_input_name, ".R"))
  writeLines(lines, file_path)
  
  file_path
}



write_svg_input_shiny_lines <- function(svg_input_name, r_file = NULL) {
  
  if (is.null(r_file)) {
    r_file <- paste0(svg_input_name, ".R")
  }
  
  lines <- c(
    "library(shiny)",
    "",
    paste0('source("', r_file, '", local = TRUE)'),
    "",
    "ui <- fluidPage(",
    '  textOutput("selected_svg"),',
    paste0('  ', svg_input_name, '("input_id")'),
    ")",
    "",
    "server <- function(input, output, session) {",
    "  ",
    "  output$selected_svg <- renderPrint({ input$input_id })",
    "  ",
    "}",
    "",
    "shinyApp(ui = ui, server = server)"
  )
  
  lines
}

write_svg_input_shiny_file <- function(lines, svg_input_name, path = ".") {
  file_path <- file.path(path, paste0("app_", svg_input_name, ".R"))
  writeLines(lines, file_path)
  
  file_path
}


write_js_binding_lines <- function(svg_input_id, class_name) {
  lines <- c(
    'var shinyBinding = new Shiny.InputBinding();',
    '$.extend(shinyBinding, {',
    '  ',
    '  find: function find(scope) {',
    paste0('    return $(scope).find(".', class_name, '")'),
    '  },',
    '  ',
    '  // get the data-svginput of the element with class selected as shiny input',
    '  getValue: function getValue(el) {',
    paste0("    var value = $(el).find('.selected').data('", svg_input_id, "')"),
    '    console.log(value)',
    '    return value',
    '  },',
    '',
    '  subscribe: function(el, callback) {',
    '    $(el).on("click.shinyBinding", function(evt) {',
    '      // remove all of the selected classes inside our element',
    '      $(el).find(".selected").removeClass("selected");',
    '      // set the selected class to the closest clicked part',
    "      //console.log($(evt.target).attr('id'))",
    "      $(evt.target).addClass('selected');",
    '      callback();',
    '    })',
    '  },',
    '  unsubscribe: function(el) {',
    '    $(el).off(".shinyBinding");',
    '  }',
    '});',
    '',
    'Shiny.inputBindings.register(shinyBinding);'
  )
  
  lines 
}

write_js_binding_file <- function(lines, svg_input_name, path = "www") {
  file_path <- file.path(path, paste0(svg_input_name, ".js"))
  writeLines(lines, file_path)
  
  file_path
}