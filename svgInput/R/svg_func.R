# MAKE SURE ONLY ' (SINGLE) QUOTATIONS ARE IN THE SVG NONE " (DOUBLE)
svg_lines_to_tibble <- function(svg_lines) {
  
  basic_shapes <- c("rect", "circle", "ellipse", "line", "polyline", "polygon", "path")
  
  svg <- svg_lines %>% 
    stringr::str_split("<") %>% 
    unlist() %>% 
    { tibble::tibble(line = .) } %>% 
    dplyr::mutate(line      = stringr::str_trim(line)) %>% 
    dplyr::filter(line != "") %>% 
    dplyr::mutate(line      = stringr::str_replace_all(line, '"', "'")) %>% 
    dplyr::mutate(line      = paste0("<", line)) %>% 
    dplyr::mutate(svg_start = stringr::str_sub(line, 1, 5) == "<svg ") %>% 
    dplyr::mutate(svg_end   = stringr::str_sub(line, 1, 5) == "</svg") %>% 
    # Line between svg tags
    dplyr::mutate(svg_in    = !svg_start & !svg_end & cumsum(svg_start) != cumsum(svg_end)) %>% 
    # Actual tag, not closing a tag or a comment
    dplyr::mutate(ele_tag   = stringr::str_detect(line, '<*[A-z]\\s[A-z]') & 
                    stringr::str_sub(line, 1, 4) != "<!--" &
                    svg_in) %>% 
    dplyr::mutate(ele_name  = dplyr::if_else(
      !ele_tag, "",
      stringr::str_sub(stringr::str_extract(line, "[^ ]+"), 2, -1))) %>% 
    # Closing tags
    dplyr::mutate(other_end = stringr::str_sub(line, 1, 2) == "</" & !svg_end)
  
  starts_of_other_ends <- svg %>%
    dplyr::filter(other_end) %>% 
    dplyr::pull(line) %>% 
    unique() %>% 
    stringr::str_extract_all("[^\\s>]+") %>% 
    stringr::str_replace("/", "") %>% 
    paste0(" ")
  
  svg <- svg %>% 
    dplyr::mutate(other_start = purrr::map_lgl(line, function(line_) {
      any(purrr::map_lgl(starts_of_other_ends, ~ stringr::str_sub(line_, 1, nchar(.)) == .))
    })) %>% 
    dplyr::mutate(level = 1 + cumsum(as.numeric(svg_start) + as.numeric(other_start)) - 
                    cumsum(as.numeric(svg_end) + as.numeric(other_end)) - 
                    (as.numeric(svg_start) + as.numeric(other_start)))
  
  svg <- svg %>% 
    dplyr::mutate(shiny_class = paste0("svg_temp_", seq_len(dplyr::n()))) %>% 
    # Basic shapes and svg tags should be available in Shiny
    dplyr::mutate(shiny_lgl   = svg_start | ele_name %in% basic_shapes) %>% 
    dplyr::mutate(shiny_line  = purrr::pmap_chr(list(line, shiny_class, shiny_lgl), function(line_, sc_, lgl_) {
      if (lgl_) { line_ <- stringr::str_replace(line_, " ", paste0(" class='", sc_, "' ")) }
      line_
    })) %>% 
    dplyr::mutate(shiny_value = NA_character_)
  
  svg
}


