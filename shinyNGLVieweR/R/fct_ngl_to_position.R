#' AA positions from NGL query
#'
#' @description
#' Find all AA positions in protein sequence from a NGL query. Returns a list of all matched positions. 
#'
#' @param sequence Protein sequence in string format
#' @param input sequence to find the positions for
#' 
#' @examples 
#' ngl_to_position("ALAAGSDFG", "1-5 OR <DFG>")
#' @import stringr
#' @importFrom dplyr rowwise mutate
#' @importFrom tidyr full_seq unnest
#' @export
ngl_to_position <- function(sequence, input){
  
  #Get string matches
  strings <- stringr::str_extract_all(input, "(?<=\\<)(.*?)(?=\\>)", simplify = TRUE)[1,]
  
  #Get sequence matches
  seq_match <- stringr::str_extract_all(input, '\\d+-\\d+', simplify = TRUE) [1,]
  if(length(seq_match) != 0){
    seq_match <- stringr::str_replace_all(seq_match, "-", ":")
    seq_match <- unlist(lapply(seq_match, function(x) {eval(parse(text = x))}))
    seq_match <- unique(seq_match)
    input <- stringr::str_replace_all(input, '\\d+-\\d+', "")
  } else {
    seq_match <- NULL
  }
  #get numeric matches
  numeric_match <- as.numeric(stringr::str_extract_all(input, '[0-9]+', simplify = TRUE)[1,])
  
  numeric <- unique(c(seq_match, numeric_match))
  
  #Return NULL if no numeric or string matches
  if(length(strings) == 0 && length(numeric) == 0){
    return(NULL)
  } else if(length(numeric) != 0 && length(strings) == 0) {
    return(numeric)
  } else {
    #Find positions
    matchPositions <- stringr::str_locate_all(sequence, strings)
    matchPositions <- do.call(rbind, matchPositions)
    if(nrow(matchPositions) == 0){
      return(NULL)
    }
    matchPositions <- data.frame(matchPositions) %>% dplyr::rowwise() %>% dplyr::mutate(sequence = list(tidyr::full_seq(c(.data$start, .data$end), 1)))
    matchPositions <- tidyr::unnest(matchPositions, sequence)
    
    positions <- sort(unique(matchPositions$sequence))
    
    if(length(numeric != 0)){
      positions <- sort(unique(c(positions, numeric)))
    }
    
    return(positions)
  }
}