#' Load sequence data.frame
#'
#' @description
#' Load sequence data.frame for D3 sequence input
#'
#' @param sequence sequence in list form.
#' @param resno amino acid numbers in list format
#' @param chainname chainname for each amino acid position in list format
#' @param selchain filter data for selected chain. Defaults to \code{NULL}
#' 
#' @import dplyr
#' @importFrom tidyr complete replace_na
#' @export
sequence_df <- function(sequence, resno, chainname, selchain = NULL){
  
  if(is.null(sequence) || is.null(chainname) || is.null(chainname) ){
    return(NULL)
  }
  seq_df <- data.frame(
    AA = sequence,
    Nr = as.numeric(resno),
    chainname = chainname,
    stringsAsFactors = FALSE
  )
  #Change 0 values to 1
  seq_df <- seq_df %>% dplyr::mutate(Nr=replace(.data$Nr, .data$Nr==0, 1))
  
  if(!is.null(selchain) && nchar(selchain) != 0 && any(seq_df$chainname %in% selchain)){
    seq_df <- seq_df %>% dplyr::filter(chainname == selchain)
  }
  
  #check if AA are unique
  seq_df <- seq_df %>% dplyr::group_by(chainname, .data$Nr) %>% mutate(unique = n())
  seq_df <- seq_df %>%
    dplyr::mutate(
      unique = dplyr::case_when(
        unique == 1 ~TRUE,
        TRUE ~FALSE
      )
    ) %>% dplyr::ungroup()
  
  #keey unique values only
  seq_df <- seq_df %>% dplyr::distinct(.data$Nr, chainname, .keep_all = T)
  
  #Complete sequence 
  seq_df <- seq_df %>% tidyr::complete(Nr = 1:max(.data$Nr), fill = list(AA="-"))
  seq_df <- seq_df %>% tidyr::replace_na(list(unique = TRUE))
  
  #divide positions in blocks of 10
  roundUp <- function(x,to=10)
  {
    to*(x%/%to + as.logical(x%%to))
  }
  seq_df <- seq_df %>% dplyr::mutate(
    block = roundUp(.data$Nr)
  )
  #change max block number to max sequence nr
  seq_df <- seq_df %>%
    dplyr::group_by(.data$block) %>%
    dplyr::mutate(
      block = dplyr::case_when(
        max(.data$block) != max(.data$Nr) ~max(.data$Nr),
        TRUE ~block
      )
    ) %>% dplyr::ungroup()
  
  #remove max block Nr if block smaller then 5
  seq_df <- seq_df %>%
    dplyr::group_by(.data$block) %>%
    dplyr::mutate(
      count = n(),
      block = dplyr::case_when(
        count < 5 ~"&nbsp;",
        TRUE ~as.character(.data$block)
      )
    ) %>% dplyr::select(., -count)
  
  #Remove empty blocks
  seq_df <- seq_df %>% 
    dplyr::group_by(.data$block) %>%
    dplyr::mutate(count = 
             sum(str_count(.data$AA, "-"))
    ) %>%
    dplyr::filter(
      count != 10
    ) %>% 
    dplyr::select(-count)

  return(seq_df)
}