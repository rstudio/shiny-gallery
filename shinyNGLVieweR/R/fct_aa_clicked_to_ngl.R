#' Clicked residues to NGL query
#'
#' @description
#' Match clicked residues to chain and transform to NGL query
#'
#' @param selected_aa selected aa in list format
#' @param chainnames all chainnames of the sequence
#' @param selchain selected chainname
#' 
#' @examples
#' \dontrun{
#' NGLVieweR_proxy("structure") %>% updateSelection("aa_clicked", 
#' sele = aa_clicked_to_ngl(aa_clicked(), structure_chainname(), selectedChain()))
#' }
#' @export
aa_clicked_to_ngl <- function(selected_aa, chainnames, selchain = NULL) {
  
  aa_sel <- paste(selected_aa, collapse = " OR ")
  
  if (!is.null(selchain) && nchar(selchain) != 0 && any(chainnames %in% selchain)) {
    output <- sprintf(":%s and (%s)", selchain, aa_sel)
  } else {
    output <- aa_sel
  }
  return(output)
}