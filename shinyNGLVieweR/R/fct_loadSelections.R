#' Load selections from a .ngl file
#' 
#' @description    
#' Load selections from a .ngl file
#'
#' @param NGLVieweR NGLVIeweR object.
#' @param selections data.frame of selections loaded from .ngl file.
#' 
#' @import NGLVieweR
#' @export
loadSelections <- function(NGLVieweR, selections) {
  viewer <- NGLVieweR
  if (!is.null(selections)) {
    for (n in 1:nrow(selections)) {
      viewer <- addRepresentation(viewer, selections[n, "structureType"],
        param =
          list(
            name = selections[n, "id"],
            colorValue = selections[n, "colorValue"],
            colorScheme = selections[n, "colorScheme"],
            opacity = as.numeric(selections[n, "opacity"]),
            sele = selections[n, "selection"]
          )
      )
    }
  }
  return(viewer)
}

