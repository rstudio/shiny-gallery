#' Load labels from a .ngl file
#' 
#' @description    
#' Load labels from a .ngl file
#'
#' @param NGLVieweR NGLVIeweR object.
#' @param labels data.frame of labels loaded from .ngl file.
#' 
#' @import NGLVieweR
#' @export
loadLabels <- function(NGLVieweR, labels) {
  viewer <- NGLVieweR
  if (!is.null(labels)) {
    for (n in 1:nrow(labels)) {
      viewer <- addRepresentation(viewer, "label",
                                     param =
                                       list(
                                         name = labels[n, "id"],
                                         sele = paste(labels[n, "selection"]),
                                         labelType = "format",
                                         labelFormat = labels[n, "labelFormat"],
                                         labelGrouping = labels[n, "labelGrouping"],
                                         color = labels[n, "textColor"],
                                         showBackground = labels[n, "labelBackground"],
                                         backgroundColor = labels[n, "labelbackgroundColor"],
                                         backgroundOpacity = labels[n, "labelbackgroundOpacity"],
                                         radiusType = 1,
                                         xOffset = labels[n, "xOffset"],
                                         yOffset = labels[n, "yOffset"],
                                         zOffset = labels[n, "zOffset"],
                                         radiusSize = labels[n, "labelSize"],
                                         fixedSize = labels[n, "fixedSize"],
                                         fontFamily = "sans-serif"
                                       )
      )
    }
  }
  return(viewer)
}