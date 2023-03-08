#' Load structure from a .ngl file
#'
#' @description
#' Load structure from a .ngl file
#'
#' @param NGLVieweR NGLVIeweR object.
#' @param structure data.frame of structure loaded from .ngl file.
#' @param format File 
#'
#' @import NGLVieweR
#' @export
loadStructure <- function(NGLVieweR, structure, format = NULL) {
  viewer <- NGLVieweR
  if (is.null(structure)) {
    if(format == "pdb" || format == "ngl" || is.null(format)){
      representation <- "cartoon"
    } else {
      representation <- "ball+stick"
    }
    viewer <- addRepresentation(viewer, representation, param = list(
      name = "structure",
      colorScheme = "residueindex"
    ))
  } else if (!is.null(structure) && !structure$type == "hide") {
    viewer <- addRepresentation(viewer,
      structure$type,
      param =
        list(
          name = "structure",
          colorScheme = structure$colorScheme,
          colorValue = structure$colorValue,
          sele = structure$selection,
          visible = structure$visible
        )
    )
  } else if (structure$type == "hide") {
    viewer <- addRepresentation(viewer, "cartoon", param = list(
      name = "structure",
      colorScheme = structure$colorScheme,
      colorValue = structure$colorValue,
      sele = structure$selection,
      visible = structure$visible
    ))
  } else {
    viewer <- NGLVieweR
  }
  return(viewer)
}