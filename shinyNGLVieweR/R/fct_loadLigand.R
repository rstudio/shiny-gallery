#' Load structure from a .ngl file
#'
#' @description
#' Load structure from a .ngl file
#'
#' @param NGLVieweR NGLVIeweR object.
#' @param ligand data.frame of ligand loaded from .ngl file.
#'
#' @import NGLVieweR
#' @export
loadLigand <- function(NGLVieweR, ligand) {
  viewer <- NGLVieweR
  if (!is.null(ligand)) {
    if (ligand$ligand != "hide") {
      viewer <- addRepresentation(viewer,
        ligand$ligand,
        param = list(
          name = "ligand",
          sele = "( not polymer or not ( protein or nucleic ) ) and not ( water or ACE or NH2 or ion)",
          colorScheme = ligand$colorScheme,
          colorValue = ligand$colorValue
        )
      )
    }
    if (!is.na(ligand$waterIon)) {
      if (grepl("Water", ligand$waterIon)) {
        viewer <- addRepresentation(viewer,
          "ball+stick",
          param = list(
            name = "water",
            sele = "water"
          )
        )
      }
      if (grepl("Ion", ligand$waterIon)) {
        viewer <- addRepresentation(viewer,
          "ball+stick",
          param = list(
            name = "ion",
            sele = "ion"
          )
        )
      }
    }
  } else {
    viewer <- NGLVieweR
  }
  return(viewer)
}