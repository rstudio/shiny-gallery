#' Load contacts from a .ngl file
#' 
#' @description    
#' Load contacts from a .ngl file
#'
#' @param NGLVieweR NGLVIeweR object.
#' @param contacts data.frame of contacts loaded from .ngl file.
#' @param contactTypes list of all different contactTypes that should be matched.
#' 
#' @import NGLVieweR
#' @import stringr
#' @export
loadContacts <- function(NGLVieweR, contacts, contactTypes = list(
                             "hydrogenBond",
                             "weakHydrogenBond", "waterHydrogenBond",
                             "backboneHydrogenBond", "hydrophobic",
                             "halogenBond", "ionicInteraction",
                             "metalCoordination", "cationPi",
                             "piStacking"
                           )) {
  viewer <- NGLVieweR
  if (!is.null(contacts)) {
    for (n in 1:nrow(contacts)) {

      # get selected contactTypes
      cont_inp <- unlist(str_split(contacts[n, "contactTypes"], ","))
      cont_df <- contactTypes %in% cont_inp
      if (is.na(cont_inp)) {
        cont_df[] <- TRUE
      }
      viewer <- addRepresentation(viewer, "contact",
        param =
          list(
            name = paste(contacts[n, "id"]),
            sele = paste(contacts[n, "selection"]),
            filterSele = list(
              contacts[n, "selectionA"],
              contacts[n, "selectionB"]
            ),
            labelVisible = contacts[n, "labelVisible"],
            labelUnit = contacts[n, "labelUnit"],
            hydrogenBond = cont_df[1],
            weakHydrogenBond = cont_df[2],
            waterHydrogenBond = cont_df[3],
            backboneHydrogenBond = cont_df[4],
            hydrophobic = cont_df[5],
            halogenBond = cont_df[6],
            ionicInteraction = cont_df[7],
            metalCoordination = cont_df[8],
            cationPi = cont_df[9],
            piStacking = cont_df[10]
          )
      )
    }
  }
  return(viewer)
}