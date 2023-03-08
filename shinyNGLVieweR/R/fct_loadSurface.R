#' Load surface from a .ngl file
#'
#' @description
#' Load surface from a .ngl file
#'
#' @param NGLVieweR NGLVIeweR object.
#' @param surface data.frame of surface loaded from .ngl file.
#'
#' @import NGLVieweR
#' @export
loadSurface <- function(NGLVieweR, surface) {
  viewer <- NGLVieweR
  if (!is.null(surface)) {
    viewer <- addRepresentation(viewer,
      "surface",
      param =
        list(
          name = "surface",
          colorScheme = surface$colorScheme,
          colorValue = surface$colorValue,
          sele = surface$selection,
          opacity = surface$opacity,
          visible = surface$visible
        )
    )
  } else {
    if (is.null(surface)) {
      viewer <- addRepresentation(viewer, "surface", param = list(
        visible = FALSE
      ))
    }
  }
  return(viewer)
}