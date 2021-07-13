#' Load stage from a .ngl file
#'
#' @description
#' Load stage from a .ngl file
#'
#' @param NGLVieweR NGLVIeweR object.
#' @param stage data.frame of selections loaded from .ngl file.
#'
#' @import NGLVieweR
#' @export
loadStage <- function(NGLVieweR, stage) {
  viewer <- NGLVieweR
  if (!is.null(stage)) {
    viewer <- stageParameters(
      viewer,
        cameraType = stage$cameraType,
        backgroundColor = stage$backgroundColor,
        lightIntensity = stage$lightIntensity,
        clipNear = stage$clipNear,
        clipFar = stage$clipFar
      )
  } else{
    viewer <- stageParameters(
      viewer,
      cameraType = "perspective",
      backgroundColor = "black",
      lightIntensity = 1,
      clipNear = 0,
      clipFar = 100
    )
  }
  return(viewer)
}