#' Shiny App Example
#'
#' Shiny App presenting the package functionality. Select one of the 100 images in the top left to see the eigenface representation, similar faces and the contribution of each eigenface to the representation.
#'
#' @examples
#' \dontrun{
#' runShiny()
#' }
#' @export
runShiny <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "REigenfaces")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `REigenfaces`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
