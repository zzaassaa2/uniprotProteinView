#' Creates Shiny app
#'
#' Will create an interactive shiny app with imbeded plotly figure
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @export
#' @import shiny
runUniprotProteinDraw <- function() {
  appDir <- system.file("shiny-scripts", package = "uniprotProteinView")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}
