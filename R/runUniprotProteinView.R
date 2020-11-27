#' Creates Shiny app
#'
#' Will create an interactive shiny app with imbeded plotly figure
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @references
#' TODO references
#'
#' @export
#' @import shiny
runUniprotProteinDraw <- function() {
  #appDir <- system.file("shiny-scripts",
  #                      package = "MPLNClust")
  shiny::runApp("./inst/shiny-scripts", display.mode = "normal")
  return()
}


