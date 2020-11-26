
#'
#' @import shiny plotly httr XML
#' @export
runUniprotProteinDraw <- function() {
  #appDir <- system.file("shiny-scripts",
  #                      package = "MPLNClust")
  shiny::runApp("./inst/shiny-scripts", display.mode = "normal")
  return()
}

#runUniprotProteinDraw()

