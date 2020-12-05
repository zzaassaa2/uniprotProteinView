#' Creates Shiny app
#'
#' Will create an interactive shiny app with imbeded plotly figure
#'
#' @references
#' Park T. Bootswatch. (2020). GitHub Repository. \href{https://github.com/thomaspark/bootswatch}{Link}
#'
#' Chang, W., Cheng J., Allaire J., Xie Y., McPherson J. (2017). Shiny: web application framework for R.
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
