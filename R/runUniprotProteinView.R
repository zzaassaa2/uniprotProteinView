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
  shiny::runApp("./inst/shiny-scripts", display.mode = "normal")#todo dev only
  return()
}

#' Retrieved data using webpage data
#'
#' Will input a wepages sources, and then get all data and return the XML data and features data frame
#'
#' @param apProData Predownload webpage data using confirmStatus function
#'
#' @return List of XML and features dataframe
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @references
#' TODO references
#'
#' @export
retrieveParseData <- function(apProData){
  xml <- getRemotePreApproved(apProData)
  features <- getFeaturesDataFrame(xml)
  return(list(xml = xml, features = features))
}


