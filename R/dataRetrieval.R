#' get list of xmls loaded from local .xml files
#'
#' This function will interate through all file names provided and attempt to parse through it
#' and then convert the entry into a list, stored inside of a larger list where each
#' entry is a separate protein
#'
#' @param fileNames A file name or list of file names that have the xml format used by Uniprot
#'
#' @return If the files exist and the xml is in the same standard format Uniprot uses, then this function
#' will return a list of lists where the parent list is of the same size as the number of file names
#' provided
#'
#' @import XML
#'
#' @examples
#' proteins <- getProteinLocal(c("Q0D270.xml","Q0D271.xml","Q0D272.xml"))
#'
getProteinLocal <- function (fileNames){
  out <- NULL
  for(i in seq_along(fileNames)){
    raw <- XML::xmlParse(fileNames[[i]])
    l <- XML::xmlToList(raw)
    out <- append(out, list(l[["entry"]]))
  }
  return(out)
}

#' Get list of xmls loaded from remote Uniprot server
#'
#' This function will interate through a single or vector of provided protein keys,
#' load the data, parse the xml, and then store the parsed data as a list within a
#' parent list
#'
#' @param codes A code or list of codes of protein keys
#'
#' @return If the webpage can be found for the valid protein key, and the data from the server is able to be
#' retrieved, then the data will be parsed and stored as a list within a parent list of the same size as
#' the number of codes provided
#'
#' @import httr, XML
#'
#' @examples
#' proteins <- getProteinRemote(c(c("Q0D270","Q0D271","Q0D272"))
#'
getProteinRemote <- function(codes){
  out <- NULL
  for(i in seq_along(codes)){
    url <- paste0(paste0("https://www.uniprot.org/uniprot/", codes[[i]]), ".xml")
    k <- XML::xmlParse(rawToChar(httr::GET(url)$content))
    dd <- XML::xmlToList(k, simplify = TRUE)[["entry"]]
    out <- append(out, list(dd))
  }
  return(out)
}

#' Download the associated .xml file for the protein, and then load it locally from the UniProt server
#'
#' This function will interate through a single or vector of provided protein keys,
#' download the data, then load, parse, and finally store the parsed data as a list within a
#' parent list
#'
#' @param codes A code or list of codes of protein keys
#'
#' @return If the webpage can be found for the valid protein key, and the data from the server is able to be
#' retrieved, then the data will be parsed and stored as a list within a parent list of the same size as
#' the number of codes provided
#'
#' @import XML, stringr
#'
#' @examples
#' proteins <- getProteinRemoteD(c(c("Q0D270","Q0D271","Q0D272"))
#'
getProteinRemoteD <- function(codes){
  fileNames <- NULL
  for(i in seq_along(codes)){
    file <- codes[[i]]
    if(!stringr::str_ends(file, ".xml")){
      file <- paste0(file, ".xml")
    }
    download.file(paste0("https://www.uniprot.org/uniprot/",file), file)
    fileNames <- c(fileNames, file)
  }
  getProteinLocal(fileNames)
}