
getProteinLocal <- function (fileName){
  raw <- XML::xmlParse(fileName)
  l <- XML::xmlToList(raw)
  l[["entry"]]
}

getProteinRemote <- function (url){
  #url <- "http://www.uniprot.org/uniprot/Q04206.xml"
  k <- XML::xmlParse(rawToChar(httr::GET(url)$content))
  dd <- XML::xmlToList(k, simplify = TRUE)[["entry"]]
}
#paste0(paste0("https://www.uniprot.org/uniprot/",proteinKey), ".xml")

getProteinRemoteD <- function(name){
  file <- paste0(name, ".xml")
  download.file(paste0(paste0("https://www.uniprot.org/uniprot/",name), ".xml"),
                file)
  getProteinLocal(file)
}