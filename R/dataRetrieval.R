
getProteinLocal <- function (code){
  raw <- XML::xmlParse(code)
  l <- XML::xmlToList(raw)
  l[["entry"]]
}

getProteinRemote <- function (code){
  url <- paste0(paste0("https://www.uniprot.org/uniprot/",code), ".xml")
  k <- XML::xmlParse(rawToChar(httr::GET(url)$content))
  dd <- XML::xmlToList(k, simplify = TRUE)[["entry"]]
}

getProteinRemoteD <- function(name){
  file <- paste0(name, ".xml")
  download.file(paste0(paste0("https://www.uniprot.org/uniprot/",name), ".xml"),
                file)
  getProteinLocal(file)
}