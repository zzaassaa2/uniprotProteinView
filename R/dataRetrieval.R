getProteinLocal <- function (fileNames){
  out <- NULL
  for(i in seq_along(fileNames)){
    raw <- XML::xmlParse(fileNames[[i]])
    l <- XML::xmlToList(raw)
    out <- append(out, list(l[["entry"]]))
  }
  return(out)
}

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

getProteinRemoteD <- function(codes){
  fileNames <- NULL
  for(i in seq_along(codes)){
    file <- codes[[i]]
    if(!str_ends(file, ".xml")){
      file <- paste0(file, ".xml")
    }
    download.file(paste0("https://www.uniprot.org/uniprot/",file), file)
    fileNames <- c(fileNames, file)
  }
  getProteinLocal(fileNames)
}