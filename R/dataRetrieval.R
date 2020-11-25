getProtein <- function(source){
  out <- vector(mode = "list", length = length(source))

  print("Loading proteins")
  pb <- txtProgressBar(min = 0,
                       max = length(source),
                       style = 3,#1-3
                       width = 50,
                       char = "="
  )

  for(i in seq_along(source)){
    s <- source[[i]]
    if(length(s) > 1){
      stop(paste0("Improper input protein: ", s, ", consult vignettes to learn how to properly get proteins"))
    }
    str <- gsub("^\\s+|\\s+$", "", s)

    if(startsWith(str, "random")){
      spt <- strsplit(str, "\\s+")[[1]]
      number <- spt[startsWith(spt, "number")]
      orgid <- spt[startsWith(spt, "orgid")]

      number <- ifelse(identical(character(0), number), 1, sub(".*:", "", number))
      orgid <- ifelse(identical(character(0), orgid), "9606", sub(".*:", "", orgid))

      if(number != 1){
        k <- length(out) + as.numeric(number) - 1
        out <- out[1:k]

        for(j in 1:number){
          out[i + j-1] <- getRandomProtein(orgid)
        }
      }else{
        out[i] <- getRandomProtein(orgID = orgid)
      }
    }else if(file.exists(str)){
      out[i] <- getLocal(str)
    }else{
      if(endsWith(str, ".xml")){
        print(paste0("Failed to find file:", str, ". Would you like to attempt to download the file?"))
        print("Enter 1 for yes, 2 for no and skip this file, 3 to force terminate process.")
        get <- readline("Command: ")
        if(get == 1){
          out[i] <- getRemoteDownload(str)
        }else if(get == 3){
          stop("Forced termination as user requested")
        }
      }else{
        out[i] <- getRemote(str)
      }
    }

    setTxtProgressBar(pb, i)
  }
  cat("\n")#done to make any text after progress bar on proper line

  return(out[lengths(out) != 0])
}

getLocal <- function(source){
  list(XML::xmlToList(XML::xmlParse(source))[["entry"]])
}
getRemote <- function(source, url = paste0(paste0("https://www.uniprot.org/uniprot/", source), ".xml")){
  get <- httr::GET(url)
  code <- httr::status_code(get)

  if(code == 200){
    k <- XML::xmlParse(rawToChar(get$content))
    dd <- XML::xmlToList(k, simplify = TRUE)[["entry"]]
    return(list(dd))
  }else if(code == 400){
    warning("Bad request. There is a problem with input: ", source)
  }else if(code == 404){
    warning("Not found. The resource you requested, ", source, " doesn't exist")
  }else if(code == 410){
    warning("Gone. The resource you requested, ", source, " was removed")
  }else if(code == 500){
    warning("Internal server error. Most likely a temporary problem, but if the problem persists please contact Uniprot services")
  }else if(code == 503){
    warning("Service not available. The server is being updated, try again later.")
  }else{
    warning("Unknown error return: ", code, ", using the input: ", source)
  }

  return(list())
}

getRemoteDownload <- function(source){
  download.file(paste0("https://www.uniprot.org/uniprot/",source), source)
  getLocal(source)
}

getRandomProtein <- function(orgID){
  k <- httr::GET(paste0("https://www.uniprot.org/uniprot/?query=reviewed:yes+AND+organism:",orgID,"&random=yes"))
  getRemote(url = paste0(k$url, ".xml"))
}

