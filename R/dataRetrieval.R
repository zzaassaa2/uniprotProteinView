#' INTERNAL FUNCTION: Get protein XML data
#'
#' Searches through list or vector of entries and attempts to retrieve UniProt protein
#' XML data. This can be done by either .xml entry for local files, where if none are found,
#' will ask if the user either want to skip or download the file and load that. Otherwise, the
#' user can specify a UniProt protein key_code where the function will then attempt to read
#' the XML data from the webpage. Alternativly, the user can use "random" to randomly pick
#' a protein.
#'
#' @param source List of entries for which to attempt to load
#'
#' @param showProgress If the user should be notified on the progress of this function
#'
#' @return List for each loaded protein, each element containing a list of XML data
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @references
#' TODO references
#'
#' @export
#' @import XML
#' @importFrom utils setTxtProgressBar txtProgressBar
getProtein <- function(source, showProgress){
  out <- vector(mode = "list", length = length(source))

  if(showProgress){
    print("Loading proteins")
    pb <- txtProgressBar(min = 0,
                         max = length(source),
                         style = 3,#1-3
                         width = 50,
                         char = "="
    )
  }

  offset <- 1
  for(i in seq_along(source)){
    s <- source[[i]]
    if(length(s) > 1){
      warning(paste0("Improper input protein: ", s, ", consult vignettes to learn how to properly get proteins\n"))
      return(list())
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
          out[offset] <- getRandomProtein(orgid)
          offset <- offset + 1
        }
      }else{
        out[offset] <- getRandomProtein(orgID = orgid)
        offset <- offset + 1
      }
    }else if(dir.exists(str)){
      found <- list.files(str, pattern = "*.xml", full.names = TRUE)
      k <- length(out) + length(found) - 1
      out <- out[1:k]
      for(j in seq_along(found)){
        out[offset] <- getLocal(found[[j]])
        offset <- offset + 1
      }
    }else if(file.exists(str)){
      out[offset] <- getLocal(str)
      offset <- offset + 1
    }else{
      if(endsWith(str, ".xml")){
        print(paste0("Failed to find file:", str, ". Would you like to attempt to download the file?"))
        print("Enter 1 for yes, 2 for no and skip this file, 3 to force terminate process.")
        get <- readline("Command: ")
        if(get == 1){
          out[offset] <- getRemoteDownload(str)
        }else if(get == 3){
          stop("Forced termination as user requested")
        }
      }else{
        out[offset] <- getRemote(str)
      }
      offset <- offset + 1
    }

    if(showProgress){
      setTxtProgressBar(pb, i)
    }
  }
  if(showProgress){
    cat("\n")#done to make any text after progress bar on proper line
  }

  .GlobalEnv$apple <- out
  return(out[lengths(out) != 0])
}

#' INTERNAL FUNCTION: Loads the source from local .xml file
#'
#' @param source .xml file to load
#'
#' @return If successful, will return a list of XML data
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @references
#' TODO references
#'
#' @import XML
getLocal <- function(source){
  xml <- XML::xmlToList(XML::xmlParse(source))
  if("entry" %in% names(xml)){
    return(list(xml[["entry"]]))
  }else{
    warning(paste0("Attempted to load file:", source, ", however the file isn't in the proper format"))
  }
  NULL
}

#' INTERNAL FUNCTION: Load source remotly
#'
#' Will attempt to see if the source is a valid uniprot page. Will return a status_code to
#' determine if successful, and if so, parse XML data
#'
#' @param source Protein UniProt keycode
#'
#' @param url URL source to search
#'
#' @return List of XML data
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @references
#' TODO references
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





#' INTERNAL FUNCTION: Download the file and then load locally
#'
#' @param source UniProt key_code.xml to try and download .xml file
#'
#' @return Returns list of XML data
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @references
#' TODO references
#'
#' @importFrom utils download.file
getRemoteDownload <- function(source){
  download.file(paste0("https://www.uniprot.org/uniprot/",source), source)
  getLocal(source)
}

#' INTERNAL FUNCTION: Randomly picks a protein to load
#'
#' Will use UniProts internal API function to find a random protein using the specified
#' organism ID
#'
#' @param orgID Organism id that UniProt uses for organism registration
#'
#' @return List of parsed XML data
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @references
#' TODO references
#'
#' @import httr
getRandomProtein <- function(orgID){
  k <- httr::GET(paste0("https://www.uniprot.org/uniprot/?query=reviewed:yes+AND+organism:",orgID,"&random=yes"))
  getRemote(url = paste0(k$url, ".xml"))
}