#' Get protein XML data
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
#' @examples
#' #Example 1:
#' #This will get the xml data for the protein Q04206
#' proteinXml <- getProtein("Q04206")
#'
#' #Example 2:
#' #This will get a random human protein and the Q04206 protein
#' proteinXml <- getProtein(c("random", "Q04206"))
#'
#' #Example 3:
#' #This will get 4 random human proteins
#' proteinXml <- getProtein("random number:4")
#'
#' #Example 4:
#' #This will get 3 random mouse proteins, the protein Q04206, and then 2 more random human proteins
#' proteinXml <- getProtein(c("random number:3 orgid:10090", "Q04206", "random number:2"))
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @export
#' @import XML
#' @importFrom utils setTxtProgressBar txtProgressBar
getProtein <- function(source, showProgress = TRUE){
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

  #Used to assist with getting the proper element, with the posibility of random values changeing things
  offset <- 1
  for(i in seq_along(source)){
    s <- source[[i]]

    #Called in case the user sneeks in a list of proteins, when it should be a single value
    if(length(s) > 1){
      warning(paste0("Improper input protein: ", s, ", consult vignettes to learn how to properly get proteins\n"))
      return(list())
    }
    #Trims the string of all white space
    str <- gsub("^\\s+|\\s+$", "", s)

    if(startsWith(str, "random")){#Handles cases where the user provides random
      spt <- gsub("\\|+", " ", str)
      spt <- strsplit(spt, "\\s+")[[1]]#Split on white space
      number <- spt[startsWith(spt, "number")]
      orgid <- spt[startsWith(spt, "orgid")]

      #These functions handle if there is or isn't the key word "number" or "orgid" provided, respectivly
      number <- ifelse(identical(character(0), number), 1, sub(".*:", "", number))
      number <- suppressWarnings(as.numeric(number))
      number <- ifelse(is.na(number), 1, number)
      orgid <- ifelse(identical(character(0), orgid), 9606, sub(".*:", "", orgid))
      orgid <- suppressWarnings(as.numeric(orgid))
      orgid <- ifelse(is.na(orgid), 9606, orgid)

      if(number != 1){
        #Expands output size
        k <- length(out) + number - 1
        out <- out[1:k]

        for(j in 1:number){
          out[offset] <- getRandomProtein(orgID = orgid)
          offset <- offset + 1
        }
      }else{
        out[offset] <- getRandomProtein(orgID = orgid)
        offset <- offset + 1
      }
    }else if(dir.exists(str)){#Used in case the user provides a directly of files
      #Gets all files ending in .xml and uses them
      found <- list.files(str, pattern = "*.xml", full.names = TRUE)
      #Expands output size
      k <- length(out) + length(found) - 1
      out <- out[1:k]

      for(j in seq_along(found)){#Loads them all
        out[offset] <- getLocal(found[[j]])
        offset <- offset + 1
      }
    }else if(file.exists(str)){#Used to get local .xml file
      out[offset] <- getLocal(str)
      offset <- offset + 1
    }else{
      if(endsWith(str, ".xml")){#If the user provided a .xml input, but the file doesn't exist, then will try and download it
        print(paste0("Failed to find file:", str, ". Would you like to attempt to download the file?"))
        print("Enter 1 for yes, 2 for no and skip this file, 3 to force terminate process.")
        get <- readline("Command: ")

        if(get == 1){
          out[offset] <- getRemoteDownload(str)
        }else if(get == 3){
          stop("Forced termination as user requested")
        }
      }else{
        out[offset] <- getRemote(str)#If all else isn't true, then try and read XML data from UniProt servers
      }
      offset <- offset + 1
    }

    if(showProgress){
      setTxtProgressBar(pb, i)#Increments progress bar after finished reading
    }
  }
  if(showProgress){
    cat("\n")#done to make any text after progress bar on proper line
  }

  return(out[lengths(out) != 0])#Used to remove any and all NULL values that managed to sneek there way in
}

#' INTERNAL FUNCTION: Loads the source from local .xml file
#'
#' @param source .xml file to load
#'
#' @return If successful, will return a list of XML data
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @import XML
getLocal <- function(source){
  xml <- XML::xmlToList(XML::xmlParse(source))

  if("entry" %in% names(xml)){
    return(list(xml[["entry"]]))#All UniProt xml files start with "entry"
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
getRemote <- function(source, url = paste0(paste0("https://www.uniprot.org/uniprot/", source), ".xml")){
  #Get webpage data
  get <- httr::GET(url)
  #Get code status of search
  code <- httr::status_code(get)

  if(code == 200){#Found the page correctly
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

  return(NULL)#Only returned if the page didn't find the proper content
}





#' INTERNAL FUNCTION: Download the file and then load locally
#'
#' @param source UniProt key_code.xml to try and download .xml file
#'
#' @return Returns list of XML data
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
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
#' @import httr
getRandomProtein <- function(orgID){
  #Address to use to get a random redirect to a new protein
  k <- httr::GET(paste0("https://www.uniprot.org/uniprot/?query=reviewed:yes+AND+organism:",orgID,"&random=yes"))
  getRemote(url = paste0(k$url, ".xml"))
}