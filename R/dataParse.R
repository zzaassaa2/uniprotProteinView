#' Creates Dataframe for features
#'
#' Cycles through all proteins and for each, finds all entries called
#' "features", then addes them to a dataframe for each protein and combines
#' all dataframes into a list
#'
#' @param xmls List of parsed XMLs for each protein
#'
#' @return List of dataframes for each protein's XML data
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @export
getFeaturesDataFrame <- function(xmls){
  #Gets a list for each protein, where each protein is a list of all of its features
  featuresList <- recursiveElementSearch(xmls, "feature")
  out <- vector(mode = "list", length = length(featuresList))

  for(i in seq_along(featuresList)){
    features <- NULL
    featList <- featuresList[[i]]

    for(j in seq_along(featList)){
      obj <- featList[[j]]#Get the feature of interest

      type <- obj$.attrs["type"]#Get type and check if it exists, if not, skip it
      if(is.na(type) || is.null(type)){
        next
      }

      des <- obj$.attrs["description"]#Get description and check if it exists, if not, use type
      if(is.na(des) || is.null(des)){
        des <- type
      }

      if(!is.null(obj$location[["begin"]])) {
        #The as list, is that sometimes there will be a position, AND a status, but cause it is a vector, it is easier to just make it a list
        posB <- as.numeric(as.list(obj$location$begin)$position)
        posE <- as.numeric(as.list(obj$location$end)$position)
      }else{
        #This is needed for single amino acid modifications
        posB <- as.numeric(obj$location$position)
        posE <- as.numeric(obj$location$position)
      }

      #Make sure the the number is good, as both NA and numeric(0) are possible bad outputs, if so, skip
      if(is.na(posB) || is.na(posE) || identical(numeric(0), posB) || identical(numeric(0), posE)){
        next
      }

      #Binds together all parts to form matrix
      features <- rbind(features, list(type = obj$.attrs["type"], description = des, begin = posB, end = posE))
    }

    #Create a data frame of all the proteins features
    dataframe <- data.frame(features)
    dataframe$begin <- as.numeric(dataframe$begin)
    dataframe$end <- as.numeric(dataframe$end)

    out[i] <- list(dataframe)
  }
  .GlobalEnv$out <- out

  return(out)
}

#' INTERNAL FUNCTION: Recursivly search for element in list
#'
#' Will recursivly search through input list, till finds first level that contains
#' at least one entry that matches name parameter
#'
#' @param listIn The list for which to search through
#'
#' @param name Name condition to which to compare all list names to match
#'
#' @return List of all elements found at first level (not exhaustive)
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
recursiveElementSearch <- function(listIn, name){
  out <- listIn[grepl(name, names(listIn))]
  if(length(out) == 0){
    out <- lapply(listIn, recursiveElementSearch, name = name)
  }
  out
}