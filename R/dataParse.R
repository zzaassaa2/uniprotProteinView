#' INTERNAL FUNCTION: Creates Dataframe for features
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
#' @references
#' TODO references
getFeaturesDataFrame <- function(xmls){
  featuresList <- recursiveElementSearch(xmls, "feature")
  out <- vector(mode = "list", length = length(featuresList))
  for(i in seq_along(featuresList)){
    features <- NULL
    featList <- featuresList[[i]]
    for(j in seq_along(featList)){
      obj <- featList[[j]]

      des <- obj$.attrs["description"]
      if(is.na(des)){
        des <- obj$.attrs["type"]
      }

      if(!is.null(obj$location[["begin"]])) {
        posB <- as.numeric(obj$location$begin)
        posE <- as.numeric(obj$location$end)
      }else{
        posB <- as.numeric(obj$location$position)
        posE <- as.numeric(obj$location$position)
      }

      features <- rbind(features, c(obj$.attrs["type"], des, posB, posE))
    }

    dataframe <- as.data.frame(features, stringsAsFactors = FALSE)
    colnames(dataframe) <- c("type", "description", "begin", "end")
    dataframe$begin <- as.numeric(dataframe$begin)
    dataframe$end <- as.numeric(dataframe$end)

    out[i] <- list(dataframe)
  }

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