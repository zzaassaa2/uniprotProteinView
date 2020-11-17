#' Convert a list of feature list for each protein into a dataFrame
#'
#' This function will take a list, where each entry represent the data for a separate protein. It will
#' then interate through each sub list and look through each and add each featuers to a dataframe. Each
#' dataFrame generated for each proteins features are then concated together as a list of dataFrames.
#'
#' @param featuresList A list of feature lists for each loaded protein
#'
#' @return A list of dataFrames for each individual protein
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#'
#' @export
featuresToDataFrame <- function(featuresList){
  out <- NULL
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

    #dataframe$proteinNumber <- i - 1
    out <- append(out, list(dataframe))
  }

  return(out)
}

#' List of elements from name form a protein's component list
#'
#' This function will interate through a protein's list of components, and if any element name matches
#' a provided name, then will give back a list of elements. Note should be tacken, that this only will work for a
#' proteins specific list, not the parent list.
#'
#' @param proteinList A list for a specific protein contaning all of its parsed elements
#'
#' @param name An identifier to preform matching on
#'
#' @return If all provided paramters are correct, then will return a list of all elements with matching name
#' to the provided pattern name
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' featuresForTheFirstProtein <- listFromName(xmls[[1]], "features")
#'
#' @export
listFromName <- function (proteinList, name){
  proteinList[grepl(name, names(proteinList))]
}

#' Wrapper method to interate a list of protein components and find all features
#'
#' This function will interate through a list of proteins, and find within each protein, all feature elements
#' and return a list of equal size as the proteinList parameter, each sub list containing each individual protein's
#' features
#'
#' @param proteinList A list for of proteins, where each sublist contanins all of the protein's parsed elements
#'
#' @return If all provided paramters are correct, then will return a list containing sublists that are protein specific,
#' each containing all feature elements. The parent list size is the same size as the inputed list.
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#'
#' @export
getFeatureList <- function (proteinList){
  lapply(proteinList, listFromName, name = "feature")
}