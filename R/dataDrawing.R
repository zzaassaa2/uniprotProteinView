
featureDataFrame <- function(featuresList){
  features <- NULL
  for(i in seq_along(featuresList)){
    obj <- featuresList[[i]]

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

  return(dataframe)
}

listFromName <- function (proteinList, name){
  proteinList[grepl(name, names(proteinList))]
}
getFeatureList <- function (proteinList) listFromName(proteinList, "feature")
getProteinName <- function(proteinList) proteinList$name