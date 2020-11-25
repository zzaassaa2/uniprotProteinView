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

recursiveElementSearch <- function(listIn, name){
  out <- listIn[grepl(name, names(listIn))]
  if(length(out) == 0){
    out <- lapply(listIn, recursiveElementSearch, name = name)
  }
  out
}