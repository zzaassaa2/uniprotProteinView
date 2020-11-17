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
    if(!stringr::str_ends(file, ".xml")){
      file <- paste0(file, ".xml")
    }
    download.file(paste0("https://www.uniprot.org/uniprot/",file), file)
    fileNames <- c(fileNames, file)
  }
  getProteinLocal(fileNames)
}

#=========================================

featuresToDataFrame <- function(featuresList){
  #can add new to datafram by frame$new <- rep(newData, times = nrow(frame)
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

listFromName <- function (proteinList, name){
  proteinList[grepl(name, names(proteinList))]
}
getFeatureList <- function (proteinList){
  lapply(proteinList, listFromName, name = "feature")
}

#==================================

drawPlot <- function (dataList){
  lapply(dataList, dp)
}
dp <- function (data, height = 1.3){
  plot <- ggplot2::ggplot() + ggplot2::geom_point() +
    ggplot2::xlim(c(0, data$end[1])) +
    ggplot2::ylim(c(0, height)) +
    ggplot2::labs(x = "Amino acid number") + ggplot2::labs(y = "todo") +
    ggplot2::guides(fill=ggplot2::guide_legend(title=data$name)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                               panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "bottom")
}

elementIfMatch <- function (plots, data, type, xoff = 0, ymi = 0, yma = 1){
  for(i in seq_along(plots)){
    plot <- plots[[i]]
    d <- data[[i]]
    k <- d[tolower(d$type) == tolower(type),]
    if(dim(k)[1] != 0){
      plots[[i]] <- plot + ggplot2::geom_rect(
        k,
        mapping = ggplot2::aes(xmin = begin - xoff, xmax = end + xoff, ymin = ymi, ymax = yma, fill = description)
      )
    }
  }
  return(plots)
}
elementIfContains <- function(plots, data, type, xoff = 1, ymi = 1, yma = 1.1){
  for(i in seq_along(plots)){
    plot <- plots[[i]]
    d <- data[[i]]
    k <- d[grepl(tolower(type), tolower(d$description), fixed = TRUE),]
    if(dim(k)[1] != 0){
      plots[[i]] <- plot + ggplot2::geom_rect(
        k,
        mapping = ggplot2::aes(xmin = begin - xoff, xmax = end + xoff, ymin = ymi, ymax = yma, fill = description)
      )
    }
  }
  return(plots)
}

drawMotifs <- function (plots, data) elementIfMatch(plots, data, "short sequence motif")
drawChain <- function(plots, data) elementIfMatch(plots, data, "chain")
drawRegions <- function(plots, data) elementIfMatch(plots, data, "region of interest")
drawDomains <- function(plots, data) elementIfMatch(plots, data, "domain")
drawBetaStrands <- function (plots, data) elementIfMatch(plots, data, "strand", xoff = 1, ymi = 1.1, yma = 1.2)
drawHelicies <- function (plots, data) elementIfMatch(plots, data, "helix", xoff = 1, ymi = 1.1, yma = 1.2)
drawTurns <- function (plots, data) elementIfMatch(plots, data, "turn", xoff = 1, ymi = 1.1, yma = 1.2)

plotProteins <- function(plots){
  cowplot::plot_grid(plotlist=plots)
}




xmls <- getProteinLocal(c("../Q04206.xml", "Q9D270.xml"))
features <- getFeatureList(xmls)
data <- featuresToDataFrame(features)
plots <- drawPlot(data)
plots <- elementIfMatch(plots, data, "chain")
plots <- drawDomains(plots, data)
plots <- drawBetaStrands(plots, data)
plotProteins(plots)




#stop()
#warning()
#message()





