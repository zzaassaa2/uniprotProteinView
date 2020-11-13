
drawLayout <- function (data){
  plot <- ggplot2::ggplot() + ggplot2::geom_point() +
    ggplot2::xlim(c(0, data$end[1])) +
    ggplot2::ylim(c(0, 10)) +
    ggplot2::labs(x = "Amino acid number") + ggplot2::labs(y = "")
    #theme_bw(base_size = 20) +
    #theme(panel.grid.minor = element_blank(),
    #      panel.grid.major = element_blank()) +
    #theme(axis.ticks = element_blank(),
    #      axis.text.y = element_blank()) +
    #theme(panel.border = element_blank())
    #scale_color_discrete(name = "my Legend title",
    #                     labels = c("hello", "h2", "h3"))
}
drawElement <- function(plot, data, type, useDescription = TRUE){
  if(useDescription){
    plot <- plot + ggplot2::geom_rect(
      data[tolower(data$type) == tolower(type),],
      mapping = ggplot2::aes(xmin = begin, xmax = end, ymin = 0, ymax = 3, fill = description)
    )
  }else{
    plot <- plot + ggplot2::geom_rect(
      data[tolower(data$type) == tolower(type),],
      mapping = ggplot2::aes(xmin = begin, xmax = end, ymin = 0, ymax = 3)
    )
  }
  return(plot)
}
drawElements <- function(plot, data, type){
  for(i in seq_along(type)){
    plot <- drawElement(plot, data, type[i])
  }
  return(plot)
}

drawType <- function(plot, data, des){
  #todo show if there are multiple
  plot <- plot + ggplot2::geom_point(
    data[grepl(tolower(des), tolower(data$description), fixed = TRUE),],
    mapping = ggplot2::aes(x = begin, y = 3.2, fill = description),
    shape = 21, size = 4
  )
}
drawTypes <- function(plot, data, des){
  for(i in seq_along(des)){
    plot <- drawType(plot, data, des[i])
  }
  return(plot)
}

drawStructure <- function (plot, data, type){
  plot <- plot + ggplot2::geom_rect(
    data[tolower(data$type) == tolower(type),],
    mapping = ggplot2::aes(xmin = begin, xmax = end, ymin = 0, ymax = 3, fill = type)
  )
}
drawStructures <- function(plot, data, type){
  for(i in seq_along(type)){
    plot <- drawStructure(plot, data, type[i])
  }
  return(plot)
}

drawMotifs <- function (plot, data) plot + drawElement(plot, data, "motif")
drawChain <- function(plot, data) plot + drawElement(plot, data, "chain")
drawRegions <- function(plot, data) plot + drawElement(plot, data, "region")
drawDomains <- function(plot, data) plot + drawElement(plot, data, "domain")
drawBetaStrands <- function (plot, data) plot + drawStructure(plot, data, "strand")
drawHelicies <- function (plot, data) plot + drawStructure(plot, data, "helix")
drawTurns <- function (plot, data) plot + drawStructure(plot, data, "turn")


getProteinList <- function (proteinKey){
  xml <- XML::xmlParse(file = "Q04206.xml")
  d <- XML::xmlToList(xml)[["entry"]]
}
getProtein <- function (proteinKey){
  url <- paste0(paste0("https://www.uniprot.org/uniprot/",proteinKey), ".xml")
  raw <- xml2::read_xml(url)
  l <- xml2::as_list(raw)
  l[["uniprot"]][["entry"]]
}

getFeatureList <- function (proteinList){
  proteinList[grepl("feature", names(proteinList))]
}

featureDataFrame <- function(listOf){
  #can add new to datafram by frame$new <- rep(newData, times = nrow(frame)
  features <- NULL
  for(i in seq_along(listOf)){
    obj <- listOf[[i]]

    des <- obj$.attrs["description"]
    if(is.na(des)){
      des <- "NONE"
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


k <- getProteinList("Q04206.xml")
l <- getFeatureList(k)
data <- featureDataFrame(l)
#
plot <- drawLayout(data)
plot <- drawElement(plot, data, "chain")
#plot <- drawElements(plot, data, c("domain", "short sequence motif", "region of interest"))
plot <- drawStructure(plot, data, "strand")
