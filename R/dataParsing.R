
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

listFromName <- function (proteinList, name){
  proteinList[grepl(name, names(proteinList))]
}
getFeatureList <- function (proteinList) listFromName(proteinList, "feature")
getProteinName <- function(proteinList) proteinList$name

#==================================

drawLayout <- function (xml, data){
  plot <- ggplot2::ggplot() + ggplot2::geom_point() +
    ggplot2::xlim(c(0, data$end[1])) +
    ggplot2::ylim(c(0, 3.2)) +
    ggplot2::labs(x = "Amino acid number") + ggplot2::labs(y = "") +
    ggplot2::guides(fill=ggplot2::guide_legend(title=getProteinName(xml))) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank())
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

drawType <- function(plot, data, des){#todo make rectangle
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

drawMotifs <- function (plot, data) drawElement(plot, data, "short sequence motif")
drawChain <- function(plot, data) drawElement(plot, data, "chain")
drawRegions <- function(plot, data) drawElement(plot, data, "region of interest")
drawDomains <- function(plot, data) drawElement(plot, data, "domain")
drawBetaStrands <- function (plot, data) drawStructure(plot, data, "strand")
drawHelicies <- function (plot, data) drawStructure(plot, data, "helix")
drawTurns <- function (plot, data) drawStructure(plot, data, "turn")

#drawProtein <- function (xml, features){
#  data <- featureDataFrame(features)
#  plot <- drawLayout(xml, data)
#  plot <- drawChain(plot, data)
#  plot <- drawDomains(plot, data)
#  plot <- drawMotifs(plot, data)
#  plot <- drawRegions(plot, data)
#}