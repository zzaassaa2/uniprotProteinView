#' Interanl function that can generate a plot for one protein
#'
#' This function will take a protein's dataFrame and make a plot from it
#'
#' @param data Protein's dataFrame
#'
#' @param height The height of the plot
#'
#' @return If dataFrame provided is of the correct format, will return a blank plot
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plotForSingleProtein <- dp(data[[1]])
#'
dp <- function (data, height = 1.3){
  plot <- ggplot2::ggplot() + ggplot2::geom_point() +
    ggplot2::xlim(c(0, data$end[1])) +
    ggplot2::ylim(c(0, height)) +
    ggplot2::labs(x = "Amino acid number") + ggplot2::labs(y = "") +
    ggplot2::guides(fill=ggplot2::guide_legend(title=data$name)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "bottom")
}

#' Generate a plot for a list of protein dataFrames
#'
#' This function will take a list of protein dataFrames and generate a list of plots for each.
#'
#' @param dataList List of protein dataFrames
#'
#' @return A list of plots for each protein feature dataFrame
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#'
drawPlot <- function (dataList){
  lapply(dataList, dp)
}

#' Find all elements from features list that matches pattern
#'
#' This function will interate through lists of proteins dataFrames and find all elements that match
#' type pattern. It will then add the element to the respetive plot. Will return original list of plots
#' with modified values for the contained plots
#'
#' @param plots List of protein plots
#'
#' @param data List of protein feature dataFrames
#'
#' @param type Pattern used for matching
#'
#' @param xoff Offset for the bar, defines how much thickness is added
#'
#' @param ymi The Y value where drawing will start
#'
#' @param yma The Y value where drawing will stop
#'
#' @return A list of plots for each of same size as provided, with modified values for each element
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#' plots <- elementIfMatch(plots, data, "chain")
#'
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

#' Find all elements from features list that contains the pattern
#'
#' This function will interate through lists of proteins dataFrames and find all elements that contain
#' type pattern. It will then add the element to the respetive plot. Will return original list of plots
#' with modified values for the contained plots
#'
#' @param plots List of protein plots
#'
#' @param data List of protein feature dataFrames
#'
#' @param type Pattern used for if the element contains
#'
#' @param xoff Offset for the bar, defines how much thickness is added
#'
#' @param ymi The Y value where drawing will start
#'
#' @param yma The Y value where drawing will stop
#'
#' @return A list of plots for each of same size as provided, with modified values for each element
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#' plots <- elementIfContains(plots, data, "Phos")
#'
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

#' Add all motifs from protein's features to respective plot
#'
#' This function is a utility function, that will interate through lists of proteins dataFrames and find all motifs.
#' It will then add the motif to the respetive plot. Will return original list of plots with modified values for the
#' contained plots
#'
#' @param plots List of protein plots
#'
#' @param data List of protein feature dataFrames
#'
#' @return A list of plots for each of same size as provided, with modified values for each element
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#' plots <- drawMotifs(plots, data)
#'
drawMotifs <- function (plots, data) elementIfMatch(plots, data, "short sequence motif")

#' Add all chains from protein's features to respective plot
#'
#' This function is a utility function, that will interate through lists of proteins dataFrames and find all chains.
#' It will then add the chain to the respetive plot. Will return original list of plots with modified values for the
#' contained plots
#'
#' @param plots List of protein plots
#'
#' @param data List of protein feature dataFrames
#'
#' @return A list of plots for each of same size as provided, with modified values for each element
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#' plots <- drawChain(plots, data)
#'
drawChain <- function(plots, data) elementIfMatch(plots, data, "chain")

#' Add all reginos from protein's features to respective plot
#'
#' This function is a utility function, that will interate through lists of proteins dataFrames and find all regions.
#' It will then add the region to the respetive plot. Will return original list of plots with modified values for the
#' contained plots
#'
#' @param plots List of protein plots
#'
#' @param data List of protein feature dataFrames
#'
#' @return A list of plots for each of same size as provided, with modified values for each element
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#' plots <- drawRegions(plots, data)
#'
drawRegions <- function(plots, data) elementIfMatch(plots, data, "region of interest")

#' Add all domains from protein's features to respective plot
#'
#' This function is a utility function, that will interate through lists of proteins dataFrames and find all domains.
#' It will then add the domain to the respetive plot. Will return original list of plots with modified values for the
#' contained plots
#'
#' @param plots List of protein plots
#'
#' @param data List of protein feature dataFrames
#'
#' @return A list of plots for each of same size as provided, with modified values for each element
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#' plots <- drawDomains(plots, data)
#'
drawDomains <- function(plots, data) elementIfMatch(plots, data, "domain")

#' Add all beta-strands from protein's features to respective plot
#'
#' This function is a utility function, that will interate through lists of proteins dataFrames and find all beta-strands.
#' It will then add the beta-strand to the respetive plot. Will return original list of plots with modified values for the
#' contained plots
#'
#' @param plots List of protein plots
#'
#' @param data List of protein feature dataFrames
#'
#' @return A list of plots for each of same size as provided, with modified values for each element
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#' plots <- drawBetaStrands(plots, data)
#'
drawBetaStrands <- function (plots, data) elementIfMatch(plots, data, "strand", xoff = 1, ymi = 1.1, yma = 1.2)

#' Add all helicies from protein's features to respective plot
#'
#' This function is a utility function, that will interate through lists of proteins dataFrames and find all helicies.
#' It will then add the helix to the respetive plot. Will return original list of plots with modified values for the
#' contained plots
#'
#' @param plots List of protein plots
#'
#' @param data List of protein feature dataFrames
#'
#' @return A list of plots for each of same size as provided, with modified values for each element
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#' plots <- drawHelicies(plots, data)
#'
drawHelicies <- function (plots, data) elementIfMatch(plots, data, "helix", xoff = 1, ymi = 1.1, yma = 1.2)

#' Add all turns from protein's features to respective plot
#'
#' This function is a utility function, that will interate through lists of proteins dataFrames and find all turns.
#' It will then add the turn to the respetive plot. Will return original list of plots with modified values for the
#' contained plots
#'
#' @param plots List of protein plots
#'
#' @param data List of protein feature dataFrames
#'
#' @return A list of plots for each of same size as provided, with modified values for each element
#'
#' @import ggplot2
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#' plots <- drawTurns(plots, data)
#'
drawTurns <- function (plots, data) elementIfMatch(plots, data, "turn", xoff = 1, ymi = 1.1, yma = 1.2)

#' Draw all plots from a plot list to a single plot
#'
#' This function will take a list of plots and combine them all onto a single plot for comparison
#'
#' @param plots List of protein plots
#'
#' @return A single plot for which all plots are combine together
#'
#' @import cowplot
#'
#' @examples
#' xmls <- getProteinRemote(c("Q04206.xml", "Q9D270.xml"))
#' features <- getFeatureList(xmls)
#' data <- featuresToDataFrame(features)
#' plots <- drawPlot(data)
#' plots <- drawMotifs(plots, data)
#' plotProteins(plots)
#'
plotProteins <- function(plots){
  cowplot::plot_grid(plotlist=plots)
}