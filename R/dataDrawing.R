drawPlot <- function (dataList){
  lapply(dataList, dp)
}
dp <- function (data, height = 1.3){
  plot <- ggplot2::ggplot() + ggplot2::geom_point() +
    ggplot2::xlim(c(0, data$end[1])) +
    ggplot2::ylim(c(0, height)) +
    ggplot2::labs(x = "Amino acid number") + ggplot2::labs(y = "todo") +
    ggplot2::guides(fill=ggplot2::guide_legend(title=getProteinName(data))) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank()) +
    theme(legend.position = "bottom")
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