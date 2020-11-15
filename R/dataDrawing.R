
drawLayout <- function (xml, data){
  plot <- ggplot2::ggplot() + ggplot2::geom_point() +
    ggplot2::xlim(c(0, data$end[1])) +
    ggplot2::ylim(c(0, 4)) +
    ggplot2::labs(x = "Amino acid number") + ggplot2::labs(y = "") +
    ggplot2::guides(fill=ggplot2::guide_legend(title=getProteinName(xml))) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank())
}

elementIfMatch <- function (data, type, xoff = 0, ymi = 0, yma = 3){
  ggplot2::geom_rect(
    data[tolower(data$type) == tolower(type),],
    mapping = ggplot2::aes(xmin = begin - xoff, xmax = end + xoff, ymin = ymi, ymax = yma, fill = description)
  )
}
elementIfContains <- function(data, type, xoff = 1, ymi = 3, yma = 3.5){
  ggplot2::geom_rect(
    data[grepl(tolower(type), tolower(data$description), fixed = TRUE),],
    mapping = ggplot2::aes(xmin = begin - xoff, xmax = end + xoff, ymin = ymi, ymax = yma, fill = description)
  )
}

drawMotifs <- function (data) elementIfMatch(data, "short sequence motif")
drawChain <- function(data) elementIfMatch(data, "chain")
drawRegions <- function(data) elementIfMatch(data, "region of interest")
drawDomains <- function(data) elementIfMatch(data, "domain")
drawBetaStrands <- function (data) elementIfMatch(data, "strand", ymi = 2, yma = 3.5)
drawHelicies <- function (data) elementIfMatch(data, "helix", ymi = 2, yma = 3.5)
drawTurns <- function (data) elementIfMatch(data, "turn", ymi = 2, yma = 3.5)
