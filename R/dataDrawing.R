randomColor <- function(){
  i <- runif(3)
  rgb(i[[1]],
      i[[2]],
      i[[3]]
  )
}

drawChain <- function(figure, xi, xf, yi, yf, info, clr, offset = 0){
  nameIn <- gsub("\\;.*","", info)
  if(nchar(nameIn) >= 30) nameIn <- paste0(substr(nameIn, 1, 30), "...")

  plotly::add_trace(figure,
                    x = c(xi-offset, xi-offset, xf+offset, xf+offset),
                    y = c(yi, yf, yf, yi),
                    fill = "toself",
                    fillcolor = clr,
                    hoveron = 'fills',
                    line = list(color ="rgba(1,1,1,0.0)"),
                    name = nameIn,
                    hoverinfo = "text",
                    text = paste0(gsub("^\\s+|\\s+$", "", info),
                                  "\n", "Start: ", xi,
                                  "\n", "End: ", xf
                    )
  )
}

drawFeature <- function(figure, d, toParse, condition, yStart, yStop, offset = 0){
  typeParse <- ifelse("type" %in% names(toParse), toParse$type, toParse)
  colors <- ifelse("colors" %in% names(toParse), toParse$colors, NULL)
  foundAny <- FALSE

  for(j in seq_along(typeParse)){
    type <- typeParse[[j]]
    clr <- ifelse(j <= length(colors), colors[[j]], randomColor())
    clr <- ifelse(clr == "random", randomColor(), clr)
    found <- d[condition(type),]
    if(dim(found)[1] != 0) foundAny <- TRUE

    for(k in seq_along(found)){
      row <- found[k,]
      info <- paste0("    ", row$description)
      xi <- row$begin
      xf <- row$end
      figure <- drawChain(figure, xi, xf, yStart, yStop, info, clr, offset)
    }
  }
  return(list(figure = figure, actionPreformed = foundAny))
}

ifelse <- function (condition, true, false){
  if(condition){
    true
  }else{
    false
  }
}

setUp <- function (env, proteins, saveGlobal){
  env$xml <- ifelse("source" %in% names(proteins), getProtein(proteins$source), getProtein(proteins))
  if(saveGlobal){
    .GlobalEnv$uniProtProteinView_xmls <- env$xml
    .GlobalEnv$uniProtProteinView_data <- getFeaturesDataFrame(uniProtProteinView_xmls)
  }else{
    env$uniProtProteinView_xmls <- env$xml
    env$uniProtProteinView_data <- getFeaturesDataFrame(uniProtProteinView_xmls)
  }

  env$figure <- plotly::plot_ly(type = "scatter", mode = "lines")
  env$colors <- ifelse("colors" %in% names(proteins), proteins$colors, NULL)
  env$yStart <- 0
}

draw <- function (env, d, figure, colors, i, types, dess, structure, yStart, btwnSpacingStart, btwnSpacing, singleOffset,
                  preChain, postChain, featureDraw, gapDraw){
  chdEnv <- environment()
  if(!is.null(preChain)) preChain(env, chdEnv)

  clr <- ifelse(i <= length(colors), colors[[i]], randomColor())
  figure <- drawFeature(figure, d, list(colors = clr), function (type) d$type == "chain", yStart, yStop = yStart + 1)$figure

  if(!is.null(postChain)) postChain(env, chdEnv)

  figure <- drawFeature(figure, d, types, function(type) d$type == type, yStart, yStop = yStart + 1)$figure
  figure <- drawFeature(figure, d, dess, function(type) grepl(type, d$description, fixed = TRUE), yStart, yStop = yStart + 1, offset = singleOffset)$figure

  if(!is.null(featureDraw)) featureDraw(env, chdEnv)

  f <- drawFeature(figure, d, structure, function (type) d$type == type, yStart+btwnSpacingStart, yStop = yStart + btwnSpacingStart + btwnSpacing)
  figure <- f$figure
  env$actionPreformed <- f$actionPreformed

  if(!is.null(gapDraw)) gapDraw(env, chdEnv)

  figure
}

#'@import httr
#' @import XML
#' @import plotly
#' @export
drawProtein <- function(proteins, types = list(), dess = list(), structure = list(), singleOffset = 1, title = NULL, saveGlobal = FALSE,
                        btwnSpacingStart = 1, btwnSpacing = 0.3,
                        preDraw = NULL, preChain = NULL, postChain = NULL,
                        featureDraw = NULL, gapDraw = NULL, postDraw = NULL
){
  environment <- environment()
  setUp(environment, proteins, saveGlobal)

  if(!is.null(preDraw)) preDraw(environment)

  for(i in seq_along(uniProtProteinView_data)){
    d <- uniProtProteinView_data[[i]]

    figure <- draw(environment, d, figure, colors, i, types, dess, structure, yStart, btwnSpacingStart, btwnSpacing, singleOffset,
                   preChain, postChain, featureDraw, gapDraw)

    figure <- plotly::layout(figure,
                             annotations = list(
                               x = -0.01,
                               y = yStart + 0.5,
                               text = uniProtProteinView_xmls[[i]]$name,
                               showarrow = FALSE,
                               xanchor = "right",
                               font = list(size = 8)
                             )
    )

    if(actionPreformed) yStart <- yStart + btwnSpacing
    yStart <- yStart + 1
  }

  figure <- plotly::layout(figure,
                           title = title,
                           legend = list(itemsizing = "constant", font = list(size = 8)),
                           xaxis = list(showgrid = FALSE, title = "Protein Size", dtick = 50),
                           yaxis = list(showgrid = FALSE, tickvals = NULL)
  )

  if(!is.null(postDraw)) postDraw(environment)

  return(figure)
}

#source("R/dataRetrieval.R")
#source("R/dataParsing.R")
#drawProtein(
#  proteins = list(source = c("Q04206.xml", "Q9D270.xml"), colors = c("green", "green")),
#  types = list(type = c("domain", "region of interest"), colors = c("red", "purple")),
#  dess = list(type = "phos", colors = "blue"),
#  structure = list(type = c("strand", "helix", "turn"), colors = c("green", "orange", "purple")),
#  singleOffset = 2,
#  saveGlobal = TRUE
#)
#preDraw = NULL, preChain = NULL, postChain = NULL,
#featureDraw = NULL, gapDraw = NULL, postDraw = NULL



#todo folder load
#todo preformace
#todo shiny app, make sure function begins with run   >>use library(shiny)