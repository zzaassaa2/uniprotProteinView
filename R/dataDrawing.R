#getFeaturesDataFrame <- function(xmls){
#  featuresList <- recursiveElementSearch(xmls, "feature")
#  out <- vector(mode = "list", length = length(featuresList))
#  for(i in seq_along(featuresList)){
#    features <- NULL
#    featList <- featuresList[[i]]
#    for(j in seq_along(featList)){
#      obj <- featList[[j]]
#
#      des <- obj$.attrs["description"]
#      if(is.na(des)){
#        des <- obj$.attrs["type"]
#      }
#
#      if(!is.null(obj$location[["begin"]])) {
#        posB <- as.numeric(obj$location$begin)
#        posE <- as.numeric(obj$location$end)
#      }else{
#        posB <- as.numeric(obj$location$position)
#        posE <- as.numeric(obj$location$position)
#      }
#
#      features <- rbind(features, c(obj$.attrs["type"], des, posB, posE))
#    }
#
#    dataframe <- as.data.frame(features, stringsAsFactors = FALSE)
#    colnames(dataframe) <- c("type", "description", "begin", "end")
#    dataframe$begin <- as.numeric(dataframe$begin)
#    dataframe$end <- as.numeric(dataframe$end)
#
#    out[i] <- list(dataframe)
#  }
#
#  return(out)
#}
#
#recursiveElementSearch <- function(listIn, name){
#  out <- listIn[grepl(name, names(listIn))]
#  if(length(out) == 0){
#    out <- lapply(listIn, recursiveElementSearch, name = name)
#  }
#  out
#}

#===============================================================

#getProtein <- function(source){#list.files("temp", pattern="*.csv", full.names=TRUE)
#  out <- vector(mode = "list", length = length(source))
#
#  print("Loading proteins")
#  pb <- txtProgressBar(min = 0,
#                       max = length(source),
#                       style = 3,#1-3
#                       width = 50,
#                       char = "="
#  )
#
#  for(i in seq_along(source)){
#    s <- source[[i]]
#    if(length(s) > 1){
#      stop(paste0("Improper input protein: ", s, ", consult vignettes to learn how to properly get proteins"))
#    }
#    str <- gsub("^\\s+|\\s+$", "", s)
#
#    if(startsWith(str, "random")){
#      spt <- strsplit(str, "\\s+")[[1]]
#      number <- spt[startsWith(spt, "number")]
#      orgid <- spt[startsWith(spt, "orgid")]
#
#      number <- ifelse(identical(character(0), number), 1, sub(".*:", "", number))
#      orgid <- ifelse(identical(character(0), orgid), "9606", sub(".*:", "", orgid))
#
#      if(number != 1){
#        k <- length(out) + as.numeric(number) - 1
#        out <- out[1:k]
#
#        for(j in 1:number){
#          out[i + j-1] <- getRandomProtein(orgid)
#        }
#      }else{
#        out[i] <- getRandomProtein(orgID = orgid)
#      }
#    }else if(file.exists(str)){
#      out[i] <- getLocal(str)
#    }else{
#      if(endsWith(str, ".xml")){
#        print(paste0("Failed to find file:", str, ". Would you like to attempt to download the file?"))
#        print("Enter 1 for yes, 2 for no and skip this file, 3 to force terminate process.")
#        get <- readline("Command: ")
#        if(get == 1){
#          out[i] <- getRemoteDownload(str)
#        }else if(get == 3){
#          stop("Forced termination as user requested")
#        }
#      }else{
#        out[i] <- getRemote(str)
#      }
#    }
#
#    setTxtProgressBar(pb, i)
#  }
#  cat("\n")#done to make any text after progress bar on proper line
#
#  return(out[lengths(out) != 0])
#}
#
#getLocal <- function(source){
#  list(XML::xmlToList(XML::xmlParse(source))[["entry"]])
#}
#getRemote <- function(source, url = paste0(paste0("https://www.uniprot.org/uniprot/", source), ".xml")){
#  get <- httr::GET(url)
#  code <- httr::status_code(get)
#
#  if(code == 200){
#    k <- XML::xmlParse(rawToChar(get$content))
#    dd <- XML::xmlToList(k, simplify = TRUE)[["entry"]]
#    return(list(dd))
#  }else if(code == 400){
#    warning("Bad request. There is a problem with input: ", source)
#  }else if(code == 404){
#    warning("Not found. The resource you requested, ", source, " doesn't exist")
#  }else if(code == 410){
#    warning("Gone. The resource you requested, ", source, " was removed")
#  }else if(code == 500){
#    warning("Internal server error. Most likely a temporary problem, but if the problem persists please contact Uniprot services")
#  }else if(code == 503){
#    warning("Service not available. The server is being updated, try again later.")
#  }else{
#    warning("Unknown error return: ", code, ", using the input: ", source)
#  }
#
#  return(list())
#}
#
#getRemoteDownload <- function(source){
#  download.file(paste0("https://www.uniprot.org/uniprot/",source), source)
#  getLocal(source)
#}
#
#getRandomProtein <- function(orgID){
#  k <- httr::GET(paste0("https://www.uniprot.org/uniprot/?query=reviewed:yes+AND+organism:",orgID,"&random=yes"))
#  getRemote(url = paste0(k$url, ".xml"))
#}


#==========================================================

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
    .GlobalEnv$uniProtProteinView_data <- getFeaturesDataFrame(.GlobalEnv$uniProtProteinView_xmls)
  }else{
    env$uniProtProteinView_xmls <- env$xml
    env$uniProtProteinView_data <- getFeaturesDataFrame(env$uniProtProteinView_xmls)
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

#'
#' @import httr XML plotly
#'
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
#source("R/dataParse.R")
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