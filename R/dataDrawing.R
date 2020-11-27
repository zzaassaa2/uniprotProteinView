#' Drawing plotly plot using various user inputs to define what is drawn on the screen
#'
#' Creates plot using input list of protein keys for uniprot entries. Will draw main chain
#' then draw further features for the protein as specified within, should they be present.
#' Can search for features by either exact match, or if description contains an input string.
#' Can also further draw certain features above the structure, traditionally structural features
#'
#' @param proteins A string, vector, list, or list with entry named "source" that is equal
#' to a string, vector or list containing valid UniProt protein key_codes. This list can also
#' contain an element called colors that can equal a rgb() value, string of a color, or the word
#' in order "random" for a random color, to specify what color the protein in the same order will
#' be
#'
#' @param types A string, vector, list, or list with entry named "type" that is equal
#' to a string, vector or list containing valid matches to feature type names. This list can also
#' contain an element called colors that can equal a rgb() value, string of a color, or the word
#' in order "random" for a random color, to specify what color the element in the same order will
#' be
#' @param dess A string, vector, list, or list with entry named "type" that is equal
#' to a string, vector or list containing word searches that will then be used to find any entries
#' description, and if it should contain the string, will be valid. This list can also
#' contain an element called colors that can equal a rgb() value, string of a color, or the word
#' in order "random" for a random color, to specify what color the element in the same order will
#' be
#'
#' @param structure A string, vector, list, or list with entry named "type" that is equal
#' to a string, vector or list containing valid matches to feature description names. These entries
#' will normally be rendered above the protein, though this can be chaged by specifiing the parameters
#' btwnSpacingStart and btwnSpacing. This list can also contain an element called colors that can
#' equal a rgb() value, string of a color, or the word in order "random" for a random color, to
#' specify what color the element in the same order will be
#'
#' @param singleOffset The offset +- by parameter value used for rendering features specified
#' using the parameter dess, as these entries are noramally reserved for protein modifications,
#' which are single amino acids, and thus would not render otherwise, thus this parameter allows
#' the user to specify how thick they will show up.
#'
#' @param title The title that the plot will inherit
#'
#' @param saveGlobal If the function will save the protein XML data and feature dataframe
#' data as global values, so that the user can use them as they wish
#'
#' @param btwnSpacingStart Where the elements specified in the structure parameter will start,
#' by default they will be rendered above the protein
#'
#' @param btwnSpacing How tall the elements specified in the structure parameter will be
#'
#' @param showProgress If the consol should tell the user the current status in loading the
#' proteins is
#'
#' @return Plotly figure
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @references
#' TODO references
#'
#' @export
#' @import plotly
drawProtein <- function(proteins, types = list(), dess = list(), structure = list(), singleOffset = 1, title = NULL, saveGlobal = FALSE,
                        btwnSpacingStart = 1, btwnSpacing = 0.3, showProgress = TRUE){
  xml <- ifelse("source" %in% names(proteins), getProtein(proteins$source, showProgress), getProtein(proteins, showProgress))
  if(saveGlobal){
    .GlobalEnv$uniProtProteinView_xmls <- xml
    .GlobalEnv$uniProtProteinView_data <- getFeaturesDataFrame(uniProtProteinView_xmls)
  }else{
    uniProtProteinView_xmls <- xml
    uniProtProteinView_data <- getFeaturesDataFrame(uniProtProteinView_xmls)
  }

  figure <- plotly::plot_ly(type = "scatter", mode = "lines")
  colors <- ifelse("colors" %in% names(proteins), proteins$colors, NULL)
  yStart <- 0

  for(i in seq_along(uniProtProteinView_data)){
    d <- uniProtProteinView_data[[i]]

    clr <- ifelse(i <= length(colors), colors[[i]], randomColor())
    figure <- drawFeature(figure, d, list(colors = clr), function (type) d$type == "chain", yStart, yStop = yStart + 1)$figure

    figure <- drawFeature(figure, d, types, function(type) d$type == type, yStart, yStop = yStart + 1)$figure
    figure <- drawFeature(figure, d, dess, function(type) grepl(type, d$description, fixed = TRUE), yStart, yStop = yStart + 1, offset = singleOffset)$figure

    f <- drawFeature(figure, d, structure, function (type) d$type == type, yStart+btwnSpacingStart, yStop = yStart + btwnSpacingStart + btwnSpacing)
    figure <- f$figure

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

    if(f$actionPreformed) yStart <- yStart + btwnSpacing
    yStart <- yStart + 1
  }

  figure <- plotly::layout(figure,
                           title = title,
                           legend = list(itemsizing = "constant", font = list(size = 8)),
                           xaxis = list(showgrid = FALSE, title = "Protein Size", dtick = 50),
                           yaxis = list(showgrid = FALSE, tickvals = NULL)
  )

  return(figure)
}

#' INTERNAL FUNCTION: Main draw function
#'
#' Used to add the the feature to the figure
#'
#' @param figure Main plotly plot/figure
#'
#' @param xi Starting location for drawing on X-axis
#'
#' @param xf Ending location for drawing on the X-axis
#'
#' @param yi Starting location for drawing on the Y-axis
#'
#' @param yf Ending location for drawing on the Y-axis
#'
#' @param info Description info, used for legend nameing and hover info
#'
#' @param clr Color
#'
#' @param offset Offset on the X-axis of where to start drawing
#'
#' @return Returns the plot, with the new feature drawn on
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @references
#' TODO references
#'
#' @import plotly
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

#' INTERNAL FUNCTION: Helper function for standardizing input for before drawing
#'
#' Corrects input varables to proper form and used to interate through entries
#' that were found based upon input condition.
#'
#' @param figure Main plotly plot/figure
#'
#' @param d Dataframe containing proteins' features
#'
#' @param toParse Specific entires that will search through the features
#' dataframe to find matchs based upon condition
#'
#' @param condition Condition function for when to return true for match
#'
#' @param yStart Height element should begin drawing from
#'
#' @param yStop Height element should stop drawing at
#'
#' @param offset Offset of where drawing on the X-axis takes place
#'
#' @return Returns figure will all matching features drawn on
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @references
#' TODO references
#'
#' @import plotly
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

#' INTERNAL FUNCTION: Get and random rgb color
#'
#' @return Returns random rgb color
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @importFrom stats runif
#' @importFrom grDevices rgb
randomColor <- function(){
  i <- runif(3)
  rgb(i[[1]],
      i[[2]],
      i[[3]]
  )
}

#' INTERNAL FUNCTION: If else function
#'
#' Made because function provided by base package A) is really slow as there
#' are a thousand unneeded things they do, and B) cause the function from the
#' base package also for some reason returns incorrect values, no clue
#'
#' @param condition Condition for which to evaluate by
#'
#' @param true What value should be returned/run should the condition be true
#'
#' @param false What value should be returned/run should the condition be false
#'
#' @return Value depending on should the condition be true or false
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
ifelse <- function (condition, true, false){
  if(condition){
    true
  }else{
    false
  }
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

