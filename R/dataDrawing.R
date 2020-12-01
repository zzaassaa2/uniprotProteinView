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
#' @param descriptionSearch A string, vector, list, or list with entry named "type" that is equal
#' to a string, vector or list containing word searches that will then be used to find any entries
#' description, and if it should contain the string, will be valid. This list can also
#' contain an element called colors that can equal a rgb() value, string of a color, or the word
#' in order "random" for a random color, to specify what color the element in the same order will
#' be
#'
#' @param offSetFeatures A string, vector, list, or list with entry named "type" that is equal
#' to a string, vector or list containing valid matches to feature description names. These entries
#' will normally be rendered above the protein, though this can be chaged by specifiing the parameters
#' btwnSpacingStart and btwnSpacing. This list can also contain an element called colors that can
#' equal a rgb() value, string of a color, or the word in order "random" for a random color, to
#' specify what color the element in the same order will be
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
#' @export
#' @import plotly
drawProtein <- function(proteins, types = list(), descriptionSearch = list(), offSetFeatures = list(), title = NULL, saveGlobal = FALSE,
                        btwnSpacingStart = 1, btwnSpacing = 0.3, showProgress = TRUE){
  #This is used by the shiny app to directly add xml and feature data
  if("preComputed" %in% names(proteins)){#format must of list(xxxx = xxxx, xml = xmlData, features = featureData, colors = colorData)
    l <- proteins$preComputed
    xml <- l[,"xml"]
    uniProtProteinView_xmls <- xml
    uniProtProteinView_data <- l[,'features']
    colors <- assertColors(l[,"colors"])
  }else{
    #Stand way for user input
    xml <- ifelse("type" %in% names(proteins), getProtein(proteins$type, showProgress), getProtein(proteins, showProgress))
    if(saveGlobal){#Option if the user wishes to save the data globally
      .GlobalEnv$uniProtProteinView_xmls <- xml
      .GlobalEnv$uniProtProteinView_data <- getFeaturesDataFrame(.GlobalEnv$uniProtProteinView_xmls)
    }else{
      uniProtProteinView_xmls <- xml
      uniProtProteinView_data <- getFeaturesDataFrame(uniProtProteinView_xmls)
    }
    colors <- ifelse("colors" %in% names(proteins), assertColors(proteins$colors), NULL)
  }

  #Start plot handling
  figure <- plotly::plot_ly(type = "scatter", mode = "lines")
  yStart <- 0#This variable is used to keep track of where the next protein element should be drawn

  for(i in seq_along(uniProtProteinView_data)){#Iterate through, this should be equal to number of proteins input
    d <- uniProtProteinView_data[[i]]#Feature data for current protein

    #Color of main chain
    clr <- ifelse(i <= length(colors), colors[[i]], randomColor())
    #Draws the proteins main chain
    figure <- drawFeature(figure, d, list(colors = clr), function (type) d$type == "chain", yStart, yStop = yStart + 1, indent = FALSE)$figure

    #Draws all stand features by if the name is a literal match (excluding capatilization) to the input
    figure <- drawFeature(figure, d, types, function(type) tolower(d$type) == tolower(type), yStart, yStop = yStart + 1)$figure
    #Draws all features that contain the given string in their description
    figure <- drawFeature(figure, d, descriptionSearch, function(type) grepl(tolower(type), tolower(d$description), fixed = TRUE), yStart, yStop = yStart + 1)$figure

    #Draws all features that are wished to be drawn offset.
    f <- drawFeature(figure, d, offSetFeatures, function (type) tolower(d$type) == tolower(type), yStart+btwnSpacingStart, yStop = yStart + btwnSpacingStart + btwnSpacing)
    figure <- f$figure

    #Draws the protein literal name to the left of the protein
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

    #Used to determine if any offSet features where drawn if not, ignore and increment yStart by just 1 and move on
    if(f$actionPreformed) yStart <- yStart + btwnSpacing
    yStart <- yStart + 1
  }

  #Used to format the plot and add the title to the desired form
  figure <- plotly::layout(figure,
                           title = title,
                           legend = list(itemsizing = "constant", font = list(size = 8)),
                           xaxis = list(showgrid = FALSE, title = "Protein Size", dtick = 50),
                           yaxis = list(showgrid = FALSE, tickvals = NULL)
  )

  return(figure)
}

#' INTERNAL FUNCTION: Fixes colors to include random
#'
#' Goes through the list of colors, and handles any random entries, specifically if a number is provided
#'
#' @param colorsIn List of colors
#'
#' @return List of colors, size might be greater, if random has a number > 1
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
assertColors <- function (colorsIn){
  colorsIn <- unlist(colorsIn)#This is needed cause apparently the startsWith function doesn't like lists
  strWith <- startsWith(colorsIn, "random")

  if(length(strWith) > 0){#If no featueres that start with "random", then move on
    out <- vector(mode = "list", length = length(colorsIn))
    offset <- 1#This variable is used for when a random also includes an amount that, such as "random number:3"

    for(i in seq_along(colorsIn)){
      #Sees if there was an element that started with "random"
      if(strWith[[i]]){
        str <- strsplit(colorsIn[[i]], "\\s+")[[1]]#Splits by white space
        number <- str[startsWith(str, "number")]#If the number feature was specified
        #If number was added, then use what is after the ":", if not, then character(0) is returned, so just use 1
        number <- ifelse(identical(character(0), number), 1, sub(".*:", "", number))
        number <- suppressWarnings(as.numeric(number))
        number <- ifelse(is.na(number), 1, number)

        if(number != 1){
          #Expands the output size
          k <- length(out) + number - 1
          out <- out[1:k]

          for(j in 1:number){#Adds random for each entry
            out[offset] <- "random"
            offset <- offset + 1
          }
        }else{
          out[offset] <- "random"
          offset <- offset + 1
        }
      }else{
        #If random was not specified, then just add the color in
        out[offset] <- colorsIn[[i]]
        offset <- offset + 1
      }
    }

    return(out)#Returned if there was at least one instance of "random"
  }

  return(colorsIn)#Returned if there were not instances of "random"
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
#' @return Returns the plot, with the new feature drawn on
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @import plotly
drawChain <- function(figure, xi, xf, yi, yf, info, clr){
  #Removes any information after and including the ";" character
  nameIn <- gsub("\\;.*","", info)
  #Limits the max legend size for the name to be 30 characters
  if(!is.na(nameIn) && nchar(nameIn) >= 30) nameIn <- paste0(substr(nameIn, 1, 30), "...")

  if(xf - xi == 0){
    xxi <- xi - 1
    xxf <- xf + 1
  }else{
    xxi <- xi
    xxf <- xf
  }

  plotly::add_trace(figure,
                    x = c(xxi, xxi, xxf, xxf),
                    y = c(yi, yf, yf, yi),
                    fill = "toself",
                    fillcolor = clr,
                    hoveron = 'fills',
                    line = list(color ="rgba(1,1,1,0.0)"),
                    name = nameIn,
                    hoverinfo = "text",
                    text = paste0(gsub("^\\s+|\\s+$", "", info),#trims off any white space
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
#' @param indent If the input should be indented, used to indent features of a protein
#'
#' @return Returns figure will all matching features drawn on
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @import plotly
drawFeature <- function(figure, d, toParse, condition, yStart, yStop, indent = TRUE){
  #These two functions are used in case the user should provide a list with type and color specified, or just a regular list or vector
  typeParse <- ifelse("type" %in% names(toParse), toParse$type, toParse)
  colors <- ifelse("colors" %in% names(toParse), toParse$colors, NULL)
  foundAny <- FALSE

  for(j in seq_along(typeParse)){
    type <- typeParse[[j]]#Current feature to draw should it be present in the protein features
    #Used to make sure that if the user only provided one color for two proteins, then the function will simply use a random color
    clr <- ifelse(j <= length(colors), colors[[j]], randomColor())
    clr <- ifelse(clr == "random", randomColor(), clr)#checks if the color was random, if so, use a random color
    found <- d[condition(type),]#Get any features of the condition type, should there by any
    if(dim(found)[1] != 0) foundAny <- TRUE #Used to show that elements were drawn to screen, needed for offset features

    if(!is.null(found) && nrow(found) > 0){
      for(k in seq_along(found)){
        row <- found[k,]

        if(!is.na(row[,1])){#Used to make sure a NA table isn't returned
          #Used by the legend, so that the main chain isn't indented, while all features for the protiein are indented on the legend
          info <- ifelse(indent, paste0("    ", row$description), row$description)
          xi <- row$begin
          xf <- row$end
          figure <- drawChain(figure, xi, xf, yStart, yStop, info, clr)
        }
      }
    }
  }

  return(list(figure = figure, actionPreformed = foundAny))
}
