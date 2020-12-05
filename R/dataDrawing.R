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
#' @examples
#' #Example 1:
#' #This is the simplest usage of the function. Will draw the main chain with a random color
#' drawProtein("Q04206")
#'
#' #Example 2:
#' #This will draw two proteins, with specified colors, showing various features drawn on
#' drawProtein(
#'   proteins = list(type = c("Q04206", "Q9D270"), colors = c("green", "green")),
#'   types = list(type = c("domain", "region of interest"), colors = c("red", "purple")),
#'   descriptionSearch = list(type = "phos", colors = "blue"),
#'   offSetFeatures = list(type = c("strand", "helix", "turn"),
#'                         colors = c("green", "orange", "purple"))
#' )
#'
#' #Example 3:
#' #This will draw the first protein red, then 4 random mouse proteins random colors,
#' # then one more random mouse protein green.
#' #It will also draw the domains and regions of interest, black and pink, respectivly.
#' # Along with all elements that contain "phos" somewhere in there descriptions
#' drawProtein(proteins = list(type = c("Q04206", "random orgid:10090 number:5"),
#'                             colors = c("red", "random number:4", "green")),
#'             types = list(type = c("domain", "region of interest"), colors = c("black", "pink")),
#'             descriptionSearch = "phos"
#' )
#'
#' #Example 4:
#' #This shows how to manually get the data and dataframe and use them directly to draw
#' xml <- uniprotProteinView::getProtein("Q04206", FALSE) #Get protein xml data
#' features <- uniprotProteinView::getFeaturesDataFrame(xml) #Get features dataframe
#'
#' #Create matrix
#' matrix <- rbind(list(xml = xml[[1]], features = features[[1]], colors = "red"))
#'
#' #Draw using the "preComputed" keyword
#' uniprotProteinView::drawProtein(proteins = list(preComputed = matrix))
#'
#' @references
#' Plotly R Open Source Graphing Library. Plotly. Website. \href{https://plotly.com/r/}{Link}
#'
#' @author {George Zorn, \email{george.zorn@mail.utoronto.ca}}
#'
#' @export
#' @importFrom plotly plot_ly layout
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

  #Fix input for the parse and associated colors
  typeParse <- ifelse("type" %in% names(types), types$type, types)
  typeClrs <- ifelse("colors" %in% names(types), types$colors, NULL)
  dessParse <- ifelse("type" %in% names(descriptionSearch), descriptionSearch$type, descriptionSearch)
  dessClrs <- ifelse("colors" %in% names(descriptionSearch), descriptionSearch$colors, NULL)
  offsetParse <- ifelse("type" %in% names(offSetFeatures), offSetFeatures$type, offSetFeatures)
  offsetClrs <- ifelse("colors" %in% names(offSetFeatures), offSetFeatures$colors, NULL)

  for(i in seq_along(uniProtProteinView_data)){#Iterate through, this should be equal to number of proteins input
    d <- uniProtProteinView_data[[i]]#Feature data for current protein

    #Color of main chain
    chainColor <- ifelse(i <= length(colors), colors[[i]], randomColor())

    #Get main chain
    chain <- getFeatures(d, list("placeholder"), chainColor, function (type) d$type == "chain", yStart, yStart + 1, FALSE)$table
    #Get types
    typeTable <- getFeatures(d, typeParse, typeClrs, function (type) tolower(d$type) == tolower(type), yStart, yStart + 1, TRUE)$table
    #Get description search
    dessTable <- getFeatures(d, dessParse, dessClrs, function (type) grepl(tolower(type), tolower(d$description), fixed = TRUE), yStart, yStart + 1, TRUE)$table
    #Get offset type search. Separated into two parts, so no blank space is left if nothing is to be drawn
    offsetTableTmp <- getFeatures(d, offsetParse, offsetClrs, function (type) tolower(d$type) == tolower(type), yStart + btwnSpacingStart, yStart + btwnSpacingStart + btwnSpacing, TRUE)
    offsetTable <- offsetTableTmp$table

    table <- rbind(chain, typeTable, dessTable, offsetTable)
    table <- data.frame(table)
    #Orders them from largest to smallest, in hopes to make sure nothing is hidden behind larger elements when drawn
    table <- table[order(unlist(table$xi) - unlist(table$xf)),]

    #Draw all rows in data frame
    for(j in seq_len(nrow(table))){
      row <- table[j,]
      figure <- drawChain(figure, row$xi[[1]], row$xf[[1]], row$yi[[1]], row$yf[[1]], row$info[[1]], row$color[[1]])
    }

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
    if(offsetTableTmp$actionPreformed) yStart <- yStart + btwnSpacing
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
#' @importFrom plotly add_trace
drawChain <- function(figure, xi, xf, yi, yf, info, clr){
  #Removes any information after and including the ";" character
  nameIn <- gsub("\\;.*","", info)
  #Limits the max legend size for the name to be 30 characters
  if(!is.na(nameIn) && nchar(nameIn) >= 30) nameIn <- paste0(substr(nameIn, 1, 30), "...")

  .GlobalEnv$x <- xf
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
#' @param data Dataframe containing proteins' features
#'
#' @param typeParse Specific entires that will search through the features
#' dataframe to find matchs based upon condition
#'
#' @param colors Colors matched to typeParse features by interation number
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
getFeatures <- function (data, typeParse, colors, condition, yStart, yStop, indent = TRUE){
  #variable initialization
  fig <- NULL
  clrLength <- length(colors)
  foundAny <- FALSE

  for(i in seq_along(typeParse)){
    type <- typeParse[[i]]
    #Fixes color input before hand
    clr <- ifelse(i <= clrLength, colors[[i]], randomColor())
    clr <- ifelse(clr == "random", randomColor(), clr)
    found <- data[condition(type),]
    if(dim(found)[1] != 0) foundAny <- TRUE

    if(!is.null(found) && nrow(found) > 0){
      for(j in seq_len(nrow(found))){
        row <- found[j,]
        #Needed cause sometimes NAs are thrown improperly, this might have been fixed, but I will keep this around just in case
        if(!is.na(row[,1])){
          info <- ifelse(indent, paste0("     ", row$description), row$description)
          xi <- row$begin
          xf <- row$end
          fig <- rbind(fig, list(xi = xi, xf = xf, yi = yStart, yf = yStop, info = info, color = clr))
        }
      }
    }
  }

  return(list(table = fig, actionPreformed = foundAny))
}
