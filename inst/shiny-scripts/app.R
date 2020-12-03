library("shiny")

source("/Users/georgezorn/CLionProjects/uniprotProteinView/R/dataDrawing.R")
source("/Users/georgezorn/CLionProjects/uniprotProteinView/R/dataParse.R")
source("/Users/georgezorn/CLionProjects/uniprotProteinView/R/dataRetrieval.R")
source("/Users/georgezorn/CLionProjects/uniprotProteinView/R/runUniprotProteinView.R")
source("/Users/georgezorn/CLionProjects/uniprotProteinView/R/utilities.R")

#Fixes the colors list to account for random|number# type inputs
assertColors <- function (colorsIn){
  colorsIn <- strsplit(colorsIn, "\\s+")[[1]]
  colorsIn <- unlist(colorsIn)#This is needed cause apparently the startsWith function doesn't like lists
  strWith <- startsWith(colorsIn, "random")

  if(length(strWith) > 0){#If no featueres that start with "random", then move on
    out <- vector(mode = "list", length = length(colorsIn))
    offset <- 1#This variable is used for when a random also includes an amount that, such as "random number:3"

    for(i in seq_along(colorsIn)){
      #Sees if there was an element that started with "random"
      if(strWith[[i]]){
        str <- gsub("\\|+", " ", colorsIn[[i]])
        str <- strsplit(str[[i]], "\\s+")[[1]]#Splits by white space
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

ui <- fluidPage(
  theme = "bootstrap.css", #Load bootstrap css see: https://bootswatch.com/cerulean/
  #Draw Title
  tagList(tags$head(tags$title("uniprotProteinView")), div(class = "col-sm-12 jumbotron", h1("uniprotProteinView"))),
  #Info
  tags$div(
  class = "card bg-light mb-3",
    tags$div(
    class = "card-body",
    tags$h4(class="card-title", "How to use"),
    p(class="card-text","The app will map out proteins and their features, as described using the UniProt API. These features can be added with a specified color or without for a random choice.",
      "For features, there are three ways to visualize them:"
    ),
    tags$ol(class="card-text",
      tags$li(class="card-text","Type search: This will show the represented types on the protein."),
      tags$li(class="card-text","Description search: Each protein has a description for each type, so things such as modified residues will have a description of what this is, so a description search will ",
              "look through and find any description that has the given string and display it. An example is \"phos\" to find all phosphorylated residues."),
      tags$li(class="card-text","Offset search: This is the same as Type search, but it will display these features offset, above the protein.")
    ),
    p(class="card-text","To start, search for your protein key_code from ", a(href = "https://www.uniprot.org/", "UniProt.")),
    p(class="card-text","An example is the ", a(href="https://www.uniprot.org/uniprot/Q06628", "Atg13 protein"), " which is found at ", a(href = "https://www.uniprot.org/uniprot/Q06628", "https://www.uniprot.org/uniprot/Q06628"), ", and the protein key_code is thus Q06628."),
    p(class="card-text","Multiple proteins can be searched at once by leaving a space between entries."),
    p(class="card-text","The user can also search random proteins by typing \"random\". Typing \"random|number:2\" will generate two random proteins, with the number being interchangable. By default ",
      "the \"random\" key word will use the organism id (orgid) for humans: 9906. If the user wishes to change this, preform the opperation like \"random|orgid:10090\", the organism id for mice. ",
    "Putting this all together: \"random|number:5|orgid:10090\" will generate 5 random mouse proteins"),
    p(class = "card-text", "The user can also specify colors, by using either color name or hecadecimal. For color names, if an invalid color name is givin, then it will show up ",
      "grey. Color names can only be one word. If the user specifies a color, then the protein with that color will inherit the color for their text color. If the user gives two proteins at once, and only ",
    "one color, then the first will inherit that color, and the second will have a random color. The user can also specify \"random\" to have a random color given. Thus, if the user gives two proteins, and they ",
      "want the second to have a desired color, type \"random red\", and the first will have a random color, and the second will be red. The user can also use similar random format as protein input to specify number. ",
    "They can type \"random|number:2\" to give the first two proteins a random color. Thus an exmaple would be to give the protein input \"random|number:3 Q04206\" and color input \"random|number:2 red green\". ",
      "This will generate 3 random proteins, with the last being red, and the Q04206 protein green. This color syntax only applies to protein selection and not to type, description, or offset choosing."),
    p(class="card-text", "The use of | for specifing conditions only applies to the Shiny app, for equivilant syntax for when using pure R code, see the vignettes."),
    p(class="card-text", "For more information, or reference, see the github site: ", a(href = "https://github.com/zzaassaa2/uniprotProteinView", "uniprotProteinView."))
    )
  ),

  hr(),

  fluidRow(
    #Protein key_code input
    column(3,
           wellPanel(#File chooser
             textInput(inputId = "file", label = "UniProt Protein Key", placeholder = "Key_code"),
             textInput(inputId = "fileChooseColor", label = "Choose Color(name or Hex)", placeholder = "Leave empty for random"),
             actionButton("addFile", label = "Add File", class = "btn btn-default action-button btn-outline-primary"),
             div(id = "placeHolder")
           )
    ),
    #Type selection input
    column(3,
           wellPanel(
            selectInput("selectType", label = "Select Type to Search", choices = list()),
            textInput(inputId = "typeChooseColor", label = "Choose Color(name or Hex)", placeholder = "Leave empty for random"),
            actionButton("addType", label = "Add Type", class = "btn btn-default action-button btn-outline-primary"),
            div(id = "addedTypes")
           )
    ),
    #Description search input
    column(3,
           wellPanel(
             textInput("selectDesSearch", label = "Select Description to Search"),
             textInput(inputId = "dessChooseColor", label = "Choose Color(name or Hex)", placeholder = "Leave empty for random"),
             actionButton("addDesSearch", label = "Add Description", class = "btn btn-default action-button btn-outline-primary"),
             div(id = "addedDesSearch")
           )
    ),
    #Type input search that are to be offset
    column(3,
           wellPanel(
             selectInput("selectOffset", label = "Select Elements to Offset", choices = list()),
             textInput(inputId = "offsetChooseColor", label = "Choose Color(name or Hex)", placeholder = "Leave empty for random"),
             actionButton("addOffset", label = "Add Offset element", class = "btn btn-default action-button btn-outline-primary"),
             div(id = "addedOffset")
           )
    )
  ),

  actionButton("updateGraph", label = "Update Graph", class = "btn btn-default action-button btn-outline-primary"),
  fluidRow(
    plotly::plotlyOutput("graph")
  ),

  #footer
  tags$div(
    class="card text-white bg-primary mb-3",
    tags$div(
      class="card-body",
      tags$footer(
        class = "card-text",
        "This package and website was developed by George Zorn, as part of an assessment for 2020BCB410H: Applied Bioinformatics, University of Toronto, Toronto, CANADA."
      )
    )
  )
)

server <- function (input, output, session){
  rv <- shiny::reactiveValues(files = NULL, types = NULL, dess = NULL, offset = NULL)

  observe({
    x <- rv$files
    if(!is.null(x)){
      if(nrow(x) > 0){#update the type and offset lists with the possible values
        x <- data.table::rbindlist(x[,"features"])
        x <- x[,1]
        x <- x[!duplicated(x)]#Removes duplicated elements
        x <- x[x$type != "chain",]#Removes the chain feature
        x <- x[order(x)]#Order them by alphabet
        updateSelectInput(session, "selectType", choices = x)
        updateSelectInput(session, "selectOffset", choices = x)
      }else{
        #This is here, for when not null, but also empty. An error would be thrown otherwise
        updateSelectInput(session, "selectType", choices = list())
        updateSelectInput(session, "selectOffset", choices = list())
      }
    }
  })

  observeEvent(input$addFile, {
    withProgress(message = "Retrieving proteins", value = 0, {
      n <- 5#Used for ease of use for incProgress
      incProgress(1/n, detail = "Parsing input")
      str <- gsub("^\\s+|\\s+$", "", input$file)#Trim
      spt <- as.list(strsplit(str, "\\s+")[[1]])#Split into individual components, and convert to list for easy element deletion

      #Iterate through each entry and check if it already has been added, if so, set to NULL as getProtein function will deal with it
      incProgress(1/n, detail = "Checking for conflicts")
      for(i in seq_along(spt)){
        s <- spt[[i]]
        if(startsWith(s, "random")){#If it starts with random, then just move on
          next
        }

        k <- rv$files[rv$files[,1] == s,]
        if(length(k) > 0){
          showNotification(paste0(s, " already exists. Skipping"))
          spt[[i]] <- NULL
        }
      }

      incProgress(1/n, detail = "Retrieving proteins, this may take some time")
      xml <- NULL #This is needed if error is thrown, just to make sure the variable exists
      tryCatch({
        xml <- uniprotProteinView::getProtein(spt, FALSE)#Get protein xml data
      },error = function (cond){
        showNotification(paste0("The following error was thrown while trying to retrieve Protein data for the input: ",
                                " generating the error: ", cond), type = "error")
      },warning = function (cond){
        showNotification(cond, type = "warning")
      })

      if(!is.null(xml) && length(xml) > 0){#if no error, and something was retrieved
        incProgress(1/n, detail = "Generating dataframe")
        features <- NULL#This is if there is an error for the feature dataframe getting, needed for if the random protein is bad xml
        tryCatch({
          features <- uniprotProteinView::getFeaturesDataFrame(xml)#Get features dataframe
        }, error = function (cond){
          showNotification(paste("Error while attempting to generate features data frame. Original error message as follows:",cond))
        }, warning = function (cond){
          showNotification(paste("Warning thrown while trying to generate features dataframe:", cond))
        })
        colors <- assertColors(input$fileChooseColor)

        #Iterate through each,
        incProgress(1/n, detail = "Adding proteins")
        lapply(seq_along(xml), function (j){
          prot <- xml[[j]]
          protFet <- features[[j]]
          if(j <= length(colors)){
            clr <- colors[[j]]
          }else{
            clr <- "random"
          }

          xValue <- prot[[1]]
          rv$files <- rbind(rv$files, list(protein = xValue, xml = prot["name"], features = protFet, colors = clr))
          k2 <- paste0(xValue, input$addFile)
          insertUI(
            selector = "#placeHolder",
            ui = tags$div(
              id = k2,
              class = "alert alert-dismissible alert-success",
              tags$p(
                tags$style(paste0("#", k2, "{color: ", clr, "}")),
                xValue
              ),
              actionButton(paste0("button", k2), "X", class = "btn btn-default action-button close")
            )
          )

          observeEvent(input[[paste0("button", k2)]], {
            removeUI(selector = paste0("#", k2))
            k <- rv$files[rv$files[,1] != xValue,]
            if(is.vector(k)){
              k <- rbind(NULL, list(protein = k[[1]], xml = k[[2]], features = k[[3]], colors = k[[4]]))
            }
            rv$files <- k
          })
        })
      }else{
        if(is.null(xml)){
          showNotification("Error when trying to retrieve proteins", type = "error")
        }else if(length(xml) == 0){
          showNotification("Incorrect input", type = "warning")
        }else{
          showNotification("Unknown error thrown", type = "error")
        }
      }
    })
  })

  #Type event
  observeEvent(input$addType, {
    if(input$selectType != ""){
      value <- input$selectType

      if(length(rv$types[rv$types[,1] == value,])){
        showNotification(paste(value, "already exists. Skipping"))
      }else{
        if(input$typeChooseColor == ""){
          clr <- "random"
        }else{
          clr <- input$typeChooseColor
        }

        rv$types <- rbind(rv$types, list(type = value, colors = clr))

        k <- gsub("\\s+", "_", value)
        k2 <- paste0("addType_", k, input$addType)
        insertUI(
          selector = "#addedTypes",
          ui = tags$div(
            id = k2,
            class = "alert alert-dismissible alert-success",
            tags$p(
              tags$style(paste0("#", k2, "{color: ", clr, "}")),
              k
            ),
            actionButton(paste0("button", k2), "X", class = "btn btn-default action-button close")
          )
        )

        observeEvent(input[[paste0("button", k2)]],{
          removeUI(selector = paste0("#", k2))
          v <- rv$types[rv$types[,1] != value,]
          if(is.vector(v)){
            v <- rbind(NULL, list(type = v[[1]], colors = v[[2]]))
          }
          rv$types <- v
        })
      }
    }
  })

  #Description search event
  observeEvent(input$addDesSearch, {
    if(input$selectDesSearch != ""){
      value <- input$selectDesSearch

      if(length(rv$dess[rv$dess[,1] == value,])){
        showNotification(paste(value, "already exists. Skipping"))
      }else{
        if(input$dessChooseColor == ""){
          clr <- "random"
        }else{
          clr <- input$dessChooseColor
        }

        rv$dess <- rbind(rv$dess, list(type = value, colors = clr))

        k <- gsub("\\s+", "_", value)
        k2 <- paste0("addDesSearch_", k, input$addDesSearch)
        insertUI(
          selector = "#addedDesSearch",
          ui = tags$div(
            id = k2,
            class = "alert alert-dismissible alert-success",
            tags$p(
              tags$style(paste0("#", k2, "{color: ", clr, "}")),
              k
            ),
            actionButton(paste0("button", k2), "X", class = "btn btn-default action-button close")
          )
        )

        observeEvent(input[[paste0("button", k2)]],{
          removeUI(selector = paste0("#", k2))
          v <- rv$dess[rv$dess[,1] != value,]
          if(is.vector(v)){
            v <- rbind(NULL, list(type = v[[1]], colors = v[[2]]))
          }
          rv$dess <- v
        })
      }
    }
  })

  #Offset Type search event
  observeEvent(input$addOffset, {
    if(input$selectOffset != ""){
      value <- input$selectOffset

      if(length(rv$offset[rv$offset[,1] == value,])){
        showNotification(paste(value, "already exists. Skipping"))
      }else{
        if(input$offsetChooseColor == ""){
          clr <- "random"
        }else{
          clr <- input$offsetChooseColor
        }

        rv$offset <- rbind(rv$offset, list(type = value, colors = clr))

        k <- gsub("\\s+", "_", value)
        k2 <- paste0("addOffset_", k, input$addOffset)
        insertUI(
          selector = "#addedOffset",
          ui = tags$div(
            id = k2,
            class = "alert alert-dismissible alert-success",
            tags$p(
              tags$style(paste0("#", k2, "{color: ", clr, "}")),
              k
            ),
            actionButton(paste0("button", k2), "X", class = "btn btn-default action-button close")
          )
        )

        observeEvent(input[[paste0("button", k2)]],{
          removeUI(selector = paste0("#", k2))
          v <- rv$offset[rv$offset[,1] != value,]
          if(is.vector(v)){
            v <- rbind(NULL, list(type = v[[1]], colors = v[[2]]))
          }
          rv$offset <- v
        })
      }
    }
  })

  #Main plot
  observeEvent(input$updateGraph, {
    if(!is.null(rv$files) && nrow(rv$files) > 0){
      withProgress(message = "Generating Plot", value = 1,{
        tryCatch({
          if(is.null(rv$types)){
            types <- list()
          }else{
            types <- list(type = isolate(rv$types[,1]), colors = isolate(rv$types[,2]))
          }
          if(is.null(rv$dess)){
            descriptionSearch <- list()
          }else{
            descriptionSearch <- list(type = isolate(rv$dess[,1]), colors = isolate(rv$dess[,2]))
          }
          if(is.null(offset)){
            offSetFeatures <- list()
          }else{
            offSetFeatures <- list(type = isolate(rv$offset[,1]), colors = isolate(rv$offset[,2]))
          }
          output$graph <- plotly::renderPlotly({
            drawProtein(proteins = list(preComputed = isolate(rv$files)),
                                            types = types,
                                            descriptionSearch = descriptionSearch,
                                            offSetFeatures = offSetFeatures,
                                            showProgress = FALSE
            )
          })
          showNotification("Plot generated, this may take a moment to show up", duration = 4)
        },error = function (cond){
          showNotification(paste0("The following error was thrown while trying to generate graph:\n", cond), type = "error")
        }, warning = function (cond){
          showNotification(paste0("The following warning was thrown while trying to generate graph:\n", cond), type = "warning")
        })
      })
    }else{
      showNotification("Empty graph input", type = "warning", duration = 2)
    }
  })

}

shiny::shinyApp(ui, server)