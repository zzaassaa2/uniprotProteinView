library("shiny")
library("uniprotProteinView")


ui <- shinyUI(fluidPage(
  theme = "bootstrap.css", #Load bootstrap css
  tagList(tags$head(tags$title("uniprotProteinView")), div(class = "col-sm-12 jumbotron", h1("uniprotProteinView"))),
  tags$div(
  class = "card bg-light mb-3",
    tags$div(
    class = "card-body",
    tags$h4(class="card-title", "How to use"),
    p(class="card-text","The app will map out proteins and their features, as described using the UniProt API. These features can be added with a specified color or without for a random choice.",
      "For features, there are three ways to visualize them:"
    ),
    tags$ol(class="card-text",
      tags$li(class="card-text","Type search: which will show the represented types on the protein."),
      tags$li(class="card-text","Description search: Each protein has description for each type, so things like modified residues will have a description describing what this is, so a description search will ",
              "look through and find any description with has the given string with it, and display that, and example is Phos to find all phosphorylated residues."),
      tags$li(class="card-text","Offset search, this is the same as 1, but will display these features above the protein.")
    ),
    p(class="card-text","To start, search for your protein key_code from ", a(href = "https://www.uniprot.org/", "UniProt")),
    p(class="card-text","An example is the ", a(href="", "Atg13 protein"), " whose URL is https://www.uniprot.org/uniprot/Q06628, and the protein key_code is thus Q06628"),
    p(class="card-text","Multiple proteins can be searched at once by leaving a space between entries."),
    p(class="card-text", "For more information, or references, see the github site: ", a(href = "https://github.com/zzaassaa2/uniprotProteinView", "uniprotProteinView"))
    )
  ),
  hr(),
  fluidRow(
    column(3,
           wellPanel(#File chooser
             textInput(inputId = "file", label = "UniProt Protein Key", placeholder = "Key_code"),
             textOutput("errorMsg"),
             textInput(inputId = "fileChooseColor", label = "Choose Color(name or Hex)", placeholder = "Leave empty for random"),
             actionButton("addFile", label = "Add File", class = "btn btn-default action-button btn-outline-primary"),
             div(id = "placeHolder")
           )
    ),
    column(3,
           wellPanel(
            selectInput("selectType", label = "Select Type to Search", choices = list()),
            textInput(inputId = "typeChooseColor", label = "Choose Color(name or Hex)", placeholder = "Leave empty for random"),
            actionButton("addType", label = "Add Type", class = "btn btn-default action-button btn-outline-primary"),
            div(id = "addedTypes")
           )
    ),
    column(3,
           wellPanel(
             textInput("selectDesSearch", label = "Select Description to Search"),
             textInput(inputId = "dessChooseColor", label = "Choose Color(name or Hex)", placeholder = "Leave empty for random"),
             actionButton("addDesSearch", label = "Add Description", class = "btn btn-default action-button btn-outline-primary"),
             div(id = "addedDesSearch")
           )
    ),
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
  )
))

server <- function (input, output, session){
  rv <- shiny::reactiveValues(files = NULL, types = list(), dess = list(), offset = list())

  observe({
    x <- rv$files
    if(!is.null(x)){
      if(nrow(x) > 0){#update the type and offset lists with the possible values
        xx <- data.table::rbindlist(x[,"features"])
        x <- xx[,1]
        x <- x[!duplicated(x)]#Removes duplicated elements
        updateSelectInput(session, "selectType", choices = x[-1])
        updateSelectInput(session, "selectOffset", choices = x[-1])
      }else{
        #This is here, for when not null, but also empty. An error would be thrown otherwise
        updateSelectInput(session, "selectType", choices = list())
        updateSelectInput(session, "selectOffset", choices = list())
      }
    }
  })

  observeEvent(input$addFile,{
    str <- gsub("^\\s+|\\s+$", "", input$file)#trim spaces
    spt <- strsplit(str, "\\s+")[[1]]#split on space
    error <- NULL#error output, if there is/was one

    for(i in seq_along(spt)){
      s <- spt[[i]] #input
      xml <- getProtein(s, FALSE)#Get the protein
      l <- length(xml)

      if(l > 0){#If a value was retrieved
        features <- getFeaturesDataFrame(xml)#Get feature dataframe Only bother getting this, if there is value to the xml
        for(i in seq_along(xml)){
          x <- xml[[i]]
          f <- features[[i]]

          #Bind together to form labeled matrix
          rv$files <- rbind(rv$files, list(protein = x[[1]], xml = x, features = f, colors = ifelse(input$fileChooseColor == "", randomColor(), input$fileChooseColor)))
          #Insert the new UI component
          insertUI(
            selector = "#placeHolder",
            ui = tags$div(
              id = x[[1]],
              class="alert alert-dismissible alert-success",
              tags$p(x[[1]]),
              actionButton(paste0("button",x[[1]]), "X", class = "btn btn-default action-button close")
            )
          )
          #Add event for when the delete button is pressed
          observeEvent(input[[paste0("button", x[[1]])]],{
            removeUI(selector = paste0("#", x[[1]]))
            #Removes element(s) when button pressed
            k <- rv$files[rv$files[,1] != x[[1]],]
            if(is.vector(k)){
              k <- rbind(NULL, list(protein = k[[1]], xml = k[[2]], features = k[[3]]))
            }
            rv$files <- k
          })
        }
      }else{
        error <- paste(error, s)
      }
    }

    if(is.null(error)){
      output$errorMsg <- NULL
    }else{
      output$errorMsg <- renderText(paste("Invalid input:", error))
    }
  })

  #Type event
  observeEvent(input$addType, {
    if(input$selectType != ""){
      rv$types$type <- append(rv$types$type, input$selectType)
      rv$types$colors <- append(rv$types$colors, ifelse(input$typeChooseColor == "", randomColor(), input$typeChooseColor))

      k <- gsub(" ", "_", input$selectType)
      insertUI(
        selector = "#addedTypes",
        ui = tags$div(
          id = k,
          class = "alert alert-dismissible alert-success",
          tags$p(k),
          actionButton(paste0("button", k), "X", class = "btn btn-default action-button close")
        )
      )
      observeEvent(input[[paste0("button", k)]],{
        print(k)
        removeUI(selector = paste0("#", k))
        rv$types <- rv$types[rv$types$type != k]
      })
    }
  })

  #Description search fields
  observeEvent(input$addDesSearch, {
    if(input$selectDesSearch != ""){
      rv$dess$type <- append(rv$dess$type, input$selectDesSearch)
      rv$dess$colors <- append(rv$dess$colors, ifelse(input$dessChooseColor == "", randomColor(), input$dessChooseColor))

      k <- gsub(" ", "_", input$selectDesSearch)
      insertUI(
        selector = "#addedDesSearch",
        ui = tags$div(
          id = k,
          class = "alert alert-dismissible alert-success",
          tags$p(k),
          actionButton(paste0("button", k), "X", class = "btn btn-default action-button close")
        )
      )
      observeEvent(input[[paste0("button", k)]],{
        print(k)
        removeUI(selector = paste0("#", k))
        rv$dess <- rv$dess[rv$dess$type != k]
      })
    }
  })

  #Offset search fields
  observeEvent(input$addOffset, {
    if(input$selectOffset != ""){
      rv$offset$type <- append(rv$offset$type, input$selectOffset)
      rv$offset$colors <- append(rv$offset$colors, ifelse(input$offsetChooseColor == "", randomColor(), input$offsetChooseColor))

      k <- gsub(" ", "_", input$selectOffset)
      insertUI(
        selector = "#addedOffset",
        ui = tags$div(
          id = k,
          class = "alert alert-dismissible alert-success",
          tags$p(k),
          actionButton(paste0("button", k), "X", class = "btn btn-default action-button close")
        )
      )
      observeEvent(input[[paste0("button", k)]],{
        print(k)
        removeUI(selector = paste0("#", k))
        rv$offset <- rv$offset[rv$offset$type != k]
      })
    }
  })

  #Main plot
  observeEvent(input$updateGraph, {
    output$graph <- plotly::renderPlotly({
      drawProtein(proteins = list(preComputed = isolate(rv$files)),
                  types = list(type = isolate(rv$types$type), colors = isolate(rv$types$colors)),
                  descriptionSearch = list(type = isolate(rv$dess$type), colors = isolate(rv$dess$colors)),
                  offSetFeatures = list(type = isolate(rv$offset$type), colors = isolate(rv$offset$colors)),
                  showProgress = FALSE,
                  saveGlobal = FALSE
      )
    })
  })

}

shiny::shinyApp(ui, server)


#todo random httr2 error, might must be ide
#todo general cleanup
#todo random format

