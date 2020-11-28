
library("shiny")

ui <- fluidPage(
  headerPanel("uniprotProteinView Shiny app page", windowTitle = "uniprotProteinView"),
  p("The app will map out proteins and their features, as described using the UniProt API. These features can be added with a specified color or without for a random choice",
    "For features, there are three ways to visualize them:"
  ),
  tags$ol(
  tags$li("Type search: which will show the represented types on the protein."),
  tags$li("Description search: Each protein has description for each type, so things like modified residues will have a description describing what this is, so a description search will ",
          "look through and find any description with has the given string with it, and display that, and example is Phos to find all phosphorylated residues."),
  tags$li("Offset search, this is the same as 1, but will display these features above the protein.")
  ),
  p("To start, search for your protein key_code from ", a(href = "https://www.uniprot.org/", "UniProt")),
  p("An example is the ", a(href="", "Atg13 protein"), " whose URL is https://www.uniprot.org/uniprot/Q06628, and the protein key_code is thus Q06628"),
  p("Multiple proteins can be searched at once by leaving a space between entries. To specify color and \"|color\", but replacing the word color with your desired color ",
    "of choice, so Q06628|red will show the protein as red, Q06628 will show it with a random color, and Q06628|#ffffff will show the protein white, using hex code"),
  hr(),
  fluidRow(
    column(3,
           wellPanel(
             textInput(inputId = "file", label = "UniProt Protein Key", placeholder = "Key_code"),
             textOutput("errorMsg"),
             actionButton("addFile", label = "Add File"),
             uiOutput("goodFiles")
           )
    ),
    column(3,
           wellPanel(
            selectInput("selectType", label = "Select Type to Search", choices = list()),
            textInput(inputId = "typeChooseColor", label = "Choose Color(name or Hex)", placeholder = "Leave empty for random"),
            actionButton("addType", label = "Add Type"),
            uiOutput("addedTypes")
           )
    ),
    column(3,
           wellPanel(
             textInput("selectDesSearch", label = "Select Description to Search"),
             textInput(inputId = "dessChooseColor", label = "Choose Color(name or Hex)", placeholder = "Leave empty for random"),
             actionButton("addDesSearch", label = "Add Description Search"),
             uiOutput("addedDesSearch")
           )
    ),
    column(3,
           wellPanel(
             selectInput("selectOffset", label = "Select Elements to Offset", choices = list()),
             textInput(inputId = "offsetChooseColor", label = "Choose Color(name or Hex)", placeholder = "Leave empty for random"),
             actionButton("addOffset", label = "Add Offset element"),
             uiOutput("addedOffset")
           )
    )
  ),
  actionButton("updateGraph", label = "Update Graph"),
  fluidRow(
    plotly::plotlyOutput("graph")
  )
)

server <- function (input, output, session){
  rv <- shiny::reactiveValues(files = NULL, types = list(), dess = list(), offset = list())

  observe({
    x <- rv$files
    if(!is.null(x)){
      xx <- data.table::rbindlist(unlist(x[,"features"],recursive = FALSE))
      x <- xx[,1]
      x <- x[!duplicated(x)]
      updateSelectInput(session, "selectType", choices = x[-1])
      updateSelectInput(session, "selectOffset", choices = x[-1])
    }
  })

  observeEvent(input$addFile,{
    str <- gsub("^\\s+|\\s+$", "", input$file)#trim spaces
    spt <- strsplit(str, "\\s+")[[1]]#split on space
    error <- NULL

    for(i in seq_along(spt)){
      s <- spt[[i]] #input
      st <- strsplit(s, "\\|")[[1]]
      code <- uniprotProteinView::confirmStatus(st[[1]], toRunIf = function (get) get, toRunElse = function () NULL)
      if(!is.null(code)){
        data <- uniprotProteinView::retrieveParseData(code)
        rv$files <- rbind(rv$files, c(st[[1]],
                                      {
                                        if(length(st) < 2){
                                          uniprotProteinView::randomColor()
                                        }else{
                                          st[[2]]
                                        }
                                      },
                                      data
                                      ))
      }else{
        error <- paste(error, st[[1]])
      }
    }

    if(is.null(error)){
      output$errorMsg <- NULL
    }else{
      output$errorMsg <- renderText(paste("Invalid input:", error))
    }

  })
  observeEvent(input$addType, {
    if(input$selectType != ""){
      rv$types$type <- append(rv$types$type, input$selectType)
      rv$types$colors <- append(rv$types$colors, ifelse(input$typeChooseColor == "", uniprotProteinView::randomColor(), input$typeChooseColor))
    }
  })
  observeEvent(input$addDesSearch, {
    if(input$selectDesSearch != ""){
      rv$dess$type <- append(rv$dess$type, input$selectDesSearch)
      rv$dess$colors <- append(rv$dess$colors, ifelse(input$dessChooseColor == "", uniprotProteinView::randomColor(), input$dessChooseColor))
    }
  })
  observeEvent(input$addOffset, {
    if(input$selectOffset != ""){
      rv$offset$type <- append(rv$offset$type, input$selectOffset)
      rv$offset$colors <- append(rv$offset$colors, ifelse(input$offsetChooseColor == "", uniprotProteinView::randomColor(), input$offsetChooseColor))
    }
  })

  output$goodFiles <- renderUI({
    checkboxGroupInput("goodFiles", "Files To Load", choices = rv$files[,1])
  })
  output$addedTypes <- renderUI({
    checkboxGroupInput("addedTypes", "Types:", choices = rv$types$type)
  })
  output$addedDesSearch <- renderUI({
    checkboxGroupInput("addedDesSearch", "Description Searchs:", choices = rv$dess$type)
  })
  output$addedOffset <- renderUI({
    checkboxGroupInput("addedOffset", "Offset Elements:", choices = rv$offset$type)
  })


  observeEvent(input$updateGraph, {
    output$graph <- plotly::renderPlotly({
      uniprotProteinView::drawProtein(proteins = list(preComputed = isolate(rv$files)),
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



#install.packages("rsconnect")
#rsconnect::setAccountInfo(name='zzaassaa2',
#                          token='494B8DA16516804A2A9860397222348E',
#                          secret='<secret key, find at the webpage>')
#rsconnect::deployApp("./R", appName = "appR")
