library("shiny")

ui <- fluidPage(
  theme = "bootstrap.css", #Load bootstrap css see: https://bootswatch.com/cerulean/
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
    p(class="card-text", "For more information, or references, see the github site: ", a(href = "https://github.com/zzaassaa2/uniprotProteinView", "uniprotProteinView."))
    )
  ),
  hr(),
  fluidRow(
    column(3,
           wellPanel(#File chooser
             textInput(inputId = "file", label = "UniProt Protein Key", placeholder = "Key_code"),
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
  ),
  tags$div(
    class="card text-white bg-primary mb-3",
    tags$div(
      class="card-body",
      tags$footer(
        class = "card-text",
        "This package and website was developed by George Zorn, as part of an assessment for 2020BCB410H: Applied Bioinformatics, University of Toronto, Toronto, CANADA.",
        "This webpage makes use of a bootstrap css script made free for use by Thomas Park. ", tags$a(href="https://bootswatch.com/", "Website "), tags$a(href="https://github.com/thomaspark/bootswatch", "Github")
      )
    )
  )
)

server <- function (input, output, session){
  rv <- shiny::reactiveValues(files = NULL, types = list(), dess = list(), offset = list())

  observe({
    x <- rv$files
    if(!is.null(x)){
      if(nrow(x) > 0){#update the type and offset lists with the possible values
        x <- data.table::rbindlist(x[,"features"])
        x <- x[,1]
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

      k <- rv$files[rv$files[,1] == s,]
      if(length(k) > 0){
        showNotification(paste0(s, " already exists. Skipping"))
        next
      }

      xml <- NULL#This is required, cause there is some stupid memory allocation error that be thrown, so in case of that...
      withProgress(message = "Retrieving Protein Information", value = 1, {
        tryCatch({
          suppressWarnings(xml <- uniprotProteinView::getProtein(s, FALSE))#Get the protein
        },error = function (cond){
          showNotification(paste0("The following error was thrown while trying to retrieve Protein data for the input: ",s,
                                  " generating the error:\n", cond), type = "error")
        })
      })
      l <- length(xml)

      if(!is.null(xml) && l > 0){#If a value was retrieved
        features <- uniprotProteinView::getFeaturesDataFrame(xml)#Get feature dataframe Only bother getting this, if there is value to the xml

        #this function is mandatory, and in general recommended, for all shiny loops, cause other wise it just doesn't work
        lapply(seq_along(xml), function (i){
          x <- xml[[i]]
          f <- features[[i]]
          xValue <- x[[1]]
          k <- rv$files[rv$files[,1] == xValue,]
          #This is placed here to catch for the random protein function
          if(length(k) > 0){
            showNotification(paste0(s, " already exists. Skipping"))
            next
          }

          if(input$fileChooseColor == ""){
            clr <- "random"
          }else{
            clr <- input$fileChooseColor
          }

          #Here we only give it a list with the first element for XML to reduce data size, and cause only the first element is needed
          rv$files <- rbind(rv$files, list(protein = xValue, xml = x[1], features = f, colors = clr))#Bind together to form labeled matrix
          #Insert the new UI component
          insertUI(
            selector = "#placeHolder",
            ui = tags$div(
              id = xValue,
              class="alert alert-dismissible alert-success",
              tags$p(
                tags$style(paste0("#",xValue, "{color: ",clr,"}")),
                xValue
              ),
              actionButton(paste0("button",xValue), "X", class = "btn btn-default action-button close")
            )
          )
          #Add event for when the delete button is pressed
          observeEvent(input[[paste0("button", xValue)]],{
            removeUI(selector = paste0("#", xValue))
            #Removes element(s) when button pressed
            k <- rv$files[rv$files[,1] != xValue,]
            if(is.vector(k)){
              k <- rbind(NULL, list(protein = k[[1]], xml = k[[2]], features = k[[3]], colors = k[[4]]))
            }
            rv$files <- k
          })
        })
      }else{
        error <- paste(error, s)
      }
    }

    if(!is.null(error)){
      showNotification(paste("Invalid input for:", error), type = "warning")
    }
  })

  #Type event
  observeEvent(input$addType, {
    if(input$selectType != ""){
      rv$types$type <- append(rv$types$type, input$selectType)
      if(input$typeChooseColor == ""){
        clr <- "random"
      }else{
        clr <- input$typeChooseColor
      }
      rv$types$colors <- append(rv$types$colors, clr)

      k <- gsub(" ", "_", input$selectType)
      insertUI(
        selector = "#addedTypes",
        ui = tags$div(
          id = k,
          class = "alert alert-dismissible alert-success",
          tags$p(
            tags$style(paste0("#",k, "{color: ",clr,"}")),
            k
          ),
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
      if(input$dessChooseColor == ""){
        clr <- "random"
      }else{
        clr <- input$dessChooseColor
      }
      rv$dess$colors <- append(rv$dess$colors, clr)

      k <- gsub(" ", "_", input$selectDesSearch)
      insertUI(
        selector = "#addedDesSearch",
        ui = tags$div(
          id = k,
          class = "alert alert-dismissible alert-success",
          tags$p(
            tags$style(paste0("#",k, "{color: ",clr,"}")),
            k
          ),
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
      if(input$offsetChooseColor == ""){
        clr <- "random"
      }else{
        clr <- input$offsetChooseColor
      }
      rv$offset$colors <- append(rv$offset$colors, clr)

      k <- gsub(" ", "_", input$selectOffset)
      insertUI(
        selector = "#addedOffset",
        ui = tags$div(
          id = k,
          class = "alert alert-dismissible alert-success",
          tags$p(
            tags$style(paste0("#",k, "{color: ",clr,"}")),
            k
          ),
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
    withProgress(message = "Generating Plot", value = 1,{
      tryCatch({
        output$graph <- plotly::renderPlotly({
          uniprotProteinView::drawProtein(proteins = list(preComputed = isolate(rv$files)),
                      types = list(type = isolate(rv$types$type), colors = isolate(rv$types$colors)),
                      descriptionSearch = list(type = isolate(rv$dess$type), colors = isolate(rv$dess$colors)),
                      offSetFeatures = list(type = isolate(rv$offset$type), colors = isolate(rv$offset$colors)),
                      showProgress = FALSE,
                      saveGlobal = FALSE
          )
        })
      },error = function (cond){
        showNotification(paste0("The following error was thrown while trying to generate graph:\n", cond), type = "error")
      }, warning = function (cond){
        showNotification(paste0("The following warning was thrown while trying to generate graph:\n", cond), type = "warning")
      })
    })
  })

}

shiny::shinyApp(ui, server)
