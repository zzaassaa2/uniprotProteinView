#source("R/dataRetrieval.R")
#source("R/dataParse.R")
#source("R/dataDrawing.R")
library("shiny")
library("uniprotProteinView")
library("XML")
library("httr")
library("plotly")

ui <- shiny::fluidPage(
  shiny::textInput(inputId = "file", label = "test"),
  shiny::actionButton("do", "Click Me"),
  shiny::uiOutput("checkbox"),
  plotly::plotlyOutput("graph")
)

server <- function (input, output){
  rv <- shiny::reactiveValues(choice = NULL)

  shiny::observeEvent(input$do, {
    rv$choice <- append(rv$choice, input$file)
  })

  output$checkbox <- shiny::renderUI({
    shiny::checkboxGroupInput("checkbox", "label", choices = rv$choice)
  })

  output$graph <- plotly::renderPlotly({
    tryCatch({
      uniprotProteinView::drawProtein(rv$choice,
                  types = list(type = c("domain", "region of interest"), colors = c("red", "purple")),
                  dess = list(type = "phos", colors = "blue"),
                  structure = list(type = c("strand", "helix", "turn"), colors = c("green", "orange", "purple")),
                  showProgress = FALSE
      )
    }, error = function (cond){
      rv$choice <- head(rv$choice, -1)
      message("issue")
      message(cond)
    }
    )
  })
}

shiny::shinyApp(ui, server)
#todo show progress in app
#todo make selectable

#todo it doesn't work on web






#install.packages("rsconnect")
#rsconnect::setAccountInfo(name='zzaassaa2',
#                          token='494B8DA16516804A2A9860397222348E',
#                          secret='<secret key, find at the webpage>')
#rsconnect::deployApp("./R", appName = "appR")
