source("R/dataRetrieval.R")
source("R/dataParse.R")
source("R/dataDrawing.R")
library("shiny")

ui <- fluidPage(
              textInput(inputId = "file", label = "test"),
              actionButton("do", "Click Me"),
              uiOutput("checkbox"),
              plotly::plotlyOutput("graph")
)

server <- function (input, output){
  rv <- reactiveValues(choice = NULL)

  observeEvent(input$do, {
    rv$choice <- append(rv$choice, input$file)
  })

  output$checkbox <- renderUI({
    checkboxGroupInput("checkbox", "label", choices = rv$choice)
  })

  output$graph <- plotly::renderPlotly({
    if(length(rv$choice) > 0){
      drawProtein(rv$choice,
                  types = list(type = c("domain", "region of interest"), colors = c("red", "purple")),
                  dess = list(type = "phos", colors = "blue"),
                  structure = list(type = c("strand", "helix", "turn"), colors = c("green", "orange", "purple")),
                  singleOffset = 2,
                  saveGlobal = TRUE)
    }
  })
}
#XML::isXMLString()
shiny::shinyApp(ui, server)
#todo show progress in app, and remove from logs
#todo make selectable
#todo make file issue






#install.packages("rsconnect")
#rsconnect::setAccountInfo(name='zzaassaa2',
#                          token='494B8DA16516804A2A9860397222348E',
#                          secret='<secret key, find at the webpage>')
#rsconnect::deployApp("./R", appName = "appR")
