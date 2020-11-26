source("../../R/dataRetrieval.R")
source("../../R/dataParse.R")
source("../../R/dataDrawing.R")
library("shiny")

ui <- fluidPage(
              fileInput(inputId = "file", label = "test"),
              uiOutput("checkbox"),
              plotly::plotlyOutput("graph")
)

server <- function (input, output){
  rv <- reactiveValues(choice = NULL)

  observeEvent(input$file, {
    rv$choice <- rbind(rv$choice, input$file)
  })

  output$checkbox <- renderUI({
    checkboxGroupInput("checkbox", "label", choices = rv$choice[,"name"])
  })

  output$graph <- plotly::renderPlotly({
    if(!is.null(rv$choice)){
      m <- vector(mode = "list", length = nrow(rv$choice))
      for(i in seq_len(nrow(rv$choice))){
        m[i] <- rv$choice[i,]$datapath
      }
      drawProtein(m,
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
#todo make not file select, as would be serverside, so just use code and get from uniprot servers






#install.packages("rsconnect")
#rsconnect::setAccountInfo(name='zzaassaa2',
#                          token='494B8DA16516804A2A9860397222348E',
#                          secret='<secret key, find at the webpage>')
#rsconnect::deployApp("./R", appName = "appR")
