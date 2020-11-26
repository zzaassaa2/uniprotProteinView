#source("dataRetrieval.R")
#source("dataParse.R")
#source("dataDrawing.R")
library("shiny")
#fileInput selectInput textInput
#inputId label ...
#textOutput
#plotOutput/plotyOutput

#data <- reactive({rnorm(input$num})   to call use data()
#isolate({}) analogous to reactive

#actionbutton in ui, observeEvent(input$click, {})
#observe({}) is similar to observeEvent, but will run code, if any of the used input values need update
#reactivevalues at 1.23

#tags$h1() == <h1></h1>   tags$a(href = "www.rstudio.com", "RStudio") = <a href="www.rstudio.com">RStudio</a>
ui <- fluidPage(
sliderInput(inputId = "num",
            label = "Choose a number",
            value = 25, min = 1, max = 100
),
              plotOutput(outputId = "hist")
)
server <- function (input, output){
  output$hist <- renderPlot({
    title <- "test"
    hist(rnorm(input$num), main = title)
  })
}
shiny::shinyApp(ui, server)

#figure <- drawProtein(
#  proteins = list(source = c("Q04206.xml", "Q9D270.xml"), colors = c("green", "green")),
#  types = list(type = c("domain", "region of interest"), colors = c("red", "purple")),
#  dess = list(type = "phos", colors = "blue"),
#  structure = list(type = c("strand", "helix", "turn"), colors = c("green", "orange", "purple")),
#  singleOffset = 2,
#  saveGlobal = TRUE
#)
#
#ui <- shiny::fluidPage(
#  plotly::plotlyOutput("graph")
#)
#
#server <- function (input, output, session){
#  output$graph <- plotly::renderPlotly({
#    figure
#  })
#}
#
#shiny::shinyApp(ui, server)
#
##todo shiny app, make sure function begins with run   >>use library(shiny)




#install.packages("rsconnect")
#rsconnect::setAccountInfo(name='zzaassaa2',
#                          token='494B8DA16516804A2A9860397222348E',
#                          secret='<secret key, find at the webpage>')
#rsconnect::deployApp("./R", appName = "appR")
