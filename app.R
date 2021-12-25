library(shiny)

data <- iris

source("./class.R")

server <- function(input, output, session) {

  rv <- reactiveValues(
    analysis = NULL
  )

  observe({
    rv$analysis <- Analysis$new(data)$reactive()
  })

}

ui <- navbarPage(
  title = "R6 Shiny Example",
  tabPanel("Multivariate Analysis"),
  tabPanel("Clustering")
)

shinyApp(ui, server)
