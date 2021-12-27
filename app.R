library(shiny)
library(DT)

data <- iris

source("./class.R")
source("./mod_data.R")
source("./mod_multivariate.R")
source("./mod_cluster.R")

server <- function(input, output, session) {

  rv <- reactiveValues(
    analysis = NULL
  )

  observe({
    analysis <- Analysis$new(data)
    analysis$pca()
    analysis$cluster()
    rv$analysis <- analysis$reactive()
  })

  data("data", analysis = rv$analysis)
  multivariate("multivariate", analysis = rv$analysis)
  cluster("cluster", analysis = rv$analysis)

}

ui <- navbarPage(
  title = "R6 Shiny Example",
  tabPanel("Data",
           data_ui("data")),
  tabPanel("Multivariate Analysis",
           multivariate_ui("multivariate")),
  tabPanel("Clustering",
           cluster_ui("cluster"))
)

shinyApp(ui, server)
