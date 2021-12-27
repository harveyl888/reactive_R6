library(shiny)
library(DT)

data <- iris

source("./class.R")
source("./mod_analyze.R")
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

  analyze("analyze", analysis = rv$analysis)
  multivariate("multivariate", analysis = rv$analysis)
  cluster("cluster", analysis = rv$analysis)

}

ui <- navbarPage(
  title = "R6 Shiny Example",
  tabPanel("Analyze",
           analyze_ui("analyze")),
  tabPanel("Multivariate Analysis",
           multivariate_ui("multivariate")),
  tabPanel("Clustering",
           cluster_ui("cluster"))
)

shinyApp(ui, server)
