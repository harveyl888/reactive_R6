library(shiny)

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
    rv$analysis <- Analysis$new(data)$reactive()
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
