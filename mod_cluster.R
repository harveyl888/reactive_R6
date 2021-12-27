cluster_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("cluster_ui"))
}

cluster <- function(id, analysis = NULL) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$cluster_ui <- renderUI({
        tagList(
          fluidRow(
            column(6, numericInput(ns("num_clusters"), "Number of Clusters", value = analysis()$get_data("cluster_count"), min = 1)),
          ),
          hr(),
          fluidRow(
            column(6, plotOutput(ns("plt_optimal_clusters"))),
            column(6, plotOutput(ns("plt_clusters")))
          )
        )
      })

      ## get clusters from class
      cluster <- reactive({
        analysis()$get_data("cluster")
      })

      coord <- reactive({
        analysis()$get_data("coords")
      })

      ## update number of clusters in class
      observeEvent(input$num_clusters, {
        isolate({
          analysis()$set_clusters(input$num_clusters)
          analysis()$cluster()
        })
      })

      ## plot - optimal number of clusters
      output$plt_optimal_clusters <- renderPlot({
        req(cluster())
        fviz_nbclust(coord(), k.max = 10, method = "wss", FUNcluster = kmeans)
      })

      ## plot - clusters
      output$plt_clusters <- renderPlot({
        req(cluster())
        fviz_cluster(cluster(), data = coord())
      })


    }
  )
}
