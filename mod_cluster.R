cluster_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, selectizeInput(ns("sel_optim_method"), "Optimal Cluster Method", choices = c("silhouette", "wss", "gap_stat"))),
      column(6, uiOutput(ns("cluster_num_ui")))
    ),
    hr(),
    fluidRow(
      column(6, plotOutput(ns("plt_optimal_clusters"))),
      column(6, plotOutput(ns("plt_clusters")))
    )
  )
}

cluster <- function(id, analysis = NULL) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$cluster_num_ui <- renderUI({
        numericInput(ns("num_clusters"), "Number of Clusters", value = analysis()$get_data("cluster_count"), min = 1)
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
        req(cluster(), input$sel_optim_method)
        fviz_nbclust(coord(), k.max = 10, method = input$sel_optim_method, nboot = 50, FUNcluster = kmeans, print.summary = FALSE, verbose = FALSE)
      })

      ## plot - clusters
      output$plt_clusters <- renderPlot({
        req(cluster())
        fviz_cluster(cluster(), data = coord())
      })


    }
  )
}
