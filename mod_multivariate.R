multivariate_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("multivariate_ui"))
}

multivariate <- function(id, analysis = NULL) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$multivariate_ui <- renderUI({
        tagList(
          fluidRow(
            column(6, plotOutput(ns("plt_screeplot"))),
            column(6, plotOutput(ns("plt_varplot")))
          )
        )
      })

      ## return multivariate analysis
      pca <- reactive({
        if (analysis()$get_data("dimension_reduction_type") == "pca") {
          analysis()$get_data("dimension_reduction")
        } else {
          NULL
        }
      })

      ## screeplot
      output$plt_screeplot <- renderPlot({
        req(pca())
        fviz_screeplot(pca(), addlabels = TRUE)
      })

      ## variable plot
      output$plt_varplot <- renderPlot({
        req(pca())
        fviz_pca_var(pca(), col.var = "contrib", gradient.cols = rainbow(3), repel = TRUE, axes = c(1, 2))
      })

    }
  )
}
