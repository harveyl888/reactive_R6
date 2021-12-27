analyze_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("analyze_ui"))
}

analyze <- function(id, analysis = NULL) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$analyze_ui <- renderUI({
        tagList(
          fluidRow(
            column(6, actionButton(ns("but_run"), "Run Analysis"))
          )
        )
      })

      ## run analysis
      observeEvent(input$but_run, {
        analysis()$pca()
        analysis()$cluster()
      })
    }
  )
}
