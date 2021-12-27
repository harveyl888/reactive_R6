data_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, DTOutput(ns("tab_data")))
  )
}

data <- function(id, analysis = NULL) {

  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      ## table of data
      output$tab_data <- renderDT({
        DT::datatable(isolate(analysis()$raw_data), filter = "top")
      })

      ## filtered rows
      observeEvent(input$tab_data_rows_all, {
        analysis()$filter(rows = input$tab_data_rows_all)
        analysis()$pca()
        analysis()$cluster()
      })
    }
  )
}
