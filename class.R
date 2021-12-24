library(R6)
library(shiny)
library(dplyr)

## reactive R6 class taken from
## https://community.rstudio.com/t/good-way-to-create-a-reactive-aware-r6-class/84890

Analysis <- R6Class("Analysis",

                    public = list(

                      raw_data = NULL,

                      initialize = function(data = NULL) {
                        private$reactiveDep <- function(x) NULL
                        self$import(data)
                      },

                      reactive = function() {
                        # Ensure the reactive stuff is initialized.
                        if (is.null(private$reactiveExpr)) {
                          private$reactiveDep <- reactiveVal(0)
                          private$reactiveExpr <- reactive({
                            private$reactiveDep()
                            self
                          })
                        }
                        private$reactiveExpr
                      },

                      import = function(data) {
                        if (!is.null(data)) {
                          self$raw_data <- as_tibble(data)
                        }
                      }

                    ),

                    private = list(
                      reactiveDep = NULL,
                      reactiveExpr = NULL,
                      invalidate = function() {
                        private$count <- private$count + 1
                        private$reactiveDep(private$count)
                        invisible()
                      },
                      count = 0
                    )


                    )
