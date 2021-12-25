library(R6)
library(shiny)
library(dplyr)
library(FactoMineR)

## reactive R6 class taken from
## https://community.rstudio.com/t/good-way-to-create-a-reactive-aware-r6-class/84890

Analysis <- R6Class("Analysis",

                    public = list(

                      raw_data = NULL,
                      processed_data = NULL,

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
                          column_classes <- lapply(data, class)
                          numeric_columns <- which(column_classes == "numeric")
                          self$processed_data <- self$raw_data[, numeric_columns]
                        }
                      },

                      # perform PCA on processed data
                      # ncp - number of dimensions kept in the results
                      # scale.unit - if TRUE then data are scaled to unit variance
                      pca = function(ncp = 5, scale.unit = TRUE) {
                        if (!is.null(self$processed_data)) {
                          pca <- FactoMineR::PCA(X = self$processed_data,
                                                 ncp = ncp,
                                                 scale.unit = scale.unit,
                                                 graph = FALSE)
                          private$.dimension_reduction <- pca
                          private$.dimension_reduction_type <- "pca"
                          private$.coords <- pca$ind$coord[ ,c(1:2)]
                          private$invalidate()
                        }
                      },

                      # perform cluster analysis
                      cluster = function() {
                        multivariate <- private$.dimension_reduction
                        multivariate_method <- private$.dimension_reduction_type
                        if (!is.null(multivariate) & multivariate_method == "pca") {
                          kmeans_cluster <- kmeans(x = private$.coords,
                                                   centers = private$.cluster_count,
                                                   nstart = 25)
                          private$.cluster <- kmeans_cluster
                          private$invalidate()
                        }
                      }


                    ),

                    private = list(
                      .dimension_reduction = NULL,
                      .dimension_reduction_type = "none",
                      .coords = data.frame(`Dim 1` = as.numeric(), `Dim 2` = as.numeric()),
                      .cluster = NULL,
                      .cluster_count = 3,
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
