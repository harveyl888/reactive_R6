library(R6)
library(shiny)
library(dplyr)
library(FactoMineR)
library(factoextra)

## reactive R6 class taken from
## https://community.rstudio.com/t/good-way-to-create-a-reactive-aware-r6-class/84890

Analysis <- R6Class("Analysis",

                    public = list(

                      raw_data = NULL,
                      filtered_data = NULL,

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
                          self$filtered_data <- self$raw_data
                        }
                      },

                      ## filter data - keep specific row indices
                      filter = function(rows) {
                        if (length(rows) > 0) {
                          self$filtered_data <- self$raw_data[rows, ]
                          private$invalidate()
                        }
                      },

                      # perform PCA on processed data
                      # ncp - number of dimensions kept in the results
                      # scale.unit - if TRUE then data are scaled to unit variance
                      pca = function(ncp = 5, scale.unit = TRUE) {
                        if (!is.null(self$filtered_data)) {

                          ## extract numeric columns only
                          col_classes <- sapply(self$filtered_data, class)
                          col_remove <- which(!col_classes %in% c("numeric", "integer"))
                          if (length(col_remove) > 0) {
                            data <- self$filtered_data[, -col_remove]
                          } else {
                            data <- self$filtered_data
                          }

                          ## PCA
                          pca <- FactoMineR::PCA(X = data,
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
                      },

                      # set number of clusters
                      set_clusters = function(n = 3) {
                        private$.cluster_count <- n
                        private$invalidate()
                      },

                      get_data = function(type) {
                        if (type == "dimension_reduction") {
                          return(private$.dimension_reduction)
                        } else if (type == "dimension_reduction_type") {
                          return(private$.dimension_reduction_type)
                        } else if (type == "coords") {
                          return(private$.coords)
                        } else if (type == "cluster") {
                          return(private$.cluster)
                        } else if (type == "cluster_count") {
                          return(private$.cluster_count)
                        }
                      },

                      print = function(...) {
                        cat ("Analysis class\n")
                        if (!is.null(self$raw_data)) {
                          cat ("Processed dataset:", ncol(self$raw_data), "columns,", nrow(self$raw_data), "rows\n")
                        } else {
                          cat ("No data imported\n")
                        }
                        invisible(self)
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
