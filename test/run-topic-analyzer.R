library (plumber)

setwd ("../src/R/")
r <- plumb ("topic-analyzer.R")
r$run (port = 8001, swagger = TRUE)
