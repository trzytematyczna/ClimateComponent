library (plumber)

setwd ("../src/R/")
r <- plumb ("climate-data.R")
r$run (port = 8000, swagger = TRUE)
