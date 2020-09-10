library(plumber)
r <- plumb("./src/ClimateData.R")
r$run(port=8000, swagger=TRUE)
