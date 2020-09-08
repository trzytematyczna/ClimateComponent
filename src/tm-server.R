library(plumber)
r <- plumb("./Plumber/topic-modeling-api.R")
r$run(port=8000, swagger=TRUE)
