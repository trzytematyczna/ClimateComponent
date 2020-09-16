
library (plumber)

pr <- Plumber$new()

files<-dir("../src/R", pattern = ".R", full.names = TRUE)
# files<-c("../src/R/climate-data.R","../src/R/topic-analyzer.R")
for (file in files) {
  prFile <- plumb(file)
  path <- gsub("\\..*$", "", basename(file))
  path <- paste0("/", path)
  
  pr$mount(path,prFile)
}
  
  pr$run(port=8000)
