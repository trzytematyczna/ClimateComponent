library (plumber)

##setwd("/data/user/l/rlamarche/zPenelope/shiny/ClimateComponent")
files <- dir ("src/R", pattern = ".R$", full.names = TRUE) ## '$' at the end of regexp to avoid emacs temp files such as "file.R~"

r <- plumber$new()
for (file in files) {
    path <- gsub ("\\..*$", "", basename (file))
    path <- paste0 ("/", path)

    r.file <- plumb (file)
    r$mount (path, r.file)
}
  
r$run (port = 8000)


