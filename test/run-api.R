library (plumber)


files <- dir ("src/R", pattern = ".R$", full.names = TRUE) ## Robin: '$' at the end of regexp to avoid emacs temp files such as "file.R~" in my case

r <- plumber$new()
for (file in files) {
    path <- gsub ("\\..*$", "", basename (file))
    path <- paste0 ("/", path)

    r.file <- plumb (file)
    r$mount (path, r.file)
}
  
r$run (port = 8000)


