# script name:
# climate-data.R

library (dplyr, quietly = TRUE, warn.conflicts = FALSE)
library (readr, quietly = TRUE, warn.conflicts = FALSE)
library (jsonlite, quietly = TRUE, warn.conflicts = FALSE)

#* @get /ping
ping <- function () { return ("OK!"); }

#* @post /request-sample
request.sample <- function (req) {
    reqs <- fromJSON (req$postBody)
    get.data (reqs, sample = TRUE)
}

#* @post /request
request <- function (req) {
    reqs <- fromJSON (req$postBody)
    get.data (reqs, sample = FALSE)
}

#* @post /timeline
timeline <- function (corpus = NULL, timescale = "week", topics = FALSE, doc_ids = FALSE, sample = FALSE) {
    reqs <- list()
    
    reqs$timeline$dims$corpus <- list()
    if (! is.null (corpus)) reqs$topics$dims$corpus$select <- corpus
    
    reqs$timeline$dims$date$group_by <- timescale
    if (topics) reqs$timeline$dims$topic <- list()

    reqs$timeline$vars <- c ("doc_nb", "word_nb")
    if (doc_ids) reqs$timeline$vars <- c (reqs$timeline$vars, "doc_ids")

    get.data (reqs, sample)
}


#* @post /topics
topics <- function (corpus = NULL, topic = NULL, doc_ids = FALSE, sample = FALSE) {
    reqs <- list()

    reqs$topics$dims$corpus <- list()
    if (! is.null (corpus)) reqs$topics$dims$corpus$select <- corpus

    reqs$topics$dims$topic <- list()
    if (! is.null (topic)) reqs$topics$dims$topic$select <- topic

    reqs$topics$vars <- c ("doc_nb", "word_nb", "word_dist")
    if (doc_ids) reqs$topics$vars <- c (reqs$topics$vars, "doc_ids")

    get.data (reqs, sample)
}


get.data <- function (reqs, sample = FALSE) {
    start.time <- Sys.time()

    if (sample) { dir <- "./data/sample/" } else { dir <- "./data/full/" }

    ress <- list ()

    ## FOR EACH REQUEST
    for (req.name in names (reqs)) {
        req <- reqs[[req.name]]
        dim.names <- names (req$dims)
        var.names <- req$vars
        data <- NULL

        ## get corpora
        corpus.names <- c ("guardian", "twitter", "uk_parliament")
        if ("corpus" %in% dim.names && length (req$dims[["corpus"]]) > 0 && names (req$dims[["corpus"]]) == "select") {
            corpus.names <- req$dims[["corpus"]]$select
        }

        ## get file name
        all.dim.names <- c ("author", "interactor", "date", "topic")
        file.name <- paste (all.dim.names [all.dim.names %in% dim.names], collapse = ".")

        main.dim.names <- dim.names [dim.names != "corpus"]
        for (corpus.name in corpus.names) {
            corpus.data <- read_csv (paste0 (dir, corpus.name, ".", file.name, ".csv"))
            corpus.data <- corpus.data %>% select (main.dim.names, names (corpus.data) %>% intersect (var.names))
            corpus.data$corpus <- corpus.name

            ## FOR EACH DIMENSION
            for (dim.name in main.dim.names) {
                dim <- req$dims[[dim.name]]

                ## FOR EACH OPERATION
                for (op.name in names (dim)) {
                    op <- dim[[op.name]]

                    if (op.name == "select") {
                        corpus.data <- corpus.data %>% filter (!! sym (dim.name) %in% !! op)
                    }

                    else if (op.name == "select_from") {
                        corpus.data <- corpus.data %>% filter (!! sym (dim.name) >= !! op)
                    }

                    else if (op.name == "select_to") {
                        corpus.data <- corpus.data %>% filter (!! sym (dim.name) <= !! op)
                    }

                    else if (op.name == "group_by") {
                        
                        sup.dim.name <- dim.name
                        if (sup.dim.name == "interactor") sup.dim.name <- "author"
                        corpus.data.tmp <- read_csv (paste0 (dir, corpus.name, ".", sup.dim.name, ".csv"))

                        agg.ids <- function (ids) {
                            paste0 (ids [! is.na (ids) & ids != ""], collapse = " ")
                        }
                        
                        corpus.data <-
                            corpus.data %>%
                            left_join (corpus.data.tmp %>% select (!! sym (dim.name) := sup.dim.name, op)) %>%
                            select (- !! sym (dim.name)) %>%
                            rename (!! sym (dim.name) := !! op) %>%
                            group_by_at (vars (dim.names)) %>%
                            mutate_at (vars (var.names %>% intersect (c ("doc_nb", "word_nb", "char_nb"))), ~ sum (.)) %>%
                            mutate_at (vars (var.names %>% intersect ("doc_ids")), ~ agg.ids (.)) %>%
                            slice (1L) %>%
                            ungroup
                    }
                }
            }

            if (any (var.names %in% c ("author_nb", "author_dist", "mean_follower"))) {
                sub.dim.name <- "community"
                corpus.data.tmp <- read_csv (paste0 (dir, corpus.name, ".", sub.dim.name, ".csv"))

                corpus.data <-
                    corpus.data %>%
                    left_join (corpus.data.tmp %>% select (author = community, var.names [var.names %in% c ("author_nb", "author_dist", "mean_follower")]))
            }
            
            if (is.null (data)) {
                data <- corpus.data
            } else {
                data <- bind_rows (data, corpus.data)
            }
        }

        ## ## AGGREGATE DATA
        ## data <-
        ##     data %>%
        ##     group_by_at (vars (dim.names)) %>%
        ##     mutate_at (vars (var.names %>% intersect ("doc_nb")), ~ sum (.)) %>%
        ##     mutate_at (vars (var.names %>% intersect ("doc_ids")), ~ paste0 (., collapse = " ")) %>%
        ##     slice (1L)

        ## ## ADD UNIDIMENSIONAL VARIABLES
        ## if ("word_dist" %in% var.names) {
        ##     dim.name <- "topic"
        ##     var.name <- "word_dist"
        ##     data.tmp <- read_csv (paste0 (dim.name, ".csv"))
        ##     data <- data %>% left_join (data.tmp %>% select (dim.name, var.name))
        ## }

        ## ## REORDER VARIABLES
        data <- data %>% select (dim.names, var.names)
        ress[[req.name]] <- data
    }

    ## RETURN RESULT
    end.time <- Sys.time()
    ## ress$request.time = as.character (end.time - start.time)

    return (ress)    
}
