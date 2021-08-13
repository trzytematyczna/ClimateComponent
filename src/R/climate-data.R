#* @apiTitle Climate Data
#* @apiDescription This API provides access to metadata of three corpora about public debate on climate change: 79M tweets (social space), 4k articles from The Guardian (media space) and 2.6k speeches given at the UK parliament (political space). This data is made available by the H2020 ODYCCEUS Project (https://www.odycceus.eu/).
#* @apiContact list (name = "API Support", email = "Monika.Ewa.Rakoczy@gmail.com")
#* @apiContact list (name = "API Support", email = "Robin.Lamarche-Perrin@lip6.fr")
#* @apiLicense list (name = "MIT License")
#* @apiVersion 1.0
#* @apiTag H2020_ODYCCEUS

library (dplyr, quietly = TRUE, warn.conflicts = FALSE)
library (readr, quietly = TRUE, warn.conflicts = FALSE)
library (jsonlite, quietly = TRUE, warn.conflicts = FALSE)

#* @get /ping
#* Simple test function.
#* @response 200 OK!
#* @serializer json
ping <- function () { return ("OK!"); }




#* Get temporal evolution of the number of documents and words, optionally dividing them by topic.
#* @param corpus The list of corpora of interest. Possible values are "guardian", "twitter", and/or "uk_parliament". By default, all three corpora are selected.
#* @param timescale The timescale according to which dates are aggregated. Possible values are "day", "week", "month", or "year". Default value is "week".
#* @param by_topic Should the timeline be divided by topic. Default value is "FALSE".
#* @param doc_ids Should document ids be also retrieved. Default value is "FALSE".
#* @param sample Should metadata be extracted from a sample of the three corpora (for testing purposes). Default value is "FALSE".
#* @response 200 timeline An array of observations, each given the number of documents and words for a given corpus at a given date (optionally, associated to a given topic).
#* @serializer json
#* @post /timeline
timeline <- function (corpus = "all", timescale = "week", by_topic = FALSE, doc_ids = FALSE, sample = FALSE) {
    reqs <- list()
    
    reqs$timeline$dims$corpus <- list()
    if (any (corpus != "all")) reqs$timeline$dims$corpus$select <- corpus
    
    reqs$timeline$dims$date$group_by <- timescale
    if (by_topic) reqs$timeline$dims$topic <- list()

    reqs$timeline$vars <- c ("doc_nb", "word_nb")
    if (doc_ids) reqs$timeline$vars <- c (reqs$timeline$vars, "doc_ids")

    get.data (reqs, sample)
}



#* Get lexical distributions of topics that have been learned on the three corpora.
#* @param corpus The list of corpora of interest. Possible values are "guardian", "twitter", and/or "uk_parliament". By default, all three corpora are selected.
#* @param topic The list of topics of interest. By default, all topics are selected.
#* @param doc_ids Should document ids be also retrieved. Default value is "FALSE".
#* @param sample Should metadata be extracted from a sample of the three corpora (for testing purposes). Default value is "FALSE".
#* @response topics An array of observations, each given the lexical distribution of a given topic.
#* @serializer json
#* @post /topics
topics <- function (corpus = "all", topic = "all", doc_ids = FALSE, sample = FALSE) {
    reqs <- list()

    reqs$topics$dims$corpus <- list()
    if (any (corpus != "all")) reqs$topics$dims$corpus$select <- corpus

    reqs$topics$dims$topic <- list()
    if (any (topic != "all")) reqs$topics$dims$topic$select <- topic

    reqs$topics$vars <- c ("doc_nb", "word_nb", "word_dist")
    if (doc_ids) reqs$topics$vars <- c (reqs$topics$vars, "doc_ids")

    get.data (reqs, sample)
}


#* Retweet network.
#* @param doc_ids Should document ids be also retrieved. Default value is "FALSE".
#* @param sample Should metadata be extracted from a sample of the three corpora (for testing purposes). Default value is "FALSE".
#* @serializer json
#* @post /network
network <- function (doc_ids = FALSE, sample = FALSE) {
    reqs <- list()

    reqs$links$dims$corpus$select <- "twitter"
    reqs$links$dims$author$group_by <- "community"
    reqs$links$dims$interactor$group_by <- "community"
    reqs$links$vars <- c ("doc_nb", "word_nb")
    if (doc_ids) reqs$links$vars <- c (reqs$links$vars, "doc_ids")

    reqs$nodes$dims$corpus$select <- "twitter"
    reqs$nodes$dims$author$group_by <- "community"
    reqs$nodes$dims$topic <- list()
    reqs$nodes$vars <- c ("doc_nb", "word_nb")
    if (doc_ids) reqs$nodes$vars <- c (reqs$nodes$vars, "doc_ids")

    reqs$communities$dims$corpus$select <- "twitter"
    reqs$communities$dims$author$group_by <- "community"
    reqs$communities$vars <- c ("doc_nb", "word_nb", "author_nb", "author_dist")
    if (doc_ids) reqs$communities$vars <- c (reqs$communities$vars, "doc_ids")
    
    reqs$topics$dims$corpus$select <- "twitter"
    reqs$topics$dims$topic <- list()
    reqs$topics$vars <- c ("doc_nb", "word_nb", "word_dist")
    if (doc_ids) reqs$topics$vars <- c (reqs$topics$vars, "doc_ids")

    get.data (reqs, sample)
}



#* Get metadata from a sample of the three corpora according to some structured JSON request describing (1) dimensions of interest (among corpus, author, interactor, date, and topic), (2) eventual filtering and/or aggregation of the elements in these dimensions, (3) variables of interest (among number of documents, of words, of characters, and list of document ids).
#* @param req A structured JSON request describing what metadata should be retreived.
#* @response res Resulting metadata from a sample of the three corpora.
#* @serializer json
#* @post /metadata
metadata <- function (req) {
    reqs <- fromJSON (req$postBody)
    get.data (reqs, sample = FALSE)
}



#* Get metadata from the three corpora according to some structured JSON request describing (1) dimensions of interest (among corpus, author, interactor, date, and topic), (2) eventual filtering and/or aggregation of the elements in these dimensions, (3) variables of interest (among number of documents, of words, of characters, and list of document ids).
#* @param req A structured JSON request describing what metadata should be retreived.
#* @response res Resulting metadata from the three corpora.
#* @serializer json
#* @post /metadata-sample
metadata.sample <- function (req) {
    reqs <- fromJSON (req$postBody)
    get.data (reqs, sample = TRUE)
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
            corpus.data <- corpus.data %>% select (all_of (main.dim.names), names (corpus.data) %>% intersect (var.names))
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
                            left_join (corpus.data.tmp %>% select (!! sym (dim.name) := all_of (sup.dim.name), all_of (op))) %>%
                            select (- !! sym (dim.name)) %>%
                            rename (!! sym (dim.name) := !! op) %>%
                            group_by_at (vars (all_of (dim.names))) %>%
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
        data <- data %>% select (all_of (dim.names), all_of (var.names))
        ress[[req.name]] <- data
    }

    ## RETURN RESULT
    end.time <- Sys.time()
    ## ress$request.time = as.character (end.time - start.time)

    return (ress)    
}
