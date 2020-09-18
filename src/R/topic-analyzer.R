# script name:
# topic-analyzer.R

#* @apiTitle TopicAnalyzer
#* @apiDescription Provides comparison of topics

library(jsonlite)
#' @get ping
ping <- function () { return ("OK!"); }


#' Computes lexical similarities between a collection of topics and infers similarity groups.
#' @param topic_word_prob A data.frame of three columns (topic_id, word, prob) giving the probability of a word given a topic.
#' @param grouping_threshold Similarity values under this threshold are removed before grouping topics.
#' @post similarity
similarity <- function (topic_word_prob, grouping_threshold = 0) {

    require (dplyr)

    ##TODO: handling of the json data - they always come as character!
    ##TODO: following talks, topic-probs returns list of either ONE or TWO dataframes (!)
    topic_word_prob<-fromJSON(topic_word_prob)
    topic_word_prob<-as.data.frame(topic_word_prob)
    names(topic_word_prob)<-c("topic_id","word","prob")
    ##
    
    ## filter words
    topic_word_prob <- topic_word_prob %>% filter (prob > 0)
    topic_nb <- topic_word_prob %>% pull (topic_id) %>% unique %>% length
    words <- topic_word_prob %>% group_by (word) %>% summarize (n = n()) %>% filter (n == topic_nb) %>% pull (word)
    topic_word_prob <- topic_word_prob %>% filter (word %in% words)
    
    ## compute similarity as the inverse of divergence
    table <-
        full_join (
            topic_word_prob %>% select (word, topic1_id = topic_id, prob1 = prob),
            topic_word_prob %>% select (word, topic2_id = topic_id, prob2 = prob)
        ) %>%
        group_by (topic1_id, topic2_id) %>%
        summarize (div = 2^sum (prob1 * log2 (prob1 / prob2))) %>%
        filter (topic1_id != topic2_id)

    ## symmetrize similarity
    table <-
        table %>%
        full_join (table %>% select (topic1_id = topic2_id, topic2_id = topic1_id, rev_div = div)) %>%
        mutate (mean_div = (div + rev_div) / 2) %>%
        mutate (similarity = 1 / mean_div) %>%
        filter (topic1_id < topic2_id)

    ## compute clusters
    require (igraph)
    graph <-
        table %>%
        select (from = topic1_id, to = topic2_id, weight = similarity) %>%
        graph_from_data_frame (directed = FALSE)

    ## graph <- graph %>% delete_edges (E(graph) [E(graph)$weight < grouping_threshold])
    clusters <- graph %>% cluster_louvain %>% membership

    ## merge results
    result <-
        list (
            similarity_matrix = table %>% select (topic1_id, topic2_id, divergence = div, similarity),
            similarity_groups = tibble (topic_id = names (clusters), group_id= as.character (clusters))
        )

    return (result)
}


#' Computes lexical specificities within a collection of topics.
#' @param topic_word_prob A data.frame of three columns (topic_id, word, prob) giving the probability of a word given a topic.
#' @post specificity
specificity <- function (topic_word_prob, dim_x = 1, dim_y = 2) {

    require (dplyr)
    require (tidyr)
    require (tibble)
    require (FactoMineR)
    
    data <- topic_word_prob %>% spread (word, prob)
    topics_id <- data %>% pull (topic_id)
    data <- data[-1]

    pca <- data %>% PCA (scale.unit = TRUE, graph = FALSE)

    dim_x_str <- paste0 ("Dim.", dim_x)
    dim_y_str <- paste0 ("Dim.", dim_y)

    pca.words <-
        tibble (
            word = row.names (pca$var$coord),
            dim_x = pca$var$coord[, dim_x_str],
            dim_y = pca$var$coord[, dim_y_str]
        ) %>%
        left_join (
            topic_word_prob %>% group_by (word) %>% summarize (max_prob = max (prob))
        ) %>%
        select (word, max_prob, dim_x, dim_y) %>%
        arrange (desc (max_prob))

    pca.topics <-
        tibble (
            topic_id = topics_id,
            dim_x = pca$ind$coord[, dim_x_str],
            dim_y = pca$ind$coord[, dim_y_str]
        )

    return (list (topics_coord = pca.topics, words_coord = pca.words))
}

