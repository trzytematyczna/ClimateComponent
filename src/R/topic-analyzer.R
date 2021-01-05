#* @apiTitle Topic Analyzer
#* @apiDescription This API provides tools for the visualisation and comparison of lexical distributions resulting from a topic model. These tools are made available by the H2020 ODYCCEUS Project (https://www.odycceus.eu/).
#* @apiContact list (name = "API Support", email = "Monika.Ewa.Rakoczy@gmail.com")
#* @apiContact list (name = "API Support", email = "Robin.Lamarche-Perrin@lip6.fr")
#* @apiLicense list (name = "MIT License")
#* @apiVersion 1.0
#* @apiTag H2020_ODYCCEUS

suppressMessages (require (dplyr))
suppressMessages (require (igraph))
suppressMessages (require (tidyr))
suppressMessages (require (tibble))
suppressMessages (require (FactoMineR))
suppressMessages (library (scales))
suppressMessages (library (RColorBrewer))
suppressMessages (library (ggrepel))

#* @get /ping
#* Simple test function.
#* @response 200 OK!
#* @serializer json
ping <- function () { return ("OK!"); }




#* Compute similarity network and similarity groups for a set of topics based on their lexical distributions.
#* @param topics An array of observations, each giving the lexical distribution of a given topic (see /climate-data/topics).
#* @param grouping_threshold The quantile of links to remove from the network, depending on their similarity score, before computing the similarity groups. Default value is 0.
#* @response 200 groups The list of similarity groups, one for each topic.
#* @content network The list of similarity scores, one for each couple of topics.
#* @serializer json
#* @post /similarity
similarity <- function (topics, grouping_threshold = 0) {
    ## topics <- top$topics
    
    ## extract word probs
    dist <- topics$word_dist %>% strsplit (" ")
    topic_word_prob <- lapply (1:length(dist), function (i) {
        d <- split (dist[[i]], 1:length(dist[[i]]) %% 2 == 0)
        tibble (topic = topics$topic[i], word = unname (d[[1]]), prob = as.numeric (unname (d[[2]])))
    }) %>% bind_rows

    ## normalise probs
    topic_word_prob <-
        topic_word_prob %>%
        group_by (topic) %>%
        mutate (prob = prob / sum (prob))

    ## compute similarity as the inverse of divergence
    table <-
        full_join (
            topic_word_prob %>% select (word, topic1 = topic, prob1 = prob),
            topic_word_prob %>% select (word, topic2 = topic, prob2 = prob)
        ) %>%
        group_by (topic1, topic2) %>%
        summarize (div = 2^sum (prob1 * log2 (prob1 / prob2))) %>%
        filter (topic1 != topic2)

    ## symmetrize similarity
    table <-
        table %>%
        full_join (table %>% select (topic1 = topic2, topic2 = topic1, rev_div = div)) %>%
        mutate (mean_div = (div + rev_div) / 2) %>%
        mutate (similarity = 1 / mean_div) %>%
        filter (topic1 < topic2)

    ## compute clusters
    graph <-
        table %>%
        select (from = topic1, to = topic2, weight = similarity) %>%
        graph_from_data_frame (directed = FALSE)
    
    threshold <- quantile (E(graph)$weight, probs = grouping_threshold)
    graph <- graph %>% delete_edges (E(graph) [E(graph)$weight <= threshold])
    clusters <- graph %>% cluster_louvain %>% membership

    array <- tibble (topic = names (clusters), group = as.numeric (clusters))
    array <- topics %>% select (- word_dist) %>% left_join (array) %>% select (topic, everything())

    ## merge results
    result <-
        list (
            network = table %>% select (topic1, topic2, divergence = mean_div, similarity),
            groups = array
        )

    return (result)
}


similarity_plot <- function (network, groups, edge_threshold = 0) {
    ## network <- sim$network
    ## groups <- sim$groups

    edges <-
        network %>%
        rename (weight = similarity)

    vertices <-
        groups %>%
        group_by (corpus) %>%
        mutate (prob = word_nb / sum (word_nb))

    graph <- graph_from_data_frame (edges, directed = FALSE, vertices)

    threshold <- quantile (E(graph)$weight, probs = edge_threshold)
    graph <- graph %>% delete_edges (E(graph) [E(graph)$weight <= threshold])
    
    E(graph)$width <- E(graph)$weight %>% rescale (from = c (0, max (E(graph)$weight)), to = c (0, 20))
    V(graph)$size <- V(graph)$prob %>% rescale (from = c (0, max (V(graph)$prob)), to = c (0, 30))
    V(graph)$label.cex <- V(graph)$prob %>% rescale (from = c (0, max (V(graph)$prob)), to = c (0, 2))

    colors <- brewer.pal (n = length (unique (V(graph)$group)), name = "Set1")
    V(graph)$color <- colors [V(graph)$group]

    ends <- graph %>% ends (E(graph), names = FALSE)
    E(graph)$color <- ifelse (V(graph)$color [ends[,1]] == V(graph)$color [ends[,2]], V(graph)$color [ends[,1]], "#555555")

    plot (
        graph,
        layout = graph %>% layout_with_fr,
        vertex.label.color = "black"
        )
}




#* Compute specificity 2D map for a set of topics based on their lexical distributions using Principal Component Analysis (PCA).
#* @param topics An array of observations, each giving the lexical distribution of a given topic (see /climate-data/topics).
#* @param dim_x Dimension of the PCA which should be used for the x-axis. Default value is 1.
#* @param dim_y Dimension of the PCA which should be used for the y-axis. Default value is 2.
#* @response 200 topics The list of topics' coordinates in the 2D map. words The list of words' coordinates in the 2D map.
#* @serializer json
#* @post /specificity
specificity <- function (topics, dim_x = 1, dim_y = 2) {
    ## topics <- top$topics
    
    ## extract word probs
    dist <- topics$word_dist %>% strsplit (" ")
    topic_word_prob <- lapply (1:length(dist), function (i) {
        d <- split (dist[[i]], 1:length(dist[[i]]) %% 2 == 0)
        tibble (topic = topics$topic[i], word = unname (d[[1]]), prob = as.numeric (unname (d[[2]])))
    }) %>% bind_rows

    ## normalise probs
    topic_word_prob <-
        topic_word_prob %>%
        group_by (topic) %>%
        mutate (prob = prob / sum (prob))

    data <- topic_word_prob %>% rename (my_topic_field = topic) %>% spread (word, prob)
    topic.list <- data %>% pull (my_topic_field)
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
            topic = topic.list,
            dim_x = pca$ind$coord[, dim_x_str],
            dim_y = pca$ind$coord[, dim_y_str]
        )

    pca.topics <-
        topics %>%
        select (- word_dist) %>%
        left_join (pca.topics) %>%
        select (topic, everything())
    
    return (list (topics = pca.topics, words = pca.words))
}


specificity_plot <- function (topics, words, word_threshold = 0) {
    ## topics <- spe$topics
    ## words <- spe$words

    m <- max (sqrt (topics$dim_x^2 + topics$dim_y^2))
    topics$dim_x <- topics$dim_x / m
    topics$dim_y <- topics$dim_y / m
    threshold <- quantile (words$max_prob, probs = word_threshold)

    topics %>%
        ggplot () +
        xlim (-1, 1) + ylim (-1, 1) +
        geom_hline (yintercept = 0) +
        geom_vline (xintercept = 0) +
        geom_point (aes (x = dim_x, y = dim_y), size = 15, shape = 1, color = "blue") +
        geom_text (aes (x = dim_x, y = dim_y, label = topic), size = 6, color = "blue", lineheight = 0.75) +
        geom_text_repel (data = words %>% filter (max_prob >= threshold), aes (label = word, x = dim_x, y = dim_y), size = 3, color = "black", force = 0.01, box.padding = 0, segment.alpha = 0) +
        theme (legend.position = "none", panel.background = element_blank())

}
