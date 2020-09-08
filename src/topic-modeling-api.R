# script name:
# topic-modeling-load.R

#* @apiTitle ClimateData/Topic
#* @apiDescription Provides pre-trained Topic Models for Datasets

library(dplyr)
library(ggplot2)
library(stringi)
library(readr)
library(textmineR)
library(ggwordcloud)
library(gridExtra)



load_model <- function(DataName = "guardian", K = 10){
  
  # if(k==5|k==9|k==10|k==15){
  #   if(stri_cmp_eq(tolower(DataName),"guardian")){
  #     load(paste0("./",k,"_topics-guardian-articles-alpha-0.1-ngram-1.rda"))
  #   }else if(stri_cmp_eq(tolower(DataName),"twitter")){
  #     load(paste0("./",k,"_topics-twitter-2M-alpha-0.1-ngram-1.rda"))
  #   }else if(stri_cmp_eq(tolower(DataName),"uk")){
  #     load(paste0("./",k,"_topics-uk-speeches-alpha-0.1-ngram-1.rda"))
  #   }
  #   return(m)
  # }
  
  if(K==5|K==9|K==10|K==15){
    if(stri_cmp_eq(tolower(DataName),"guardian")){
      load(paste0("./Models/",K,"_topics-guardian-articles-alpha-0.1-ngram-1.rda"))
    }else if(stri_cmp_eq(tolower(DataName),"twitter")){
      load(paste0("./results/twitter-2M/",K,"_topics-twitter-2M-alpha-0.1-ngram-1.rda"))
    }else if(stri_cmp_eq(tolower(DataName),"uk")){
      load(paste0("./Models/",K,"_topics-uk-speeches-alpha-0.1-ngram-1.rda"))
    }
    return(m)
  }
}


#* Returns dendrogram of topics for selected data and cluster numer TODO:ggplot -> html
#* @param DataName Name of the data {guardian, twitter, uk}
#* @param k Cluster number
#* @png
#* @get /dendrogram 
dendrogram<-function(DataName = "guardian", K = 10){
  
  Model<-load_model(DataName,K)
  
  #Visualising of topics in a dendrogram
  #probability distributions called Hellinger distance, distance between 2 probability vectors
  Model$topic_linguistic_dist <- CalcHellingerDist(Model$phi)
  Model$hclust <- hclust(as.dist(Model$topic_linguistic_dist), "ward.D")
  Model$hclust$labels <- paste(Model$hclust$labels, Model$labels[ , 1])
  print(plot(Model$hclust))
  
  
}


#* Returns worldclod of each of the topic (all in one plot) TODO:ggplot -> html
#* @param DataName Name of the data {guardian, twitter, uk}
#* @param K Cluster number
#* @png
#* @get /top-words-cloud
top_words_cloud<-function(DataName = "guardian", K = 10){
  
  Model<-load_model(DataName,K)
  
  #terms.summary -> word + topic + probability
  TermsSummary <-data.frame(t(Model$phi))
  TermsSummary$word <- rownames(TermsSummary) 
  rownames(TermsSummary) <- 1:nrow(TermsSummary)
  TermsSummary <- TermsSummary %>% 
    reshape2::melt(idvars = "word") %>%
    rename(topic_id = variable, prob = value) %>% 
    tidyr::separate(topic_id, into =c("t","topic_id")) %>% 
    select(-t) %>% 
    group_by(topic_id) %>% 
    arrange(desc(prob))%>%
    select(topic_id,word,prob)

  Top20Summary <- TermsSummary %>% group_by(topic_id) %>% top_n(20)

  Top20Summary <- Top20Summary %>% 
    group_by(topic_id, word) %>% 
    filter(row_number() == 1) %>%
    ungroup() #%>%

  wclist<-list()
  for(i in 1:length(unique(Top20Summary$topic_id))){
    wclist[[i]]<- ggwordcloud(words = subset(Top20Summary, topic_id == i)$word,
                              freq = subset(Top20Summary, topic_id == i)$prob,
                              scale = c(1.8, 0.5),
                              min.freq = 1,
                              max.words = 20,
                              random.order = FALSE,
                              random.color = FALSE,
                              rot.per = 0.2,
                              colors = "#1B9E77")+ #green
                            # colors = "#7570B3")+ #violet
                            # colors = "#E7298A")+ #pink
      ggtitle(paste0('Topic ',i))
  }
  wcall<-grid.arrange(grobs=wclist, top="Top words", ncol=2)
  print(wcall)
  
}


documents<-function(DataName = "guardian", StartDate = "", EndDate = ""){

  if(stri_cmp_eq(tolower(DataName),"guardian")){
    data<-read_csv(paste0("./Data/guardian/guardian-articles-predicted-k10.csv"))
  }else if(stri_cmp_eq(tolower(DataName),"twitter")){
    data<-read_csv(paste0(""))
  }else if(stri_cmp_eq(tolower(DataName),"uk")){
    data<-read_csv(paste0(""))
  }
  
  res <- data %>%
    rename(doc_id = id)
    filter(doc_id, date, author, text)
  
  return(res)
  
}

#* Function returing the list containing 1) dataframe: documents x topics x probabilities 2) dataframe: topics x words x probabilities  
#* @param DataName Name of the data {guardian, twitter, uk}
#* @param K Cluster number
#* @param JustWordsDF option of returning just the second dataframe: topics x words x probabilities
#* @param ProbThreshold value below which the word probabilities will be filtered out
#* @post /topics-probs 
topics_probs<-function(DataName = "guardian", K = 10, JustWordsDF = TRUE, ProbThreshold = 0){
  
  Model<-load_model(DataName,K)
  Out <- list()
  
  TermsSummary <-data.frame(t(Model$phi))
  TermsSummary$word <- rownames(TermsSummary) 
  rownames(TermsSummary) <- 1:nrow(TermsSummary)
  TermsSummary <- TermsSummary %>% 
    reshape2::melt(idvars = "word") %>%
    rename(topic_id = variable, prob = value) %>% 
    tidyr::separate(topic_id, into =c("t","topic_id")) %>% 
    select(-t) %>% 
    group_by(topic_id) %>% 
    filter(prob > 0) %>%
    arrange(desc(prob))%>%
    select(topic_id,word,prob)

  
  if(as.double(ProbThreshold) > 0){
    TermsSummary <- TermsSummary %>%
      filter(prob > as.double(ProbThreshold))
  }
  
  Out$topic_word_prob <- TermsSummary
  
  if(!as.logical(JustWordsDF)){
    DocumentTopic <- data.frame(Model$theta)
    DocumentTopic$document <-rownames(DocumentTopic) 
    rownames(DocumentTopic) <- 1:nrow(DocumentTopic)
    DocumentTopic <- DocumentTopic %>% 
      reshape2::melt(id.vars = "document") %>% 
      rename(topic_id = variable, prob = value, doc_id = document) %>% 
      tidyr::separate(topic_id, into =c("t","topic_id")) %>% 
      select(-t) %>% 
      group_by(doc_id) %>% 
      arrange(desc(prob)) 
    
    Out$doc_topic_prob <- DocumentTopic
    
  }
  
  return(Out)
}

# #* Loads the selected data that contains the id and date of documents, and their topic and topic probability
# #* @param DataName Name of the data to load
# #* @param k Cluster number
# #* @post /loadData 
# getData<-function(DataName = "guardian", k = 10){
#   
#   if(k==5|k==9|k==10|k==15){
#     if(stri_cmp_eq(tolower(DataName),"guardian")){
#       data <- read_csv("./results/guardian-articles/guardian-predicted-id-date-topic-probability.csv")
#     }else if(stri_cmp_eq(tolower(DataName),"twitter")){ 
#       # data <- read_csv("./results/twitter-trained/") TODO file
#     }else if(stri_cmp_eq(tolower(DataName),"uk")){ 
#       
#     }
#     return(data)
#   }  
# }
