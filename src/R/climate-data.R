# script name:
# climate-data.R

suppressMessages (library(dplyr))
suppressMessages (library(ggplot2))
suppressMessages (library(stringi))
suppressMessages (library(readr))
suppressMessages (library(textmineR))
suppressMessages (library(ggwordcloud))
suppressMessages (library(gridExtra))

#* @get /ping
ping <- function () { return ("OK!"); }


load_model <- function(corpus = "guardian", k = 10){
  
  if(k==5|k==9|k==10|k==15){
    if(stri_cmp_eq(tolower(corpus),"guardian")){
      load(paste0("./data/",k,"_topics-guardian-articles-alpha-0.1-ngram-1.rda"))
    }else if(stri_cmp_eq(tolower(corpus),"twitter")){
      # load(paste0("./results/twitter-2M/",k,"_topics-twitter-2M-alpha-0.1-ngram-1.rda"))
    }else if(stri_cmp_eq(tolower(corpus),"uk")){
      # load(paste0("./data/",k,"_topics-uk-alpha-0.1-ngram-1.rda"))
      load(paste0("./results/parliamentary/10_topics-uk-alpha-0.1-ngram-1.rda"))
    }
    return(m)
  }
}

#* Function returing sample of documents
#* @param corpus Name of the corpus {guardian, twitter, uk}
#* @param start_date Start of period of data to return
#* @param end_date  End of period of data to return
#* @get /sampledocuments 
sampledocuments<-function(corpus = "guardian", start_date = "", end_date = ""){

  #loading corpus
  if(stri_cmp_eq(tolower(corpus),"guardian")){
    data<-read_csv(paste0("./data/guardian-sample.csv"), col_types = cols (doc_id = col_character()))
  }else if(stri_cmp_eq(tolower(corpus),"twitter")){
    data<-read_csv(paste0("./data/twitter-sample.csv"))
  }else if(stri_cmp_eq(tolower(corpus),"uk")){
    data<-read_csv(paste0("./data/uk-sample.csv"))
  }
  
  #filtering data with dates
  if(!stri_isempty(start_date)& !stri_isempty(end_date)){
     start_date<-as.Date(start_date)
     end_date<-as.Date(end_date)
    if(start_date<end_date & start_date>=min(data$date) & end_date<=max(data$date)){
      data<-data%>%filter(between(date,start_date,end_date))
    } else{
      print("wrong data!")
    }
  }
  
  return(data)
  
}

#* Function returing the list containing one or two dataframes: documents x topics x probabilities and topics x words x probabilities  
#* @param corpus Name of the corpus {guardian, twitter, uk}
#* @param k Cluster number
#* @param just_words option of returning just the second dataframe: topics x words x probabilities
#* @param prob_threshold value below which the word probabilities will be filtered out
#* @post /topics-probs 
topics_probs<-function(corpus = "guardian", k = 10, just_words = TRUE, prob_threshold = 0){
  
  #loading model .rda
  Model<-load_model(corpus,k)
  Out <- list()
  
  #extracting dataframe with words
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

  #thresholding words that have probability > threshod
  if(as.double(prob_threshold) > 0){
    TermsSummary <- TermsSummary %>%
      filter(prob > as.double(prob_threshold))
  }
  
  #saving words to result
  Out$topic_word_prob <- TermsSummary
  
  
  #extracting dataframe with documents x topics
  if(!as.logical(just_words)){
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
    
    #saving to result
    Out$doc_topic_prob <- DocumentTopic
    
  }
  
  return(Out)
}

