
library(dplyr)
# library(ggplot2)
library(readr)
library(tidyr)
library(jsonlite)
library(stringr)
library(tidytext)
library(purrr)



#* @get /ping
#* Simple test function.
#* @response 200 OK!
#* @serializer json
ping <- function () { return ("OK!"); }


#* Returns dummy event data in json
#* @get /evdoc 
# evdoc<-function(){
#   # datajson<-read_json("data/date_topic_prob.json")
  # datajson<-read_json("data/res_guardian_timeline.json")
  # dat<-datajson$timeline
  # data<-data.frame(matrix(unlist(dat), nrow=length(dat), byrow=T))
  # return(datajson)

# }



#* Returns higlighted events defined as document probability higher than event_min_prob lasting at least event_min_length; output:  result$data -  #xaxis - index, yaxis - sum_probability, result$avg - horisontal line, result$segments - for a single rectangle: xmin=seg_start, xmax=seg_end (x-axis bounds) ymin=min_prob, ymax=max_prob (y-axis bounds )
#* @param timeline An array of observations, each giving the lexical distribution of a given topic (see /climate-data/topics).
#* @param event_min_length Minimum duration of the event in units in accordance with the aggregation unit of the input data (day/week/month)
#* @param event_min_prob Minimum probability that above which event is defined
#* @param trends 
#* @param trendsthreshold a threshold value which states what is the minimal difference between two aggregation units (days/weeks/etc) which qualifies upward and downward trend
#* @serializer json
#* @post /events 
events<-function(timeline, event_min_prob = 0.12, event_min_length = 2, trends = F, trendthreshold = 0.02){

  
  # Is it an already parsed set of arguments (e.g. using curl and application/json content)
  if (is.data.frame(timeline)) {
    print(timeline)
    dtp<-timeline
    print(dtp)
  } else {
    print(timeline)
    dtp<-fromJSON(timeline)$timeline
    print(dtp)
  }

  names(dtp)<-c("corpus","date","topic","doc_nb","word_nb")
  
  print(head(dtp))

  # dtp<- dtp %>% 
  #   select(date,topic,doc_nb) %>%  
  #   mutate(topic = str_replace_all(topic, 'G',"")) %>%  
  #   mutate(topic = str_replace_all(topic, 'T',"")) %>%  
  #   mutate(topic = str_replace_all(topic, 'P',""))
  
  dtp <- dtp  %>% mutate_if(is.factor, as.character)
  dtp$doc_nb<- as.double(dtp$doc_nb)
  
  #converting doc_nb to normalized probability per date and topic
  date_topic_prob <- dtp%>%
    group_by(date) %>%
    mutate(doc_sums=sum(doc_nb)) 
  date_topic_prob <- date_topic_prob%>%
    mutate(sum_probability=doc_nb/doc_sums) %>%
    select(date,topic,sum_probability)
  
  #each time slot (day/month/week) indexed  with same number (all rows with probs of different topics)
  data <- date_topic_prob %>%
    group_by(topic) %>%
    mutate(index=dplyr::row_number()) #%>%
  #creating mean and median for each topic for data
  avg_data <- data %>%
    group_by(topic) %>%
    summarise(mean=mean(sum_probability),
              median=median(sum_probability))
  
  #enumerating each slot with event with different index per topic (operates on each group from groupby!)
  segments_data <- data %>%
    group_modify(function(d, ...) {
      # Compute average
      avg_probability <- d %>% pull(sum_probability) %>% mean()
      min_probability <- d %>% pull(sum_probability) %>% min()
      max_probability <- d %>% pull(sum_probability) %>% max()
      # Create mask
      with_mask <- d %>% mutate(mask = sum_probability > event_min_prob)
      # Find segments
      with_segments <- with_mask %>%
        mutate(segment = ifelse(mask, 1, 0))
      current_segment <- 1
      for (n in 2:nrow(with_segments)) {
        if (with_segments[n, ]$mask) {
          if (with_segments[n-1, ]$segment > 0) {
            with_segments[n, ]$segment <- current_segment
          } else {
            current_segment <- current_segment + 1
            with_segments[n, ]$segment <- current_segment
          }
        }
      }
      # Find segments start/end points
      only_segments <- with_segments %>%
        filter(segment > 0)
      
      coord_segments_min <- only_segments %>%
        group_by(segment) %>%
        filter(index == min(index)) %>%
        mutate(seg_start=index) %>%
        ungroup() %>%
        select(seg_start, segment)
      coord_segments_max <- only_segments %>%
        group_by(segment) %>%
        filter(index == max(index)) %>%
        mutate(seg_end=index) %>%
        ungroup() %>%
        select(seg_end) ##segment already in min tibble
      all_coords <- cbind(coord_segments_min, coord_segments_max) %>%
        mutate(seg_end=seg_end+1) %>% ### for geom_step --> visualisation moved by 1
        mutate(min_prob = min_probability,
               max_prob = max_probability)
      
      all_coords
    })
  
 
  non_unitary_segments_data <- segments_data %>%
    mutate(seg_len=seg_end-seg_start+1)%>%
    filter(seg_len>=event_min_length)   ##length of period of the event used here -- defines how long the event supposed to last (in time slot defined by aggregation from data)

  temp <- merge(dtp,data, by=c("date","topic"))
  non_unitary_filt <- non_unitary_segments_data %>% select(-segment)
   
  # all_res <- left_join(temp, non_unitary_filt, by = c("topic")) 
  # asd<-all_res %>% filter(index>=seg_start, index<= seg_end)
  # asd2<-left_join(temp,asd)%>%
  #   arrange(topic,date)
  
  all_res <- left_join(temp, non_unitary_filt, by = c("topic"))  %>%
    filter(index>=seg_start, index<= seg_end) %>%
    full_join(temp) %>%
    arrange(topic,date)
  
  result <- merge(all_res,avg_data, by="topic") %>%
    select(corpus, date, topic, doc_nb, word_nb, index, sum_probability, seg_start, seg_end, min_prob, max_prob, seg_len, mean, median)%>%
    rename(x_axis_index = index, 
           y_axis_sumarized_prob=sum_probability, 
           x_seg_start=seg_start, 
           x_seg_end=seg_end, 
           y_seg_start=min_prob, 
           y_seg_end=max_prob, 
           topic_mean=mean, 
           topic_median=median)
  

  if(trends){
  trends_segments <- non_unitary_segments_data %>%
    group_by(topic,segment) %>%
    group_modify(function(d, ...) {
      
      avg_prob <- avg_data %>% 
        filter(topic == d$topic) %>%
        pull(mean)
      
      data %>%
        filter(topic == d$topic) %>%
        ungroup() %>%
        select(-topic) %>% 
        filter(index >= d$seg_start,index <= d$seg_end) %>%
        mutate(diff = sum_probability - lag(sum_probability, n=1, default= avg_prob))
      # data %>% filter_at(index, between(.,d$seg_start, d$seg_end))
    }, keep = TRUE)
  
  trends_area <- trends_segments %>% 
    group_by(topic,segment) %>%
    summarise(area = sum(sum_probability))
  trends_trend <- trends_segments %>% 
    mutate(trend = ifelse(abs(diff)<trendthreshold, "same", ifelse(diff<0, "down", "up")))
  
  trends_selected <- trends_trend %>%
    ungroup()%>%
    select(topic,index,date,diff,trend)%>%
    rename(x_axis_index=index)
  result <- full_join(result,trends_selected, by=c("topic","x_axis_index","date"))#, by.y=c("topic","index","date"))
  
  }
  
  return(result)
  
}


# data<-read_csv(paste0("./data/guardian-sample.csv"), col_types = cols (doc_id = col_character()))
# 
# id_text<-data%>%select(doc_id, title)
# names(id_text)<-c("doc_id","text")


#* Function returing most significant words (using tfidf) from given texts (to be used for naming the events from events())
#* @param id_text data with only doc_id and text already filtered by date and topic
#* @param top how many words (descending importance) to return, default 10
#* @post /eventwords 
eventwords<-function(id_text, top = 10){
  
  #calculate word frequencies
  wordsFreq <- id_text %>% 
    unnest_tokens(word, text, to_lower = TRUE) %>%
    filter(!str_detect(word, "^[0-9]*$")) %>%
    anti_join(stop_words) %>% #removing stop words
    count(doc_id, word, sort = TRUE)
  
  #caluculate tfidf
  tfidf_data <- wordsFreq %>%
    bind_tf_idf(word, doc_id, n) %>%
    arrange(desc(tf_idf))  
  
  #select top words
  res <- tfidf_data %>% 
    top_n(n = top, wt = tf_idf)%>%
    select(word) 
    
  return(res)
  
}



# data<-read_csv("./data/sample/guardian.author.date.csv")
# data<-read_csv("./data/sample/uk_parliament.author.date.csv")

#* Returns all metadata (including text!) of tweets/speeches/articles given the selected ids
#* @param ids metadata structure from function /climate-data/timeline (columns: "corpus","date","topic","doc_nb","word_nb", "doc_ids")
#* @param corpus
#* @serializer json
#* @post /texts 
texts<-function(ids, corpus = "guardian"){
  
  # Is it an already parsed set of arguments (e.g. using curl and application/json content)
  if (is.data.frame(ids)) {
    data<-ids
  } else {
    data<-fromJSON(ids)$timeline
  }
  names(data)<-c("corpus","date","topic","doc_nb","word_nb", "doc_ids")
  
  unnested_dfif <- data %>% 
    select(date, doc_ids) %>%
    group_split(date) %>%
    map_dfr(~ .x %>% 
              mutate_at(-1, ~ list(unlist(.))) %>% 
              unnest(c(doc_ids)))
  ids_string<-unnested_dfif %>% pull(doc_ids) %>% paste(collapse = "|")
   
  # x<-paste0("xargs -I {} grep \"^{}\" data/",corpus,"-all-data.csv < ", ids$doc_ids)
  # cat(x)
  xcommand<-paste0('grep -P \"', ids_string,'\" ./data/content/',corpus,'.content.csv ')
 system_res <-system(xcommand,intern = TRUE)
 df<-as.data.frame(system_res, stringsAsFactors = F)
 if(corpus == "guardian"){
  
   result_data<- read.table(text = df$system_res, sep =",", header = F, stringsAsFactors = FALSE, quote = "\"", col.names = c("doc_id", "type", "url", 
                                                                                                                    "authors","authors_nb",
                                                                                                                    "section","tags", "tags_nb",
                                                                                                                    "date_published","share_count",
                                                                                                                    "comment_nb","title", 
                                                                                                                    "description","text", "length",
                                                                                                                    "t_1", "t_2", "t_3", "t_4", 
                                                                                                                    "t_5", "t_6", "t_7", "t_8", 
                                                                                                                    "t_9", "t_10", "date"))
 }
 else if(corpus == "uk_parliament"){
   result_data<-read.table(text = df$system_res, sep =",", header = F, stringsAsFactors = FALSE, quote = "\"", col.names = c("doc_id", "date", 
                                                                                                                    "discussion_title","name",
                                                                                                                    "party","speaker_id","text",
                                                                                                                    "length","t_1","t_2","t_3",
                                                                                                                    "t_4","t_5","t_6","t_7",
                                                                                                                    "t_8","t_9","t_10"))
 }
 else{
   result_data<-read.table(df$system_res, sep =",", header = F, stringsAsFactors = FALSE,  col.names = c("doc_id","date","retweetcount",
                                                                                                         "from_user_id","from_user_name",
                                                                                                                    "from_user_followercount",
                                                                                                                    "text","t_1","t_2","t_3",
                                                                                                                    "t_4","t_5","t_6","t_7",
                                                                                                                    "t_8","t_9"))
 }

   return(result_data)  
}
