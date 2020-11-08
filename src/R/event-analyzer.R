
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(jsonlite)
library(stringr)
library(tidytext)

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
#* @param date_topic_prob
#* @param event_min_length Minimum duration of the event in units in accordance with the aggregation unit of the input data (day/week/month)
#* @param event_min_prob Minimum probability that above which event is defined
#* @param trends 
#* @param trendsthreshold a threshold value which states what is the minimal difference between two aggregation units (days/weeks/etc) which qualifies upward and downward trend
#* @post /events 
events<-function(timeline, event_min_prob = 0.12, event_min_length = 2, trends = F, trendthreshold = 0.02){

  names(timeline)<-c("corpus","date","topic","doc_nb","word_nb")
  # cleaning the data - leaving only needed columns
  dtp<-as.data.frame(fromJSON(timeline))
  names(dtp)<-c("corpus","date","topic","doc_nb","word_nb")
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

  
  temp <- merge(timeline,data, by=c("date","topic"))
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
  
  result<-full_join(result,trends_trend, by.x=c("topic","x_axis_index"), by.y=c("topic","index"))
  
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
    arrange(desc(doc_id, tf_idf))  
  
  #select top words
  res <- tfidf_data %>% 
    select(word) %>% 
    top_n(top)
  
  return(res)
  
}




# trends<-function(date_topic_prob, event_min_prob = 0.12, event_min_length = 2, trendthreshold = 0.02){
#   
#   date_topic_prob<-as.data.frame(fromJSON(date_topic_prob))
#   names(date_topic_prob)<-c("date","topic","sum_probability")
#   
#   #each time slot (day/month/week) indexed  with same number (all rows with probs of different topics)
#   data <- date_topic_prob %>%
#     group_by(topic) %>%
#     mutate(index=dplyr::row_number()) #%>%
#   
#   #creating mean and median for each topic for data
#   avg_data <- data %>%
#     group_by(topic) %>%
#     summarise(mean=mean(sum_probability),
#               median=median(sum_probability))
#   
#   
#   segments_data <- data %>%
#     group_modify(function(d, ...) {
#       # Compute average
#       avg_probability <- d %>% pull(sum_probability) %>% mean()
#       min_probability <- d %>% pull(sum_probability) %>% min()
#       max_probability <- d %>% pull(sum_probability) %>% max()
#       # Create mask
#       with_mask <- d %>% mutate(mask = sum_probability > avg_probability)
#       # Find segments
#       with_segments <- with_mask %>%
#         mutate(segment = ifelse(mask, 1, 0))
#       current_segment <- 1
#       for (n in 2:nrow(with_segments)) {
#         if (with_segments[n, ]$mask) {
#           if (with_segments[n-1, ]$segment > 0) {
#             with_segments[n, ]$segment <- current_segment
#           } else {
#             current_segment <- current_segment + 1
#             with_segments[n, ]$segment <- current_segment
#           }
#         }
#       }
#       # Find segments start/end points
#       only_segments <- with_segments %>%
#         filter(segment > event_min_prob)
#       
#       coord_segments_min <- only_segments %>%
#         group_by(segment) %>%
#         filter(index == min(index)) %>%
#         mutate(seg_start=index) %>%
#         ungroup() %>%
#         select(seg_start, segment)
#       coord_segments_max <- only_segments %>%
#         group_by(segment) %>%
#         filter(index == max(index)) %>%
#         mutate(seg_end=index) %>%
#         ungroup() %>%
#         select(seg_end) ##segment already in min tibble
#       all_coords <- cbind(coord_segments_min, coord_segments_max) %>%
#         mutate(seg_end=seg_end+1) %>% ### for geom_step --> visualisation moved by 1
#         mutate(min_prob = min_probability,
#                max_prob = max_probability)
#       
#       all_coords
#     })
#   
#   ##seg_len is length of the segment-1 (!) so one-time peaks will have seg_len = 0
#   non_unitary_segments_data <- segments_data %>%
#     mutate(seg_len=seg_end-seg_start)%>%
#     filter(seg_len>event_min_length)
#   
#   
#   trends_segments <- non_unitary_segments_data %>%
#     group_by(topic,segment) %>%
#     group_modify(function(d, ...) {
#       
#       avg_prob <- avg_data %>% 
#         filter(topic == d$topic) %>%
#         pull(mean)
#       
#       data %>%
#         filter(topic == d$topic) %>%
#         ungroup() %>%
#         select(-topic) %>% 
#         filter(index >= d$seg_start,index <= d$seg_end) %>%
#         mutate(diff = sum_probability - lag(sum_probability, n=1, default= avg_prob))
#       # data %>% filter_at(index, between(.,d$seg_start, d$seg_end))
#     }, keep = TRUE)
#   
#   trends_area <- trends_segments %>% 
#     group_by(topic,segment) %>%
#     summarise(area = sum(sum_probability))
#   trends_trend <- trends_segments %>% 
#     mutate(trend = ifelse(abs(diff)<trendthreshold, "same", ifelse(diff<0, "down", "up")))
#   
#   
#   # Prepare labels for dates
#   # label_dates <- data %>%
#   #   filter(topic == 1) %>%
#   #   pull(date) %>%
#   #   as.character()
#   # num_of_indices <- data %>%
#   #   pull(index) %>%
#   #   max()
#   
#   result<-list()
#   
#   result$data<-data #xaxis - index, yaxis - sum_probability
#   result$avg<-avg_data   #horisontal line
#   result$trends<-trends_trend
# }
# 

