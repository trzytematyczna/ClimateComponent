
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(jsonlite)


# data_dir <- "./results/twitter-trained/assign-joined-data/assign-3.csv"
# df<-read_csv(data_dir, col_types = cols (id = col_character()))
# df$date<-as.Date(df$date)
# df<-df[1:2000000,]
# probs<-df%>%select(-from_user_id,-from_user_name,-from_user_followercount,-text)%>%
#   # mutate(month=format(date,divide.by))%>%
#   gather(topic, probability, t_1:t_9) %>% ##topic_number k_list
#   tidyr::separate(topic, into =c("t","topic")) %>%
#   select(-t)%>%
#   select(-id)
# 
# grouped.sp <- probs%>%
#   group_by(date,topic) %>%
#   summarise(sum_probability=mean(probability))
# date_topic_prob<-grouped.sp
# library(jsonlite)
# date_topic_prob[1:81,]%>%toJSON()%>%write("date_topic_prob.json")

# result$data -  #xaxis - index, yaxis - sum_probability
# result$avg - horisontal line
# result$segments - for a single rectangle: xmin=seg_start, xmax=seg_end (x-axis bounds) ymin=min_prob, ymax=max_prob (y-axis bounds )


#* Returns higlighter events defined as document probability higher than eventminprob lasting at least eventminlength
#* @param date_topic_prob
#* @param eventminlength Minimum of length of the event in units in accordance with the aggregation unit of the input data (day/week/month)
#* @param eventminprob Minimum probability that event must have
#* @serializer png
#* @get /events 
events<-function(date_topic_prob, eventminprob = 0.12, eventminlength = 2){

  date_topic_prob<-as.data.frame(fromJSON(date_topic_prob))
  names(date_topic_prob)<-c("date","topic","sum_probability")
  # print(date_topic_prob)
  #each time slot (day/month/week) indexed  with same number (all rows with probs of different topics)
  data <- date_topic_prob %>%
    group_by(topic) %>%
    mutate(index=dplyr::row_number()) #%>%
  #creating mean and median for each topic for data
  avg_data <- data %>%
    group_by(topic) %>%
    summarise(mean=mean(sum_probability),
              median=median(sum_probability))
  
  segments_data <- data %>%
    group_modify(function(d, ...) {
      # Compute average
      avg_probability <- d %>% pull(sum_probability) %>% mean()
      min_probability <- d %>% pull(sum_probability) %>% min()
      max_probability <- d %>% pull(sum_probability) %>% max()
      # Create mask
      with_mask <- d %>% mutate(mask = sum_probability > eventminprob)
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
  
  ##seg_len is length of the segment-1 (!) so one-time peaks will have seg_len = 0
  non_unitary_segments_data <- segments_data %>%
    mutate(seg_len=seg_end-seg_start)%>%
    filter(seg_len>eventminlength)   ##length of period of the event used here -- defines how long the event supposed to last (in time slot defined by aggregation from data)

  
  
  # Prepare labels for dates
  label_dates <- data %>%
    filter(topic == 1) %>%
    pull(date) %>%
    as.character()
  num_of_indices <- data %>%
    pull(index) %>%
    max()
  
  result<-list()
  
  result$data<-data #xaxis - index, yaxis - sum_probability
  result$avg<-avg_data   #horisontal line
  result$segments<-non_unitary_segments_data #rectagle bounds: xaxis: seg_start, seg_end; yaxis: min_prob, ymax=max_prob
  
  # 
  # #only segments
  # p <- ggplot(data, aes(x=index, y=sum_probability)) +
  #   geom_step() +
  #   # geom_line()+
  #   geom_hline(data=avg_data, aes(yintercept=mean), lty=2, color="red") +
  #   geom_rect(data=non_unitary_segments_data,
  #             inherit.aes = FALSE,
  #             aes(xmin=seg_start, xmax=seg_end, ymin=min_prob, ymax=max_prob),
  #             alpha=0.3,
  #             fill="green") +
  #   facet_grid(topic~., scales = "free_y")+
  #   ylab("Probability")+
  #   xlab("Date")+
  #   # ylim(0.3, 0.37)+
  #   scale_x_continuous(breaks = seq(1,num_of_indices,4), labels=label_dates[seq(1,num_of_indices,4)]) +
  #   theme(axis.text.x = element_text(angle = 90, hjust=1))
  # 
  # print(p)
  # 
  
}


#* Returns events (same as event function) with highlighted trends: colors signifying increasing/decreasing/same trend
#* @param date_topic_prob
#* @param eventminlength Minimum of length of the event in units in accordance with the aggregation unit of the input data (day/week/month)
#* @param eventminprob Minimum probability that event must have
#* @post /trends 
trends<-function(date_topic_prob, eventminprob = 0.12, eventminlength = 2, trendthreshold = 0.02){
  
  date_topic_prob<-as.data.frame(fromJSON(date_topic_prob))
  names(date_topic_prob)<-c("date","topic","sum_probability")
  
  #each time slot (day/month/week) indexed  with same number (all rows with probs of different topics)
  data <- date_topic_prob %>%
    group_by(topic) %>%
    mutate(index=dplyr::row_number()) #%>%
  
  #creating mean and median for each topic for data
  avg_data <- data %>%
    group_by(topic) %>%
    summarise(mean=mean(sum_probability),
              median=median(sum_probability))
  
  
  segments_data <- data %>%
    group_modify(function(d, ...) {
      # Compute average
      avg_probability <- d %>% pull(sum_probability) %>% mean()
      min_probability <- d %>% pull(sum_probability) %>% min()
      max_probability <- d %>% pull(sum_probability) %>% max()
      # Create mask
      with_mask <- d %>% mutate(mask = sum_probability > avg_probability)
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
        filter(segment > eventminprob)
      
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
  
  ##seg_len is length of the segment-1 (!) so one-time peaks will have seg_len = 0
  non_unitary_segments_data <- segments_data %>%
    mutate(seg_len=seg_end-seg_start)%>%
    filter(seg_len>eventminlength)
  
  
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
  
  
  # Prepare labels for dates
  label_dates <- data %>%
    filter(topic == 1) %>%
    pull(date) %>%
    as.character()
  num_of_indices <- data %>%
    pull(index) %>%
    max()
  
  result<-list()
  
  result$data<-data #xaxis - index, yaxis - sum_probability
  result$avg<-avg_data   #horisontal line
  result$trends<-trends_trend
  
  # 
  # q <- ggplot(data, aes(x=index, y=sum_probability)) +
  #   geom_step() +
  #   # geom_line()+
  #   geom_hline(data=avg_data, aes(yintercept=mean), lty=2, color="red") +
  #   geom_rect(data=trends_trend,
  #             inherit.aes = FALSE,
  #             aes(xmin=index, xmax=index+1, ymin=0, ymax=0.3, fill=trend),
  #             alpha=0.3) +
  #   facet_grid(topic~., scales = "free_y")
  # 
  # q
  # 
}
