## setwd ("..")
source ("./src/R/climate-data.R")
source ("./src/R/topic-analyzer.R")

print ("GET topics-probs")
probs <- topics_probs (dataname = "guardian", k = 10)
print (probs)

print ("GET similarity")
sim <- similarity (probs$topic_word_prob)
print (sim)

