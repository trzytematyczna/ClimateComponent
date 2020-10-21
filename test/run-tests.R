## setwd ("..")
source ("./src/R/climate-data.R")
source ("./src/R/topic-analyzer.R")
source ("./src/R/event-analyzer.R")

print ("GET topics")
top <- topics ()
print (top)

print ("GET similarity")
sim <- similarity (top$topics, grouping_threshold = 0.90)
print (sim)

## pdf ("test.pdf")
similarity_plot (sim$matrix, sim$groups, edge_threshold = 0.90)
## dev.off()

print ("GET specificity")
top <- topics (topic = c ("G1", "T2", "G4", "P8"))
spe <- specificity (top$topics)
print (spe)
specificity_plot (spe$topics, spe$words, word_threshold = 0.90)
