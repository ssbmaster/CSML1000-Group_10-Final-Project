uniqueChoices <- read.csv("./uniqueChoices.csv", header = TRUE, na.strings = c('NA','','#NA'), stringsAsFactors = FALSE)



save(uniqueChoices, file="uniqueChoices.RData")

