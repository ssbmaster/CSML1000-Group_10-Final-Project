shotData <- as_tibble(read.csv('./testData.csv', header = TRUE, na.strings = c('NA','','#NA'), stringsAsFactors = FALSE))
save(shotData, file = "shotData.RData")
