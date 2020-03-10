# Load libraries
library(tidyverse)
library(mclust)
library(chron)
library(lubridate)

# Read the data
initialData <- as_tibble(read.csv('../data/shot_logs.csv', header = TRUE, na.strings = c('NA','','#NA'), stringsAsFactors = FALSE))

# Explore the data a bit
summary(initialData)
head(initialData)
tail(initialData)

# Find NA shot clock data that was NA because the game clock was < 25 seconds left.
# Assumption is that shot clock is equal to game clock in this case. Possible that it just wasn't recorded.
cleanData <- initialData
gameClock <- as.vector(second(fast_strptime(initialData$GAME_CLOCK, "%M:%S")))
shotClock <- is.na(initialData$SHOT_CLOCK)
for(i in 1:length(gameClock)){
  if(shotClock[i] & gameClock[i] < 25){
    cleanData$SHOT_CLOCK[i] <- gameClock[i]
  }
}

# Place NA leftovers in a new dataframe for examination/backup.
weirdShotClock <- subset(cleanData, is.na(SHOT_CLOCK))

# Remove all NA's from cleanData
cleanNoNAData <- subset(cleanData, !is.na(SHOT_CLOCK))

# Custom function to capitalize first letter of each word in a string. 
# Currently not used.
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Custom function for name format reverse from "firstname lastname" to "lastname, firstname"
nameformatreverse <- function(s) {
  fname <- str_extract(s, "^\\w+")
  lname <- str_extract(s, "\\w+$")
  s <- paste(lname, fname, sep = ", ")
}

# Clean up shooter names to all capitals and "lastname, firstname" format both to ensure uniformity.
shooterName <- cleanNoNAData$player_name
shooterName <- toupper(shooterName)
shooterName <- nameformatreverse(shooterName)

# Clean up defender names to all capitals and no "." both to ensure uniformity.
cleanNoNAData$player_name <- shooterName
cleanNoNAData$CLOSEST_DEFENDER <- toupper(cleanNoNAData$CLOSEST_DEFENDER)
cleanNoNAData$CLOSEST_DEFENDER <- gsub("[.]", "", cleanNoNAData$CLOSEST_DEFENDER)

# Seconds for game clock
cleanNoNASecondsClockData$GAME_CLOCK <- as.vector(second(fast_strptime(cleanNoNAData$GAME_CLOCK, "%M:%S"))) + 
  as.vector(minute(fast_strptime(cleanNoNAData$GAME_CLOCK, "%M:%S"))) * 60

#write.csv(cleanData, "../data/shot_logs_clean.csv")
#write.csv(cleanNoNAData, "../data/shot_logs_clean_noNA.csv")
write.csv(cleanNoNASecondsClockData, "../data/shot_longs_clean_noNA_secondsclock.csv")

ggplot(cleanNoNASecondsClockData, mapping = aes(SHOT_CLOCK, stat(count))) + geom_bar()
ggplot(cleanNoNASecondsClockData, mapping = aes(SHOT_CLOCK, stat(count))) + geom_histogram()
ggplot(cleanNoNASecondsClockData, mapping = aes(GAME_CLOCK, stat(count))) + geom_bar()
ggplot(cleanNoNASecondsClockData, mapping = aes(GAME_CLOCK, stat(count))) + geom_histogram()



# # We can now remove any records that have NAs
# myDataClean <- na.omit(initialData)
# summary(myDataClean)
# xOnlyData <- myDataClean[, -1]
# 
# # Let us apply kmeans for k=3 clusters 
# kmm = kmeans(xOnlyData, 6, nstart = 50, iter.max = 15) 
# # We keep number of iter.max=15 to ensure the algorithm converges and nstart=50 to 
# # Ensure that atleat 50 random sets are choosen  
# kmm
# 
# # Elbow Method for finding the optimal number of clusters
# set.seed(123)
# # Compute and plot wss for k = 2 to k = 15.
# k.max <- 30
# wss <- sapply(1:k.max, function(k){kmeans(xOnlyData, k, nstart=50,iter.max = 15 )$tot.withinss})
# wss
# plot(1:k.max, wss,
#      type="b", pch = 19, frame = FALSE, 
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")
# 
# # Bayesian Inference Criterion for k means to validate choice from Elbow Method
# d_clust <- Mclust(as.matrix(xOnlyData), G=1:30, 
#                   modelNames = mclust.options("emModelNames"))
# d_clust$BIC
# plot(d_clust)
# 
# # 30 indices to find the best one
# library(NbClust)
# nb <- NbClust(xOnlyData, diss=NULL, distance = "euclidean", 
#               min.nc=5, max.nc=23, method = "kmeans", 
#               index = "all", alphaBeale = 0.1)
# hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

#subetting ----
# set.seed(123) # set the seed to make the partition reproducible
# 
# # 80% of the sample size
# smp_size <- floor(0.0025 * nrow(studentInfo))
# 
# train_ind <- sample(seq_len(nrow(studentInfo)), size = smp_size)
# 
# # creating test and training sets that contain all of the predictors
# studentInfo_test <- studentInfo[train_ind, ]
# studentInfo_train <- studentInfo[-train_ind, ]
# 
# needed<-which(studentAssessment$id_student %in% studentInfo_test$id_student)    
# studentAssessment_test<-studentAssessment[needed,]
# 
# 
# needed<-which(studentRegistration$id_student %in% studentInfo_test$id_student)    
# studentRegistration_test<-studentRegistration[needed,]
# 
# needed<-which(studentVle$id_student %in% studentInfo_test$id_student)    
# studentVle_test<-studentVle[needed,]