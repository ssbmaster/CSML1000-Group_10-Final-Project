# Load libraries
library(caTools)
library(rpart)
library(rpart.plot)
library(pROC)

# Read the data
shotDataRaw <- read.csv('../data/shot_logs_clean_noNA_secondsclock.csv', header = TRUE, na.strings = c('NA','','#NA'))

#columns to keep
shotData <- shotDataRaw[
  c(
    'LOCATION', 
    'PERIOD', 
    'GAME_CLOCK', 
    'SHOT_CLOCK', 
    'DRIBBLES', 
    'TOUCH_TIME', 
    'SHOT_DIST', 
    'PTS_TYPE', 
    'CLOSEST_DEFENDER', 
    #'CLOSEST_DEFENDER_PLAYER_ID',
    'CLOSE_DEF_DIST',
    'player_name',
    'FGM'
  )
]

#scaling not required for decision tree
#kdataunscaled <- shotData[, c("PERIOD", "GAME_CLOCK", "SHOT_CLOCK", "DRIBBLES", "TOUCH_TIME", "SHOT_DIST", "CLOSE_DEF_DIST")]
#kdata <- scale(kdataunscaled)

#make sure columns are set as factor, ordered, or numerical
shotData$LOCATION <- as.factor(shotData$LOCATION)
shotData$PERIOD <- as.factor(shotData$PERIOD)
shotData$GAME_CLOCK <- as.numeric(shotData$GAME_CLOCK)
shotData$SHOT_CLOCK <- as.numeric(shotData$SHOT_CLOCK)
shotData$DRIBBLES <- as.numeric(shotData$DRIBBLES)
shotData$TOUCH_TIME <- as.numeric(shotData$TOUCH_TIME)
shotData$SHOT_DIST <- as.numeric(shotData$SHOT_DIST)
shotData$PTS_TYPE <- as.factor(shotData$PTS_TYPE)
shotData$CLOSEST_DEFENDER <- as.factor(shotData$CLOSEST_DEFENDER)
shotData$CLOSE_DEF_DIST <- as.numeric(shotData$CLOSE_DEF_DIST)
shotData$player_name <- as.factor(shotData$player_name)
shotData$FGM <- as.factor(shotData$FGM)


#no need to under/oversample, because FGM of 0 is close to 55%, and FGM of 1 is close to 45%
#so pretty balanced dataset!

#split the data into training and testing datasets
set.seed(123)
shotSample = sample.split(shotData$FGM, SplitRatio = 0.70)
shotTrain = subset(shotData, shotSample == TRUE)
shotTest = subset(shotData, shotSample == FALSE)

#build a decision tree model
fit <- rpart(FGM ~ ., shotTrain, method="anova")
fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
prune(fit, cp=0.01)
predictedValues <- predict(fit, shotTest)

#for predicting individual shots with the data that we specify
individualShot <- shotTest[1,]
individualShot[,]=NA
individualShot$LOCATION <- as.factor(individualShot$LOCATION)
individualShot$PERIOD <- as.factor(individualShot$PERIOD)
individualShot$GAME_CLOCK <- as.numeric(individualShot$GAME_CLOCK)
individualShot$SHOT_CLOCK <- as.numeric(individualShot$SHOT_CLOCK)
individualShot$DRIBBLES <- as.numeric(individualShot$DRIBBLES)
individualShot$TOUCH_TIME <- as.numeric(individualShot$TOUCH_TIME)
individualShot$SHOT_DIST <- as.numeric(individualShot$SHOT_DIST)
individualShot$PTS_TYPE <- as.factor(individualShot$PTS_TYPE)
individualShot$CLOSEST_DEFENDER <- as.factor(individualShot$CLOSEST_DEFENDER)
individualShot$CLOSE_DEF_DIST <- as.numeric(individualShot$CLOSE_DEF_DIST)
individualShot$player_name <- as.factor(individualShot$player_name)
individualShot$FGM <- as.factor(individualShot$FGM)
save(individualShot, file="individualShot.RData")
individualShot$SHOT_DIST <- 0
individualShot$CLOSE_DEF_DIST <- 5
individualPredict <- predict(fit, individualShot, na.action=na.pass)
print(individualPredict)

#define function
splitfun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width = 40), collapse = "\n")
  }
  labs
}

# Generate the plots
rpart.plot(fit, main="Regression Tree", box.palette = "Blues", under=TRUE, roundint = FALSE, clip.right.labs = FALSE, type=3, split.fun=splitfun)
rpart.rules(fit, cover=TRUE, roundint = FALSE)
auc.rp = roc(shotTest$FGM, factor(predictedValues, ordered = TRUE), plot = TRUE, col = "blue")
#post(fit, file = "tree.ps", title = "Pruned Regression Tree for Crashes")
auc(shotTest$FGM, predictedValues)
auc.rp

# Save the model into a file
save(fit, file="fittedRegTreeModel.rda")
