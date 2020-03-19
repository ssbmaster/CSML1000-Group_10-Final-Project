library(dplyr)
library(caTools)
library(pROC)
library(caret)
library(e1071)

# Read the data
shotDataRaw <- read.csv('../../data/shot_longs_clean_noNA_secondsclock.csv', header = TRUE, na.strings = c('NA','','#NA'))

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

#scaling not required for glm (logistic regression)
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

#glm is a bit weird, doesn't accept 1 or 0, so we will convert FGM into "yes" or "no"
shotData$FGM <- as.factor(
  ifelse(shotData$FGM == 0, "no", "yes")
)


#no need to under/oversample, because FGM of 0 is close to 55%, and FGM of 1 is close to 45%
#so pretty balanced dataset!

#split the data into training and testing datasets
set.seed(123)
shotSample = sample.split(shotData$FGM, SplitRatio = 0.70)
shotTrain = subset(shotData, shotSample == TRUE)
shotTest = subset(shotData, shotSample == FALSE)


#set a train control
#use cross validation
glm.trainControl = trainControl(
  method = "cv",
  number = 5,
  #Estimate class probabilities
  classProbs = TRUE,
  #Evaluate performance using the following function
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  verbose = TRUE
)

#no need to tuneGrid because logistic regression using glm has no parameters
#train model
set.seed(123)
ptm_rf <- proc.time()
model_glm <- train(
  FGM ~ ., 
  data = shotTrain,
  method = 'glm',
  trControl = glm.trainControl
)
proc.time() - ptm_rf

#make prediction against testData with the new model
print(model_glm)
pred.model_glm.prob = predict(model_glm, newdata = shotTest, type="prob")
pred.model_glm.raw = predict(model_glm, newdata = shotTest)

roc.model_glm = pROC::roc(
  shotTest$FGM,
  as.vector(ifelse(pred.model_glm.prob[,"yes"] > 0.5, 1, 0))
)
auc.model_glm = pROC::auc(roc.model_glm)
print(auc.model_glm)

#plot ROC curve
plot.roc(roc.model_glm, print.auc = TRUE, col = 'red', print.thres = "best")

#generate confusion matrix, as well as other metrics such as accuracy, balanced accuracy
confusionMatrix(data = pred.model_glm.raw, shotTest$FGM)

#summary of model
summary(model_glm)

# Save the model into a file
#don't do this unless if you are ready, this thing is 3gb big for some fucking reason
save(model_glm, file="model_glm.rda")
