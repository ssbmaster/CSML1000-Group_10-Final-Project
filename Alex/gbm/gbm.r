library(dplyr)
library(gbm)
library(caTools)
library(pROC)
library(doParallel)
library(caret)
library(MLmetrics)

# Read the data
shotDataRaw <- read.csv('../../data/shot_logs_clean_noNA_secondsclock.csv', header = TRUE, na.strings = c('NA','','#NA'))

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

#scaling not required for gbm
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

#gbm is a bit weird, doesn't accept 1 or 0, so we will convert FGM into "yes" or "no"
shotData$FGM <- as.factor(
  ifelse(shotData$FGM == 0, "no", "yes")
)


#split the data into training and testing datasets
set.seed(123)
shotSample = sample.split(shotData$FGM, SplitRatio = 0.70)
shotTrain = subset(shotData, shotSample == TRUE)
shotTest = subset(shotData, shotSample == FALSE)

#set trainControl
#5-fold cross validation
gbm.trainControl = trainControl(
  method = "cv", 
  number = 5,
  # Estimate class probabilities
  classProbs = TRUE,
  # Evaluate performance using the following function
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  verbose = TRUE
)

#tuneGrid for GBM
gbmGrid <- expand.grid(
  #interaction.depth = c(10, 20),
  #n.trees = c(50, 100, 250),
  interaction.depth = c(5),
  n.trees = c(40),
  n.minobsinnode = 10,
  shrinkage = .1
)

#train model
set.seed(123)
ptm_rf <- proc.time()
model_gbm <- train(
  FGM ~ .,
  #data = data[trainSlices[[1]],],
  data = shotTrain,
  #data = train_data,
  method = "gbm",
  #family="gaussian",
  #distribution = "gaussian",
  trControl = gbm.trainControl,
  #tuneLength = 5
  tuneGrid = gbmGrid
)
proc.time() - ptm_rf

#make predictions aginst testData with the new model 
print(model_gbm)
pred.model_gbm.prob = predict(model_gbm, newdata = shotTest, type="prob")
pred.model_gbm.raw = predict(model_gbm, newdata = shotTest)


roc.model_gbm = pROC::roc(
  shotTest$FGM, 
  as.vector(ifelse(pred.model_gbm.prob[,"yes"] >0.5, 1,0))
)
auc.model_gbm = pROC::auc(roc.model_gbm)
print(auc.model_gbm)

#plot ROC curve
plot.roc(roc.model_gbm, print.auc = TRUE, col = 'red' , print.thres = "best" )

#generate confusion matrix, as well as other metrics such as accuracy, balanced accuracy
confusionMatrix(data = pred.model_gbm.raw, shotTest$FGM)

#summary of model 
summary(model_gbm)

# Save the model into a file
save(model_gbm, file="gbm.rda")
