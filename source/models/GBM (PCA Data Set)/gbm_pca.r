library(dplyr)
library(caTools)
library(pROC)
library(caret)
library(e1071)

# Read the data
shotDataRaw <- read.csv('../../data/shot_logs_pca.csv', header = TRUE, na.strings = c('NA','','#NA'))
shotData <- shotDataRaw

#scaling not required for gbm
#kdataunscaled <- shotData[, c("PERIOD", "GAME_CLOCK", "SHOT_CLOCK", "DRIBBLES", "TOUCH_TIME", "SHOT_DIST", "CLOSE_DEF_DIST")]
#kdata <- scale(kdataunscaled)

#make sure columns are set as factor, ordered, or numerical
shotData$PC1 <- as.numeric(shotData$PC1)
shotData$PC2 <- as.numeric(shotData$PC2)
shotData$PC3 <- as.numeric(shotData$PC3)
shotData$PC4 <- as.numeric(shotData$PC4)
shotData$PC5 <- as.numeric(shotData$PC5)
shotData$PC6 <- as.numeric(shotData$PC6)
shotData$PC7 <- as.numeric(shotData$PC7)
shotData$PC8 <- as.numeric(shotData$PC8)

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
  interaction.depth = c(1, 2, 5, 10),
  n.trees = c(25, 50, 100, 150, 200),
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
save(model_gbm, file="gbm_pca.rda")