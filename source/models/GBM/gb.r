library(dplyr)
library(gbm)
library(caTools)
library(pROC)
library(doParallel)
library(caret)
#library(DMwR)
#library(ROSE)
library(MLmetrics)

# Read the data
shotDataRaw <- read.csv('../data/shot_longs_clean_noNA_secondsclock.csv', header = TRUE, na.strings = c('NA','','#NA'))

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


#time series split
# myTimeControl <- trainControl(method = "timeslice",
#                               initialWindow = 674520,
#                               horizon = 674520,
#                               fixedWindow =  TRUE,
#                               #allowParallel = TRUE,
#                               verboseIter = TRUE#,
#                               #sampling = "smote"
#                               )

#create time slices
# timeSlices <- createTimeSlices(1:nrow(data),
#                                initialWindow = 36, horizon = 12, fixedWindow = TRUE)
# trainSlices <- timeSlices[[1]]
# testSlices <- timeSlices[[2]]

#separate the data into train and test
#because we run into memory issues 
#train_data will be the data from 2017-01-
#test_data will be from 2019-01-26 till 2020-01-26
trainData_older = data[data$timestamp < '2017-01-27', ]
trainData = data[data$timestamp > '2017-01-27' & data$timestamp < '2019-01-27', ]
testData = data[data$timestamp > '2019-01-26', ]

#splitting by timestamp
# splitIdx = createDataPartition(data$did_crash_happen, p=0.7, list = FALSE)  # 70% training data, 30% testing
# trainData = data[splitIdx, ]
# testData = data[-splitIdx, ]

#we will need to upsample the trainData
set.seed(123)
columns = colnames(trainData)
trainData_upsampled = upSample(
  x = trainData[, columns[columns != "did_crash_happen"] ], 
  y = trainData$did_crash_happen, list = F, yname = "did_crash_happen"
)
print(table(trainData_upsampled$did_crash_happen))

set.seed(456)
#try downsampling instead...
trainData_downsampled = downSample(
  x = trainData[, columns[columns != "did_crash_happen"] ], 
  y = trainData$did_crash_happen, list = F, yname = "did_crash_happen"
)
print(table(trainData_downsampled$did_crash_happen))


gbm.trainControl = trainControl(
  method = "cv", 
  number = 3, # it takes forever for 10 - fold 
  # Estimate class probabilities
  classProbs = TRUE,
  # Evaluate performance using the following function
  summaryFunction = twoClassSummary,
  allowParallel = TRUE,
  verbose = TRUE
)

#make sure we get rid as much useless data objects in R environment as possible
gc()
rm(motor_collision_crash_clean_data, data, trainData, trainData_older)
rm(trainData_upsampled)
#rm(trainData_downsampled)
gc()
memory.limit()
memory.limit(size=30000)

#tuneGrid
gbmGrid <- expand.grid(
  #interaction.depth = c(10, 20),
  #n.trees = c(50, 100, 250),
  interaction.depth = c(20),
  n.trees = c(200),
  n.minobsinnode = 10,
  shrinkage = .1
)

#train model
set.seed(789)
ptm_rf <- proc.time()
model_gbm <- train(
  did_crash_happen ~ . - timestamp,
  #data = data[trainSlices[[1]],],
  data = trainData_downsampled,
  #data = train_data,
  method = "gbm",
  #family="gaussian",
  #distribution = "gaussian",
  trControl = gbm.trainControl,
  #tuneLength = 5
  tuneGrid = gbmGrid
)
proc.time() - ptm_rf

#when we are done with parallel processing needs
#stopCluster(cl)

#make predictions aginst testData with the new model 
print(model_gbm)
pred.model_gbm.prob = predict(model_gbm, newdata = testData, type="prob")
pred.model_gbm.raw = predict(model_gbm, newdata = testData)


roc.model_gbm = pROC::roc(
  testData$did_crash_happen, 
  as.vector(ifelse(pred.model_gbm.prob[,"yes"] >0.5, 1,0))
)
auc.model_gbm = pROC::auc(roc.model_gbm)
print(auc.model_gbm)

#plot ROC curve
plot.roc(roc.model_gbm, print.auc = TRUE, col = 'red' , print.thres = "best" )

#generate confusion matrix, as well as other metrics such as accuracy, balanced accuracy
confusionMatrix(data = pred.model_gbm.raw, testData$did_crash_happen)

#summary of model 
summary(model_gbm)

#see the different metrics and roc curve this model scored against trainData_downsampled
pred.model_gbm.train.prob = predict(model_gbm, newdata = trainData_downsampled, type="prob")
pred.model_gbm.train.raw = predict(model_gbm, newdata = trainData_downsampled)

roc.model_gbm.train = pROC::roc(
  trainData_downsampled$did_crash_happen, 
  as.vector(ifelse(pred.model_gbm.train.prob[,"yes"] >0.5, 1,0))
)
auc.model_gbm.train = pROC::auc(roc.model_gbm.train)
print(auc.model_gbm.train)

#plot ROC curve
plot.roc(roc.model_gbm.train, print.auc = TRUE, col = 'blue' , print.thres = "best" )

#generate confusion matrix, as well as other metrics such as accuracy, balanced accuracy
confusionMatrix(data = pred.model_gbm.train.raw, trainData_downsampled$did_crash_happen)

# Save the model into a file
save(model_gbm, file="caret_gbm.rda")