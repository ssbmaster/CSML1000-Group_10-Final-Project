assessments<-read.csv("../data/assessments.csv",na.strings="NA")
courses<-read.csv("../data/courses.csv",na.strings="NA")
vle<-read.csv("../data/vle.csv",na.strings="NA")

studentAssessment<-read.csv("../data/studentAssessment.csv",na.strings="NA")
studentInfo<-read.csv("../data/studentInfo.csv",na.strings="NA")
studentRegistration<-read.csv("../data/studentRegistration.csv",na.strings="NA")
studentVle<-read.csv("../data/studentVle.csv",na.strings="NA")

library(tidyr)
library(dplyr)


#assessmentsClean <- unite(assessments, module_presentation, c("code_module", "code_presentation"), sep="+", remove=TRUE)
#coursesClean <- unite(courses, module_presentation, c("code_module", "code_presentation"), sep="+", remove=TRUE)
#VLEClean <- unite(vle, module_presentation, c("code_module", "code_presentation"), sep="+", remove=TRUE)

studentVle <- rename(studentVle, "interaction_date"="date")

assessments <- rename(assessments, "due_date"="date")

#subetting ----
set.seed(123) # set the seed to make the partition reproducible

# 80% of the sample size
smp_size <- floor(0.0025 * nrow(studentInfo))

train_ind <- sample(seq_len(nrow(studentInfo)), size = smp_size)

# creating test and training sets that contain all of the predictors
studentInfo_test <- studentInfo[train_ind, ]
studentInfo_train <- studentInfo[-train_ind, ]

needed<-which(studentAssessment$id_student %in% studentInfo_test$id_student)    
studentAssessment_test<-studentAssessment[needed,]


needed<-which(studentRegistration$id_student %in% studentInfo_test$id_student)    
studentRegistration_test<-studentRegistration[needed,]

needed<-which(studentVle$id_student %in% studentInfo_test$id_student)    
studentVle_test<-studentVle[needed,]



#write.csv(studentInfo_test, "selectedStudents.csv")

# merging ----

merged1 <- merge(assessments, courses, by=c("code_module", "code_presentation"), all=TRUE)

merged2 <- merge(merged1, vle, by=c("code_module", "code_presentation"), all=TRUE)
#rm(merged1)
merged3 <- merge(merged2, studentInfo_test, by=c("code_module", "code_presentation"), all=TRUE)
#rm(merged2)
merged4 <- merge(merged3, studentRegistration_test, by=c("id_student"), all=TRUE)
#rm(merged3)
merged5 <- merge(merged3, studentAssessment_test, by=c("id_student"), all=TRUE)
#rm(merged4)
merged6 <- merge(merged5, studentVle_test, by=c("id_student"), all=TRUE)
#rm(merged5)
