rm(list = ls())

h1b_transformed_na <- readRDS("/Users/jaydangi/Downloads/h1bAnalytics-master/h1b_transformed_without_na.rds")


# Dividing h1b_transformed_na into train and test data
train_na = h1b_transformed_na[1:1299875,]
test_na = h1b_transformed_na[1299876:2599750,]

# Replacing the CASE_STATUS of Test data with Only "Certified" values in h1b_na data
test.case.na <- data.frame(CASE_STATUS=rep("None", nrow(test_na)), test_na[,])
test.case.na$CASE_STATUS.1 <- NULL

# Combine test and train datasets
data.combined.na <- rbind(train_na,test.case.na)

# Converting Employer name, Job Title into character
data.combined.na$JOB_TITLE <- as.character(data.combined.na$JOB_TITLE)
data.combined.na$EMPLOYER_NAME <- as.character(data.combined.na$EMPLOYER_NAME)

# Removing extra levels from CASE_STATUS
data.combined.na$CASE_STATUS <- factor(data.combined.na$CASE_STATUS)

#converting employer name, job title into numeric data
data.combined.na$JOB_TITLE <- as.numeric(data.combined.na$JOB_TITLE)
data.combined.na$EMPLOYER_NAME <- as.factor(data.combined.na$EMPLOYER_NAME)


#select the first 1000 case statuses
select <- train_na[1:98000,]

# Implementing randomForest on the data
#install.packages("randomForest")
library(randomForest)
library(caret)
# Random forest training 1
rf.train.1 <- data.combined.na[1:98000 ,c("FULL_TIME_POSITION","PREVAILING_WAGE")]
rf.label <- as.factor(select$CASE_STATUS)
rf.label <- factor(rf.label)

set.seed(1234)
rf.1 <- randomForest(x= rf.train.1,y=rf.label, importance=TRUE,ntree = 5)
rf.1
varImpPlot(rf.1)
plot(rf.1)



control<- trainControl(method='repeatedcv', repeats=1, verboseIter=TRUE, classProbs = TRUE)
mod<- train(x=rf.train.1,y=rf.label, method= 'rf', trControl =control)
plot(mod)

predictrffit <- predict(mod, test_na)
plot(predictrffit)
confusionMatrix(predictrffit, test_na$CASE_STATUS)

