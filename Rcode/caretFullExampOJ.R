library(tidyverse)
library(caret)
library(skimr)
library(RANN)
library(rpart)
library(rpart.plot)

oj_df <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/orange_juice_withmissing.csv')

head(oj_df)

str(oj_df)

train_ids <- createDataPartition(oj_df$Purchase,p=0.75,list = F)

oj_train <- oj_df[train_ids, ]
oj_test <- oj_df[-train_ids, ]

X <- oj_train %>% select(-Purchase)
Y <- oj_train$Purchase

skimmed <- skim_to_wide(oj_train)
skimmed[, c(1:5, 9:11, 13, 15:16)]

preProcess_missingdata_model <- preProcess(oj_train, method='knnImpute')
preProcess_missingdata_model

oj_train <- predict(preProcess_missingdata_model, newdata = oj_train)
anyNA(oj_train)

dummies_model <- dummyVars(Purchase ~ ., data=oj_train)

trainData_mat <- predict(dummies_model, newdata = oj_train)

oj_train <- as.data.frame(trainData_mat)

preProcess_range_model <- preProcess(oj_train, method='range')
oj_train <- predict(preProcess_range_model, newdata = oj_train)

oj_train$Purchase <- Y

head(oj_train)

featurePlot(x = oj_train[, 1:18], 
            y = oj_train$Purchase, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

featurePlot(x = oj_train[, 1:18], 
            y = oj_train$Purchase, 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))


options(warn=-1)

subsets <- c(1:5, 10, 15, 18)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=oj_train[, 1:18], y=oj_train$Purchase,
                 sizes = subsets,
                 rfeControl = ctrl)


lmProfile

modelLookup(model="rpart")

tC <- trainControl(method = "repeatedcv",number = 10,repeats = 5)

tP <- expand.grid(cp=seq(10,0,by=-0.05))

oj_model <- train(Purchase~.,data=oj_train,trControl=tC,tuneGrid=tP,method="rpart")

plot(oj_model)

oj_model

pred_vals <- predict(oj_model)

confusionMatrix(pred_vals,oj_train$Purchase)

rpart.plot(oj_model$finalModel,type=5)

oj_df %>% select(LoyalCH,Purchase) %>% head()

varimp_rpart <- varImp(oj_model)
plot(varimp_rpart, main="Variable Importance with rpart")

testData2 <- predict(preProcess_missingdata_model, oj_test)  

testData3 <- predict(dummies_model, testData2)

testData4 <- predict(preProcess_range_model, testData3)

head(testData4)

test_pred <- predict(oj_model, testData4)

confusionMatrix(test_pred,oj_test$Purchase)










