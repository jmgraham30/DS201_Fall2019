library(tidyverse)
library(ISLR)
library(broom)
library(caret)
library(ROCR)

train_ids <- createDataPartition(Default_to_fit$default_num,p=0.75,list=F)

Default_train <- Default_to_fit[train_ids, ]

Default_test <- Default_to_fit[-train_ids, ]

default_glm_mod = train(
  form = default ~ balance,
  data = Default_train,
  metric = "ROC",
  trControl = trainControl(method = "repeatedcv", number = 10,repeats = 5,summaryFunction=twoClassSummary,,classProbs=TRUE),
  method = "glm",
  family = "binomial")

default_glm_mod$results



