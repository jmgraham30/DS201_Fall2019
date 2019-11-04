library(tidyverse)
library(ISLR)
library(rpart)
library(rpart.plot)

df <- Hitters[complete.cases(Hitters), ]

df$Log_salaray <- log(df$Salary)

tC <- trainControl(method = "cv",number=10)

train_mod <- train(Log_salaray ~ Years + Hits,data=df,trControl=tC,method="rpart")

train_mod$results

rpart.plot(train_mod$finalModel)

pred_vals <- predict(train_mod,df)

RMSE(pred_vals,df$Log_salaray)


train_ids <- createDataPartition(iris$Species,p=0.75,list = F)
iris_train <- iris[train_ids, ]
iris_test <- iris[-train_ids, ]


tC <- trainControl(method="repeatedcv",number = 10,repeats = 5)

train_result <- train(Species~.,data=iris_train,trControl=tC,method="rpart")

train_result$results

rpart.plot(train_result$finalModel)

pred_class <- predict(train_result,iris_test)

test_results <- data.frame(pred=pred_class,obs=iris_test$Species)

defaultSummary(test_results)

confusionMatrix(test_results$pred,test_results$obs)$table

