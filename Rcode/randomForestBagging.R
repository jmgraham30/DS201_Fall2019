library(tidyverse)
library(caret)
library(randomForest)
library(MASS)

train_ids <- createDataPartition(iris$Species,p=0.75,list = F)
iris_train <- iris[train_ids, ]
iris_test <- iris[-train_ids, ]

rf_mod <- randomForest(Species ~., data=iris_train,mtry=4)

rf_mod$confusion

rf_mod$importance

train_results <- data.frame(pred=rf_mod$predicted,obs=rf_mod$y)

defaultSummary(train_results)

mod_preds <- predict(rf_mod,iris_test)

confusionMatrix(mod_preds,iris_test$Species)$table

(train_acc <- 1 - 6/(35 + 35 + 38 + 3 + 3))
(test_acc <- 1 - 2/(12 + 12 + 10 + 2))

train_ids2 <- createDataPartition(Boston$medv,p=0.8,list=F)
boston_train <- Boston[train_ids2, ]
boston_test <- Boston[-train_ids2, ]

boston_rfmod <- randomForest(medv ~., data=boston_train,mtry=13)

(test_rmse_est <- RMSE(boston_rfmod$predicted,boston_train$medv))

pred_vals <- predict(boston_rfmod,boston_test) 

(test_rmse <- RMSE(pred_vals,boston_test$medv))






