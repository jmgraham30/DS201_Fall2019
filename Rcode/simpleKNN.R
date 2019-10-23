library(tidyverse)
library(broom)
library(caret)

train_ids <- createDataPartition(iris$Species,p=0.75,list = F)
iris_train <- iris[train_ids, ]
iris_test <- iris[-train_ids, ]


tC <- trainControl(method="repeatedcv",number = 10,repeats = 5)
tG <- expand.grid(k=seq(1,25,by=2))

train_result <- train(Species~.,method="knn",data=iris_train,trControl=tC,tuneGrid=tG)

train_result$results

plot(train_result)

best_preds <- predict(train_result$finalModel,newdata = iris_test[ ,1:4])


