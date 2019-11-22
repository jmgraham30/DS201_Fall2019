library(tidyverse)
library(nnet)
library(NeuralNetTools)
library(neuralnet)
library(caret)
library(e1071)

set.seed(1234)

x <- seq(-2,2,length.out = 201)
y <- 2*x -3 + rnorm(length(x),sd=0.5)
df <- data.frame(x=x,y=y)

train_ids <- createDataPartition(y,p=0.75,list = F)
df_train <- df[train_ids, ]
df_test <- df[-train_ids, ]

preProcValues <- preProcess(df_train, method = c("range"))
df_train_scaled <- predict(preProcValues,df_train)
df_test_scaled <- predict(preProcValues,df_test)

df_train_scaled %>% ggplot(aes(x=x,y=y)) + geom_point()

trained_nn <- neuralnet(y~x,data=df_train_scaled,hidden=10)

nn_pred <- predict(trained_nn,newdata = df_test_scaled)

test_results <- df_test_scaled
test_results$preds <- nn_pred 

ggplot() + geom_point(data=test_results,aes(x=x,y=y),color="red") + 
  geom_point(data=test_results,aes(x=x,y=preds),color="blue")

plot(trained_nn)

tC <- trainControl(method="repeatedcv",number=10,repeats=5)
tG <- expand.grid(size=c(5,10,15,20,25),decay=c(0.01,0.05,0.1,0.5,1,1.5,5))

train_nn <- train(y~x,data=df_train_scaled,trControl=tC,tuneGrid=tG,method="nnet")

train_nn$results

plot(train_nn)

caret_nn_pred <- predict(train_nn,df_test_scaled)

test_results <- df_test_scaled
test_results$preds <- caret_nn_pred 

test_results %>% ggplot() + geom_point(aes(x=x,y=y),color="red") + 
  geom_point(aes(x=x,y=preds),color="blue")

RMSE(predict(train_nn$finalModel,newdata = df_test_scaled),df_test_scaled$y)

plotnet(train_nn$finalModel)

#####
tC <- trainControl(method="repeatedcv",number=10,repeats = 5)
tG <- expand.grid(layer1=c(5,10,15,20,25),layer2=c(5,10,15,20,25),layer3=c(5,10,15,20,25))

#train_nn <- train(y~x,data=df_train_scaled,trControl=tC,tuneGrid=tG,method="neuralnet")

saveRDS(train_nn,"./Rcode/train_neuralnet.rds")
train_nn <- readRDS("./Rcode/train_neuralnet.rds")

train_nn$results

plot(train_nn)

caret_nn_pred <- predict(train_nn,df_test_scaled)

test_results <- df_test_scaled
test_results$preds <- caret_nn_pred 

test_results %>% ggplot() + geom_point(aes(x=x,y=y),color="red") + 
  geom_point(aes(x=x,y=preds),color="blue")

RMSE(predict(train_nn$finalModel,newdata = df_test_scaled),df_test_scaled$y)

#plot(train_nn$finalModel)

#### Neural network for classification
train_iris_ids <- createDataPartition(iris$Species,p=0.75,list=F)
iris_train <- iris[train_iris_ids, ]
iris_test <- iris[-train_iris_ids, ]

preProcValues <- preProcess(iris_train, method = c("range"))
df_train_scaled <- predict(preProcValues,iris_train)
df_test_scaled <- predict(preProcValues,iris_test)

iris_nn_trained <- nnet(Species~.,data=df_train_scaled,
                        size=15,rang=0.1,decay=0.005,maxit=5000)

pred_classes <- predict(iris_nn_trained,newdata = df_test_scaled,type = "class")

table(pred_classes,df_test_scaled$Species)

iris_test$pred <- pred_classes

iris_test %>% ggplot(aes(x=Petal.Length,y=Petal.Width,color=Species,shape=pred)) + 
  geom_point()

plotnet(iris_nn_trained)

#garson(iris_nn_trained)


