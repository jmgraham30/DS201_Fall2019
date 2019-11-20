library(tidyverse)
library(nnet)
library(caret)
library(e1071)

x <- seq(-2,2,by=0.05)
y <- 2*x -3 + rnorm(length(x),sd=0.5)
df <- data.frame(x=x,y=y)

train_ids <- createDataPartition(y,p=0.75,list = F)
df_train <- df[train_ids, ]
df_test <- df[-train_ids, ]

preProcValues <- preProcess(df_train, method = c("range"))
df_train_scaled <- predict(preProcValues,df_train)
df_test_scaled <- predict(preProcValues,df_train)

df_train_scaled %>% ggplot(aes(x=x,y=y)) + geom_point()

trained_nn <- neuralnet(y~x,data=df_train_scaled,hidden=10)

nn_pred <- predict(trained_nn,newdata = df_test_scaled)

test_results <- df_test_scaled
test_results$preds <- nn_pred 

ggplot() + geom_point(data=test_results,aes(x=x,y=y),color="red") + 
  geom_point(data=test_results,aes(x=x,y=preds),color="blue")

plot(trained_nn)

tC <- trainControl(method="cv")
tG <- expand.grid(size=c(1,3,5,15),decay=c(0.05,0.5,1))

train_nn <- train(y~x,data=df_train_scaled,trControl=tC,tuneGrid=tG,method="nnet")

train_nn$results

plot(train_nn)

caret_nn_pred <- predict(train_nn,df_test_scaled)

test_results <- df_test_scaled
test_results$preds <- caret_nn_pred 

test_results %>% ggplot() + geom_point(aes(x=x,y=y),color="red") + 
  geom_point(aes(x=x,y=preds),color="blue")

RMSE(predict(train_nn$finalModel,newdata = df_test_scaled),df_test_scaled$y)
#####
tC <- trainControl(method="cv")
tG <- expand.grid(layer1=c(1,3,5,15),layer2=c(1,3,5,15),layer3=c(1,3,5,15))

train_nn <- train(y~x,data=df_train_scaled,trControl=tC,tuneGrid=tG,method="neuralnet")

train_nn$results

plot(train_nn)

caret_nn_pred <- predict(train_nn,df_test_scaled)

test_results <- df_test_scaled
test_results$preds <- caret_nn_pred 

test_results %>% ggplot() + geom_point(aes(x=x,y=y),color="red") + 
  geom_point(aes(x=x,y=preds),color="blue")

RMSE(predict(train_nn$finalModel,newdata = df_test_scaled),df_test_scaled$y)




