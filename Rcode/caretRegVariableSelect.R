library(tidyverse)
library(caret)
library(ISLR)
library(MASS)
library(leaps)


train_ids <- createDataPartition(diamonds$price,p=0.75,list = F)
D_train <- diamonds[train_ids, ]
D_test <- diamonds[-train_ids, ]

x1 <- rnorm(500,12,3.7)
x2 <- runif(500,0,50)
x3 <- rnorm(500,100,57)

y <- 2*x1 - 7*x2 + 10 + rnorm(500,sd=12)


fake_df <- data.frame(x1=x1,x2=x2,x3=x3,y=y)

train_ids <- createDataPartition(fake_df$y,p=0.75,list=F)
fake_train <- fake_df[train_ids, ]
fake_test <- fake_df[-train_ids, ]

tC <- trainControl(method="repeatedcv",number=10,repeats=5)
t_result <- train(y~.,data=fake_train,
                  trControl=tC,
                  method="lmStepAIC")

plot(t_result)

t_result$results

t_result$finalModel

resids <- fake_test$y -  predict(t_result,fake_test)

sqrt(sum(resids^2)/length(resids))


