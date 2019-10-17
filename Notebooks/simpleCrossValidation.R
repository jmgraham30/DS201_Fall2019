library(tidyverse)
library(caret)

set.seed(7176)

df <- data.frame(x=rnorm(250,15,7.2))
df <- df %>% mutate(y=3*x - 5 + rnorm(250,sd=10))

df %>% ggplot(aes(x=x,y=y)) + geom_point()

train_id <- createDataPartition(df$y,p=0.75,list=FALSE)

df_train <- df[train_id, ]
df_test <- df[-train_id, ]

df_train %>% ggplot(aes(x=x,y=y)) + geom_point()
df_test %>% ggplot(aes(x=x,y=y)) + geom_point()



folds <- 10
reps <- 5
df_folds <- createMultiFolds(df_train$y,k=folds,times=reps)
k <- 1
err_mat <- matrix(0,nrow=folds,ncol=reps)
for (i in 1:reps){
  for (j in 1:folds){
    t_train <- lm(y~x,df_train[df_folds[[k]], ])
    t_pred <- predict(t_train,new=df_train[-df_folds[[k]], ])
    err_mat[j,i] <- sum((t_pred - df_train[-df_folds[[k]], 2])^2)/length(t_pred) 
    k <- k+1 
  }
}


err_mat

mean(apply(sqrt(err_mat),2,mean))


tt_train <- lm(y~x,df_train)
tt_pred <- predict(tt_train,new=df_test)
test_err <- sum((tt_pred - df_test$y)^2)/length(tt_pred) 
sqrt(test_err)


tC <- trainControl(method = "repeatedcv",number=10,repeats=5)


tM <- train(y~x,data=df_train,method="lm",trControl=tC)

tM$results[2]

pT <- predict.train(tM,newdata = df_test)
tE <- sum((pT - df_test$y)^2)/length(pT) 
sqrt(tE)


train_iris_id <- createDataPartition(iris$Species,p=0.75,list=FALSE)

iris_train <- iris[train_iris_id, ]
iris_test <- iris[-train_iris_id, ]


tC_iris <- trainControl(method = "repeatedcv",number=10,repeats=5)

tG <- expand.grid(k=c(1,3,5,7,9,11,13))

tM_iris <- train(Species~.,data=iris_train,method="knn",trControl=tC,tuneGrid=tG)

plot(tM_iris)

tM_iris$results


pT_iris <- predict(tM_iris,newdata=iris)


iris <- iris %>% mutate(Preds=pT_iris)

iris %>% ggplot(aes(x=Petal.Length,y=Petal.Width,color=Species,shape=Preds)) + geom_point()


table(iris$Species,iris$Preds)

1 - 5/nrow(iris)




