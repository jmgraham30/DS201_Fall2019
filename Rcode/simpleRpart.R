library(tidyverse)
library(ISLR)
library(rpart)
library(rpart.plot)


head(Hitters)

dim(Hitters)

names(Hitters)

summary(Hitters)

df <- Hitters[complete.cases(Hitters), ]

df$Log_salaray <- log(df$Salary)

t_mod <- rpart(Log_salaray ~ Years + Hits,data=df,method = "anova")

printcp(t_mod)

plotcp(t_mod)

rsq.rpart(t_mod)

rpart.plot(t_mod,type = 5)

t_mod$variable.importance

pfit<- prune(t_mod, cp=0.1) 

pfit$variable.importance

rpart.plot(pfit,type = 5)
rattle::fancyRpartPlot(pfit,caption = NULL)

df %>% ggplot(aes(x=Years,y=Hits)) + geom_point() + 
  geom_vline(xintercept = 5,color="green") + 
  geom_segment(aes(x = 5, y = 118, xend = 25, yend = 118),color="green")


c_mod <- rpart(Species~.,data=iris,method = "class")

printcp(c_mod)

plotcp(c_mod)

rsq.rpart(c_mod)

c_mod$variable.importance

rpart.plot(c_mod,type=5)
rattle::fancyRpartPlot(c_mod,caption = NULL)

iris %>% ggplot(aes(x=Petal.Length,y=Petal.Width,color=Species)) + 
  geom_point() + 
  geom_vline(xintercept = 2.5,color="green") + 
  geom_segment(aes(x = 2.5, y = 1.8, xend = 7, yend = 1.8),color="green")


pred_classes <- predict(c_mod,newdata = iris,type="class")
library(caret)
confusionMatrix(pred_classes,iris$Species)$table




