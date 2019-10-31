library(tidyverse)
library(ISLR)
library(rpart)
library(rpart.plot)

head(Hitters)

names(Hitters)

df <- Hitters[complete.cases(Hitters), ]

df$Log_salaray <- log(df$Salary)

t_mod <- rpart(Log_salaray ~ Years + Hits,data=df,method = "anova")

printcp(t_mod)

plotcp(t_mod)

rsq.rpart(t_mod)

pfit<- prune(t_mod, cp=0.1) 

rpart.plot(pfit,type = 5)

df %>% ggplot(aes(x=Years,y=Hits)) + geom_point() + 
  geom_vline(xintercept = 5,color="green") + 
  geom_segment(aes(x = 5, y = 118, xend = 25, yend = 118),color="green")


c_mod <- rpart(Species~.,data=iris,method = "class")

printcp(c_mod)

plotcp(c_mod)

rsq.rpart(c_mod)

rpart.plot(c_mod,type=5)


iris %>% ggplot(aes(x=Petal.Length,y=Petal.Width,color=Species)) + 
  geom_point() + 
  geom_vline(xintercept = 2.5,color="green") + 
  geom_segment(aes(x = 2.5, y = 1.8, xend = 7, yend = 1.8),color="green")
