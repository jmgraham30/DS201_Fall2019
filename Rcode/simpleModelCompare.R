library(tidyverse)
library(caret)
library(broom)

x <- rnorm(45,mean=7,sd=2.2)
y <- 2*x^2 - 3*x + 13 + rnorm(45,sd=5)

df <- data.frame(x=x,y=y)

df %>% ggplot(aes(x=x,y=y)) + geom_point()  + 
  geom_smooth(method = "lm",formula = y~poly(x,2))


summary(lm(y~x,df))
summary(lm(y~poly(x,2),df))

lm_linear <- lm(y~x,df)

lm_quad <- lm(y~poly(x,2),df)

lm_linear_aug <- augment(lm_linear)

lm_quad_aug <- augment(lm_quad)


lm_linear_aug %>% ggplot(aes(x=.fitted,y=.resid)) + geom_point()
lm_quad_aug  %>% ggplot(aes(x=.fitted,y=.resid)) + geom_point()


anova(lm_linear,lm_quad)


glance(lm_linear)

glance(lm_quad)

AIC(lm_linear,lm_quad)

tC <- trainControl(method = "repeatedcv",number=10,repeats=5)

tM_lin <- train(y~x,data=df,method="lm",trControl=tC)

tM_quad <- train(y~poly(x,2),data=df,method="lm",trControl=tC)

tM_cube <- train(y~poly(x,3),data=df,method="lm",trControl=tC)

tM_quin <- train(y~poly(x,4),data=df,method="lm",trControl=tC)

tM_lin$results

tM_quad$results

tM_cube$results

tM_quin$results

AIC(tM_lin$finalModel,tM_quad$finalModel,tM_cube$finalModel,tM_quin$finalModel)

anova(tM_lin$finalModel,tM_quad$finalModel,tM_cube$finalModel,tM_quin$finalModel)


