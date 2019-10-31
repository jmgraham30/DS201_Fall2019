library(tidyverse)
library(broom)
library(faraway)


head(gala)

gala_lm <- lm(Species~.,data=gala)

tidy(gala_lm)

gala_lm_step <- step(gala_lm,direction="backward")

gala_lm_step$anova

gala_lm_step$call

extractAIC(gala_lm_step)

extractAIC(gala_lm)
