library(tidyverse)
library(caret)

x <- diamonds %>% select(carat,depth,table,x,y,z)
y <- diamonds %>% select(price)

subsets <- c(1:6)

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(diamonds[,c(1,5,6,8,9,10)],diamonds$price,
                 sizes = subsets,
                 rfeControl = ctrl)

# summarize the results
print(lmProfile)
# list the chosen features
predictors(lmProfile)
# plot the results
plot(lmProfile, type=c("g", "o"))
