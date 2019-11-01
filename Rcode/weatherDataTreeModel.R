library(magrittr)     # Pipe operator %>% %<>% %T>% equals().
library(lubridate)    # Dates and time.
library(rattle)       # normVarNames().
library(ROCR)         # Use prediction() for evaluation.
library(rpart)        # Model: decision tree.
library(scales)       # Include commas in numbers.
library(stringi)      # String concat operator %s+%.
library(tidyverse)    # ggplot2, tibble, tidyr, readr, purr, dplyr, stringr
library(caret)
library(rpart)

dsorig <- file.path("https://rattle.togaware.com/weatherAUS.csv")

# Name of the dataset.

dsname <- "weatherAUS"


dsloc  <- "https://essentials.togaware.com"
dspath <- file.path(dsloc, dsname %s+% ".csv") %T>% print()

dspath %>% read_csv() %>% assign(dsname, ., envir=.GlobalEnv)

dsname %>% get() %T>% print() -> ds

names(ds) %<>% normVarNames() %T>% print()

names(ds)[23] <- c("rainfall_tomorrow")

var_names_df <- data.frame(var_names = names(ds),stringsAsFactors = F)

var_names_df <- arrange(var_names_df,var_names)

ingnore_vars <- var_names_df$var_names[c(3,7,10,15,17,18)]

ds_for_model <- ds %>% dplyr::select(-ingnore_vars)

dim(ds_for_model)

names(ds_for_model)

train_ids <- createDataPartition(ds_for_model$rain_tomorrow,p=0.8,list=F)

weather_train <- ds_for_model[train_ids, ]
weather_test <- ds_for_model[-train_ids, ]

str(weather_train)


weather_model <- rpart(rain_tomorrow~., data=weather_train)


summary(weather_model)

rpart.plot::rpart.plot(weather_model,type=5)

fancyRpartPlot(weather_model)

ggVarImp(weather_model,log=T)





