library(randomForest)
library(randomForestExplainer)
library(MASS)
library(tidyverse)

x <- Boston %>% dplyr::select(-medv)
y <- Boston %>% dplyr::select(medv) %>% unlist()


tune_rf_boston <- tuneRF(x,y,stepFactor=1.5, improve=1e-5, ntree=500)


print(tune_rf_boston)


boston_rf <- randomForest(medv~.,data=Boston,mtry=6,localImp = TRUE)

boston_rf$importance

plot(boston_rf)

min_depth_frame <- min_depth_distribution(boston_rf)

plot_min_depth_distribution(min_depth_frame)

importance_frame <- measure_importance(boston_rf)

plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

explain_forest(boston_rf, interactions = TRUE, data = Boston)


