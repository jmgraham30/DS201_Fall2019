library(tidyverse)
library(caret)
library(broom)

x <- rnorm(45,mean=7,sd=2.2)
y <- 2*x - 13 + rnorm(45,sd=5)

df <- data.frame(x=x,y=y)

resamps <- createResample(df$y,times=5000)

resamp_df <- lapply(resamps,function(x) df[x, ])

get_slope_coeff <- function(rs_df){
  lm_fit <- lm(y~x,rs_df)
  c(coefficients(lm_fit),r_squared=summary(lm_fit)$r.squared)
}

results <- lapply(resamp_df,get_slope_coeff)

names(results)

slopes_2 <- lapply(names(results),function(x) results[x][[1]][2])

unlist(slopes_2)


