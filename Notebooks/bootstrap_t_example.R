library(tidyverse)
library(boot)
library(resampledata)

x_vals <- Walleye$Weight

x_mean <- mean(x_vals)
x_sd <- sd(x_vals)
n <- length(x_vals)

samp_t <- (x_mean - 2.5)/(x_sd/sqrt(n))

N <- 10000

boot_t_vals <- numeric(N)

for (i in 1:N){
  temp_x <- sample(x_vals,replace = TRUE)
  boot_t_vals[i] <- (mean(temp_x) - x_mean)/(sd(temp_x)/sqrt(n))
}

ggplot(data=NULL,mapping=aes(x=boot_t_vals)) + geom_histogram() + geom_vline(xintercept = samp_t)

sum(boot_t_vals <= samp_t)/N

t.test(x_vals,mu=2.5,conf.level = 0.95,alternative = "less")
