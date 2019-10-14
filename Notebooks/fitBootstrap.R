library(tidyverse)
library(boot)


fit_method <- "lm"

x <- rnorm(50,mean=5,sd=7.2)

y <- 4*x - 3 + rnorm(50,sd=14)

df <- data.frame(x=x,y=y)

df %>% ggplot(aes(x=x,y=y)) + 
  geom_point() + 
  geom_smooth(method=fit_method)


N <- 5000

get_lm_bootstrap <- function(data_df,id){
  re_fit <- lm(y~x,data=data_df[id, ])
  predict(re_fit,new=data_df)
}

boot_result <- boot(data=df,statistic = get_lm_bootstrap,R=N,stype="i")

boot_se <- apply(boot_result$t, MARGIN=2, FUN = function(x) sd(x,na.rm=T))

lower <- boot_result$t0 - 2*boot_se
upper <- boot_result$t0 + 2*boot_se

df <- df %>% mutate(lower=lower,upper=upper)

df %>% ggplot(aes(x=x,y=y)) + 
  geom_point() + 
  geom_smooth(method=fit_method) + 
  geom_errorbar(aes(x=x,ymin=lower,ymax=upper),color="red")

kidney_df <- read.table("https://web.stanford.edu/~hastie/CASI_files/DATA/kidney.txt",header = T)

head(kidney_df)

names(kidney_df) <- c("x","y")

boot_result <- boot(data=kidney_df,statistic = get_lm_bootstrap,R=N,stype="i")

boot_se <- apply(boot_result$t, MARGIN=2, FUN = sd)

lower <- boot_result$t0 - 2*boot_se
upper <- boot_result$t0 + 2*boot_se

kidney_df <- kidney_df %>% mutate(lower=lower,upper=upper)

kidney_df %>% ggplot(aes(x=x,y=y)) + 
  geom_point() + 
  geom_smooth(method=fit_method) + 
  geom_errorbar(aes(x=x,ymin=lower,ymax=upper),color="red")

