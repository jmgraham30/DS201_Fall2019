library(tidyverse)
library(caret)
library(ggfortify)

X <- data.frame(x1=rnorm(10,2,3.5),x2=rnorm(10,5,1.6),x3=rnorm(10,17,22))
X_cs_proc <- preProcess(X,method = c("center","scale")) 
X_cs_df <- predict(X_cs_proc,X)


pr_out1 <- prcomp(X,center = T,scale = T)

as.matrix(X_cs_df) %*% pr_out1$rotation
pr_out1$x

pr_out2 <- prcomp(X_cs_df)

as.matrix(X_cs_df) %*% pr_out2$rotation
pr_out2$x

pr_out3 <- prcomp(X)

as.matrix(apply(X, 2, function(x) x - mean(x))) %*% pr_out3$rotation
pr_out3$x


iris_X <- iris %>% dplyr::select(-Species)
pr_iris <- prcomp(iris_X,center=T,scale=T)
Z <- as.data.frame(pr_iris$x)
Z
names(Z) <- c("Z1","Z2")
Z$Species <- iris$Species

biplot(pr_iris)

Z %>% ggplot(aes(x=Z1,y=Z2,color=Species)) + geom_point()

autoplot(prcomp(iris[ ,1:4],center=T,scale=T), 
         data = iris,colour = 'Species',
         frame = TRUE, frame.type = 'norm',
         loadings.label = TRUE, loadings.label.size = 3)

(pr_var <- pr_iris$sdev ^2)

(pve <- pr_var/sum(pr_var))

cumsum(pve)

library(MASS)

autoplot(prcomp(Boston[,-c(14)],center=T,scale=T), data = Boston,loadings.label = TRUE, loadings.label.size = 3)
