library(caret)
library(purrr)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

#Q2
cutoff <- seq(0,10,0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "versicolor", "virginica") %>%
    factor(levels = levels(test$Species))
  F_meas(data = y_hat, reference = factor(train$Species))
})

best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(test$Sepal.Length > best_cutoff, "versicolor", "virginica") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)

yaccu <- mean(y_hat == test$Species)