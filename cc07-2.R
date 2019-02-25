#Q1

library(tidyverse)
library(caret)
library(purrr)
library(dslabs)

data(heights)

set.seed(1)
ks <- seq(1, 101, 3)
sex <- heights$sex
height <- heights$height



f1 <- sapply(ks, function(k) {
  test_index <- createDataPartition(sex, times = 1, p = 0.5, list = FALSE)
  
  train_set <- heights[-test_index,]
  
  test_set <- heights[test_index,]
  
  fit <- knn3(sex ~ height, data = train_set, k = k)
  
  yhat <- predict(fit, data = test_set, type = "class") %>% factor(levels = levels(test_set$sex))
  
  F_meas(data = yhat, reference = factor(train_set$sex))
  
})

max(f1)

  
  
#Q2
  library(dslabs)
  data("tissue_gene_expression")
  y <- tissue_gene_expression$y
  d <- dist(tissue_gene_expression$x)
  
  ind <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train <- tissue_gene_expression %>% slice(-ind)
  test <- tissue_gene_expression %>% slice(ind)
  
  ft <- knn3(y ~ x, data = train, k = 1)
  yh <- predict(ft, data = test, type = "class")  %>% factor(levels = levels(test$y))
  
  F_meas(data = yh, reference = factor(train$y))
  
