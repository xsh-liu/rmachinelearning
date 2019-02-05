library(caret)
library(tidyverse)
library(dslabs)
data(heights)

y <- heights$sex
x <- heights$height

set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

#1st algo
y_hat1 <- sample(c("Male", "Female"),
                length(test_index), replace = TRUE) %>%
      factor(levels = levels(test_set$sex))

y1accu <- mean(y_hat1 == test_set$sex)

#2nd algo 
y_hat2 <- ifelse(x > 62, "Male", "Female") %>% 
       factor(levels = levels(test_set$sex))

y2accu <- mean(y_hat2 == test_set$sex)

#3rd algo
cutoff <- seq(61,70)
accuracy3 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
        factor(levels = levels(test_set$sex))
        mean(y_hat == train_set$sex)
})

plot(accuracy3)
max(accuracy3)

best_cutoff3 <- cutoff[which.max(accuracy3)]

y_hat3 <- ifelse(test_set$height > best_cutoff3, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat3 <- factor(y_hat3)

y3accu <- mean(y_hat3 == test_set$sex)

#Display Accuracies
y1accu
y2accu
y3accu