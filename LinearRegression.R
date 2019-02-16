library(HistData)
library(caret)
library(dplyr)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg
mean((avg - test_set$son)^2)

fit <- lm(son ~ father, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2] * test_set$father
mean((y_hat - test_set$son)^2)

y_hat1 <- predict(fit, test_set)
mean((y_hat1 - test_set$son)^2)
