#Confusion Matrix for PredSex
table(predicted = y_hat3, actual = test_set$sex)

test_set %>% mutate(y_hat = y_hat3) %>%
  group_by(sex) %>%
  summarize(accuracy = mean(y_hat == sex))

prev <- mean(train_set$sex == "Male")
prev

confusionMatrix(data = y_hat3, reference = test_set$sex)
