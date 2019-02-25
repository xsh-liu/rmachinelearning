models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

#Q2
length(mnist_27$test$y)
length(models)

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)

#Q3
c <- 1:23
confusionvector <- sapply(c, function(c){
  confusionMatrix(factor(pred[,c]), mnist_27$test$y)$overall["Accuracy"]})

mean(confusionvector)

#Answer's Codes
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

#Q4
library(purrr)
library(dplyr)
df <- data.frame(matrix(unlist(pred), nrow=23, byrow=T))
colnames(df) <- seq(1:200)
rownames(df) <- models


loop <- for (i in 1:ncol(df)) {
  ifelse(test = sum(df[,1] == 7) > 12, yes = 7, no = 2)}


col_index <- seq(1,ncol(df), 1)
predict_vote <- map_df(col_index, function(j){
  vote <- ifelse(test = sum(df[,j] == 7) > 12, yes = 7, no = 2)
  return(data_frame(vote = vote))
})   # returns a df
  
predict_vote <- as.factor(predict_vote$vote) #  as factor

confusionMatrix(predict_vote,  mnist_27$test$y)

#I am stupid... Answer's Codes
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Q5


#Q6
fits <- lapply(models, function(model){ print(model)
  train(y ~ ., method = model, data = mnist_27$train, trControl=trainControl(method = 'cv')) })
accuracy_cv<-sapply(models,function(m){ fits[[m]][['results']][['Accuracy']]})
mean(accuracy_cv)
##This assignment has not been completed. Need future review. 