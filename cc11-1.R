#Q1
library(caret)
library(Rborist)
library(rpart)

n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

set.seed(1)
minNode <- seq(25, 100, 25)
fit <- train(y ~ x, 
             data = dat, 
             method = "Rborist", 
             tuneGrid = data.frame(predFixed = 1, minNode = minNode))

fit

#Q2
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)

#Q3
library(dslabs)
set.seed(1991)
data("tissue_gene_expression")
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

cp <- seq(0, 0.1, 0.01)
fit <- train(x,y,
             method = "rpart", 
             tuneGrid = data.frame(cp))
fit

#Answer's codes
set.seed(1991)
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

#Q4
confusionMatrix(fit)

#Q5
set.seed(1991)
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  control = rpart.control(minsplit = 0),
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
confusionMatrix(fit)

#Q6
plot(fit$finalModel)
text(fit$finalModel)

#Q7
library(randomForest)
set.seed(1991)
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50,200, 25))))
fit
confusionMatrix(fit)

#Q8
imp <- varImp(fit)

#Q9
data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)

