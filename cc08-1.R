#Q1
set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

#Q2
library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value


#Q3
ind <- which(pvals < 0.01)
length(ind)

#Q4 
x_subset1 <- x[,ind]
fit1 <- train(x_subset1, y, method = "glm")
fit1$results

#Q5
fit2 <- train(x_subset1, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit2)

#Q7
