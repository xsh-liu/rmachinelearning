x <- matrix(rnorm(100*10), 100, 10) 

dim(x)
nrow(x)
ncol(x)

x <- sweep(x, 2, 1:nrow(x),"+")
x <- x + seq(nrow(x))


x <- sweep(x, 2, 1:ncol(x), FUN = "+")
x <- 1:col(x)
x <- -x
x <- 1:ncol(x)


mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y)