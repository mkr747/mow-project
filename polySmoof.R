library(polynom)
library(ParamHelpers)
library(checkmate)

source("functions.R")

set.seed(9)

arguments <- 40000
area <- matrix(data = runif(arguments,-10,10), nrow=arguments/2, ncol=2)
y <- generateAluffiPentini(area)

model <- lm(y ~ poly(area[,1],area[,2], degree = 6, raw=TRUE)) # tu trzeba sporzadzic dataset y x1 x2
train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)

set.seed(210)
area2 <- matrix(data = runif(arguments,10,20), nrow=arguments/2, ncol=2)
y2 <- generateAluffiPentini(area)
test_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y2)

intervals <- predict(model, test_data, interval='confidence', level = 0.9)
MSEerr <- mse(intervals - test_data$Y)