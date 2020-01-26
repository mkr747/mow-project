library(polynom)

source("functions.R")

set.seed(42)

arguments <- 40000000
area <- matrix(data = runif(arguments,-10,10), nrow=arguments/2, ncol=2)
y <- generateAluffiPentini(area)

model <- lm(y ~ poly(area[,1],area[,2], degree = 6, raw=TRUE)) # tu trzeba sporzadzic dataset y x1 x2
data_outpu = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
intervals <- predict(model, data_outpu, interval='confidence', level = 0.9)
MSEerr <- mse(model$residuals)