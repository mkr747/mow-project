
library(polynom)
library(ParamHelpers)
library(checkmate)

source("functions.R")

#TRAIN
set.seed(9)
arguments <- 400000
area <- matrix(data = runif(arguments,-10,10), nrow=arguments/2, ncol=2)
y <- generateAluffiPentini(area)
train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)

model <- lm(y~ poly(X1, X2, degree =4, raw=TRUE), data=train_data)

intervals <- predict(model, interval='confidence', level = 0.99)
MSEerr <- mse(intervals - train_data$Y) #maly blad

#TEST
set.seed(210)
area2 <- matrix(data = runif(arguments,-10,10), nrow=arguments/2, ncol=2)
y2 <- generateAluffiPentini(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)

intervals_T <- predict(model, newdata = test_data, interval='confidence', level = 0.99)
MSEerr_T <- mse(intervals_T - test_data$Y) 

summary(model)

#CEC2013
set.seed(42)
#d <- 2 #wymiary, zmiennych jest tyle ile wymiarów
arguments <- 2
area <- matrix(data = runif(arguments,-30000,30000), nrow=arguments*50, ncol=2)
y <- generateShiftedAndRotatedAckley(area)
train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)

model <- lm(y~ poly(X1,X2, degree =10, raw=TRUE), data=train_data)

intervals <- predict(model, interval='confidence', level = 0.99)
MSEerr <- mse(intervals - train_data$Y) #maly blad