
library(polynom)
library(ParamHelpers)
library(checkmate)

source("functions.R")

#SMOOF
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
RMSEerr <- rmse(intervals_T - test_data$Y)

summary(model)

###############################################################################################################
#CEC2013
set.seed(42)
#d <- 2 #wymiary, zmiennych jest tyle ile wymiarów
arguments <- 40000
area <- mat #matrix(data = runif(arguments,-32,32), nrow=arguments/2, ncol=2)
y <- generateShiftedAndRotatedAckley(area)

train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
model <- lm(y~ poly(X1,X2, degree =2, raw=TRUE), data=train_data)
intervals <- predict(model, interval='confidence', level = 0.99)
MSEerr <- mse(intervals - train_data$Y)

set.seed(210)
arguments <- 40000
area2 <- matrix(data = runif(arguments,-32,32), nrow=arguments/2, ncol=2)
y2 <- generateShiftedAndRotatedAckley(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)
intervals_T <- predict(model, newdata = test_data, interval='confidence', level = 0.99)
MSEerr_T <- mse(intervals_T - test_data$Y)
RMSEerr <- rmse(intervals_T - test_data$Y)

##########################################################################################
#GlobalOpt - dwoch zmiennych
set.seed(42)
arguments <- 40000
area <- matrix(data = runif(arguments,-2,2), nrow=arguments/2, ncol=2)
y <- generateGoldPrice(area)

train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
model <- lm(y~ poly(X1,X2, degree =9, raw=TRUE), data=train_data) # min 8 - 2.5 -18
intervals <- predict(model, interval='confidence', level = 0.99)
MSEerr <- mse(intervals - train_data$Y)

set.seed(210)
arguments <- 40000
area2 <- matrix(data = runif(arguments,-2,2), nrow=arguments/2, ncol=2)
y2 <- generateShiftedAndRotatedAckley(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)
intervals_T <- predict(model, newdata = test_data, interval='confidence', level = 0.99)
MSEerr_T <- mse(intervals_T - test_data$Y)
RMSEerr <- rmse(intervals_T - test_data$Y)

##################################################################################################
#Test nowych wartości
library(pracma)
grid_Ackley <- meshgrid(seq(-32, 32, by=1))
grid_Aluffi <- meshgrid(seq(-10, 10, by=0.2))
grid_Gold <- meshgrid(seq(-2, 2, by=0.05))
input_Ackley <- cbind(as.vector(grid_Ackley[[1]]), as.vector(grid_Ackley[[2]]))
input_Aluffi <- cbind(as.vector(grid_Aluffi[[1]]), as.vector(grid_Aluffi[[2]]))
input_Gold <- cbind(as.vector(grid_Gold[[1]]), as.vector(grid_Gold[[2]]))