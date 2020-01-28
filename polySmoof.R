
library(polynom)
library(ParamHelpers)
library(checkmate)

source("functions.R")

#Dane wejściowe
library(pracma)
grid_Ackley <- meshgrid(seq(-32, 32, by=0.645))
grid_Aluffi <- meshgrid(seq(-10, 10, by=0.202))
grid_Gold <- meshgrid(seq(-2, 2, by=0.0402))
#Wszystkie permutacje elementów dwoch sekwencji
input_Ackley <- cbind(as.vector(grid_Ackley[[1]]), as.vector(grid_Ackley[[2]]))
input_Aluffi <- cbind(as.vector(grid_Aluffi[[1]]), as.vector(grid_Aluffi[[2]]))
input_Gold <- cbind(as.vector(grid_Gold[[1]]), as.vector(grid_Gold[[2]]))

#SMOOF
#TRAIN
area <- input_Aluffi
y <- generateAluffiPentini(area)
train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
#tworzenie modelu
model <- lm(y~ poly(X1, X2, degree =6, raw=TRUE), data=train_data)

#Błędy zbioru treningowego
intervals <- predict(model, interval='confidence', level = 0.99)
MSEerr_Aluffi <- mse(intervals - train_data$Y) 
MAE_Aluffi <- mae(intervals - train_data$Y) 

#TEST
set.seed(210)
arguments <- 40000
area2 <- matrix(data = runif(arguments,-10,10), nrow=arguments/2, ncol=2)
y2 <- generateAluffiPentini(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)
#Błędy zbioru testowego
intervals_T <- predict(model, newdata = test_data, interval='confidence', level = 0.99)
MSEerr_Aluffi_T <- mse(intervals_T - test_data$Y) 
MAE_Aluffi_T <- mae(intervals_T - test_data$Y)

summary(model)

###############################################################################################################
#CEC2013
#TRAIN
area <- input_Ackley
y <- generateShiftedAndRotatedAckley(area)

train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
#tworzenie modelu
model <- lm(y~ poly(X1,X2, degree =10, raw=TRUE), data=train_data)
#Błędy zbioru treningowego
intervals <- predict(model, interval='confidence', level = 0.99)
MSEerr_Ackley <- mse(intervals - train_data$Y)
MAE_Ackley <- mae(intervals - train_data$Y)

set.seed(210)
arguments <- 40000
area2 <- matrix(data = runif(arguments,-32,32), nrow=arguments/2, ncol=2)
y2 <- generateShiftedAndRotatedAckley(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)
#Błędy zbioru testowego
intervals_T <- predict(model, newdata = test_data, interval='confidence', level = 0.99)
MSEerr_Ackley_T <- mse(intervals_T - test_data$Y)
MAE_Ackley_T <- mae(intervals_T - test_data$Y)

##########################################################################################
#GlobalOpt
#TRAIN
area <- input_Gold
y <- generateGoldPrice(area)

train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
#tworzenie modelu
model <- lm(y~ poly(X1,X2, degree =15, raw=TRUE), data=train_data)
#Błędy zbioru treningowego
intervals <- predict(model, interval='confidence', level = 0.99)
MSEerr_Gold <- mse(intervals - train_data$Y)
MAE_Gold <- mae(intervals - train_data$Y)

set.seed(210)
arguments <- 40000
area2 <- matrix(data = runif(arguments,-2,2), nrow=arguments/2, ncol=2)
y2 <- generateGoldPrice(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)
#Błędy zbioru testowego
intervals_T <- predict(model, newdata = test_data, interval='confidence', level = 0.99)
MSEerr_Gold_T <- mse(intervals_T - test_data$Y)
MAE_Gold_T <- mae(intervals_T - test_data$Y)

##################################################################################################
