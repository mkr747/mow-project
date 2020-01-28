library("ggplot2")
library("nnet")

library(pracma)
grid_Ackley <- meshgrid(seq(-32, 32, by=0.645))
grid_Aluffi <- meshgrid(seq(-10, 10, by=0.202))
grid_Gold <- meshgrid(seq(-2, 2, by=0.0402))
input_Ackley <- cbind(as.vector(grid_Ackley[[1]]), as.vector(grid_Ackley[[2]]))
input_Aluffi <- cbind(as.vector(grid_Aluffi[[1]]), as.vector(grid_Aluffi[[2]]))
input_Gold <- cbind(as.vector(grid_Gold[[1]]), as.vector(grid_Gold[[2]]))

area <- input_Aluffi
y <- generateAluffiPentini(area)
train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
#Model sieci neuronowej
nn <- nnet(y~X1+X2, data=train_data, size=60, maxit=1000, linout=TRUE, skip=TRUE)

out <- predict(nn, train_data)
MSEerr_Aluffi <- mse(out - train_data$Y) 
MAE_Aluffi <- mae(out - train_data$Y)


#TEST
set.seed(210)
arguments <- 40000
area2 <- matrix(data = runif(arguments,-10,10), nrow=arguments/2, ncol=2)
y2 <- generateAluffiPentini(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)

out_T <- predict(nn, test_data)
MSEerr_Aluffi_T <- mse(out_T - test_data$Y) 
MAE_Aluffi_T <- mae(out_T - test_data$Y)

###############################################################################################################
#CEC2013
area <- input_Ackley
y <- generateShiftedAndRotatedAckley(area)

train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
nn <- nnet(y~X1+X2, data=train_data, size=60, maxit=1000, linout=TRUE, skip=TRUE)
out <- predict(nn, train_data)
MSEerr_Ackley <- mse(out - train_data$Y)
MAE_Ackley <- mae(out - train_data$Y)

set.seed(210)
arguments <- 40000
area2 <- matrix(data = runif(arguments,-32,32), nrow=arguments/2, ncol=2)
y2 <- generateShiftedAndRotatedAckley(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)
out_T <- predict(nn, test_data)
MSEerr_Ackley_T <- mse(out_T - test_data$Y)
MAE_Ackley_T <- mae(out_T - test_data$Y)

##########################################################################################
#GlobalOpt
area <- input_Gold
y <- generateGoldPrice(area)

train_data = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
nn <- nnet(y~X1+X2, data=train_data, size=100, maxit=5000, linout=TRUE, skip=TRUE)
out <- predict(nn, train_data)
MSEerr_Gold <- mse(out - train_data$Y)
MAE_Gold <- mae(out - train_data$Y)

set.seed(210)
arguments <- 40000
area2 <- matrix(data = runif(arguments,-2,2), nrow=arguments/2, ncol=2)
y2 <- generateGoldPrice(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)

out_T <- predict(nn, test_data)
MSEerr_Gold_T <- mse(out_T - test_data$Y)
MAE_Gold_T <- mae(out_T - test_data$Y)

##################################################################################################


