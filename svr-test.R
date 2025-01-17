library(e1071)
source("functions.R")

#Test nowych wartości
library(pracma)
grid_Ackley <- meshgrid(seq(-32, 32, by=6.45))
grid_Aluffi <- meshgrid(seq(-10, 10, by=2.02))
grid_Gold <- meshgrid(seq(-2, 2, by=0.402))
input_Ackley <- cbind(as.vector(grid_Ackley[[1]]), as.vector(grid_Ackley[[2]]))
input_Aluffi <- cbind(as.vector(grid_Aluffi[[1]]), as.vector(grid_Aluffi[[2]]))
input_Gold <- cbind(as.vector(grid_Gold[[1]]), as.vector(grid_Gold[[2]]))

##########################################################################################################
#SMOOF
area <- input_Aluffi
y <- generateAluffiPentini(area)
train_data_aluffi = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
model_svr_aluffi <- svm(Y ~ X1 + X2 , train_data_aluffi, kernel = 'radial')

MSE_Aluffi_svm <- mse(model_svr_aluffi$residuals)
MAE_Aluffi_svm<- mae(predictions_aluffi - test_data_aluffi$Y)
RMSE_Aluffi_svm <- rmse(predictions_aluffi - test_data_aluffi$Y)

tuneResult_aluffi <- tune(svm, Y ~ X1 + X2,  data = train_data_aluffi,
                   ranges = list(epsilon = seq(0,0.03,0.001), cost = 2^(2:9))
)

print(tuneResult_aluffi)
plot(tuneResult_aluffi)

tuneModel_aluffi <- tuneResult_aluffi$best.model

#TEST
set.seed(210)
arguments <- 40
area2 <- matrix(data = runif(arguments,-10,10), nrow=arguments/2, ncol=2)
y2 <- generateAluffiPentini(area2)
test_data_aluffi = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)

predictions_notTuned <- predict(model_svr_aluffi, newdata = test_data_aluffi)
predictions_aluffi <- predict(tuneModel_aluffi, newdata = test_data_aluffi, interval='confidence', level = 0.99)
MSE_Aluffi_T <- mse(predictions_aluffi - test_data_aluffi$Y)
MAE_Aluffi_T<- mae(predictions_aluffi - test_data_aluffi$Y)
RMSE_Aluffi_T <- rmse(predictions_aluffi - test_data_aluffi$Y)

summary(tuneModel_aluffi)

###############################################################################################################
#CEC2013
area <- input_Ackley
y <- generateShiftedAndRotatedAckley(area)

train_data_ackley = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
model_svr_ackley <- svm(Y ~ X1 + X2, data = train_data_ackley)

tuneResult_ackley <- tune(svm, Y ~ X1 + X2,  data = train_data_ackley,
                          ranges = list(epsilon = seq(0,0.003,0.0001), cost = 2^(8:12))
)

print(tuneResult_ackley)
plot(tuneResult_ackley)

tuneModel_ackley <- tuneResult_ackley$best.model

#TEST
set.seed(210)
arguments <- 40
area2 <- matrix(data = runif(arguments,-32,32), nrow=arguments/2, ncol=2)
y2 <- generateShiftedAndRotatedAckley(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)

predictions_ackley <- predict(tuneModel_ackley, newdata = test_data)
MAE_Ackley <- mae(predictions_ackley - test_data$Y)
MSE_Ackley_T <- mse(predictions_ackley - test_data$Y)
RMSE_Ackley_T <- rmse(predictions_ackley - test_data$Y)

summary(tuneModel_ackley)

##########################################################################################
#GlobalOpt - dwoch zmiennych
area <- input_Gold
y <- generateGoldPrice(area)

train_data_gold = data.frame(X1 = area[,1], X2 = area[,2], Y = y)
model_svr_gold <- svm(Y ~ X1 + X2, data = train_data_gold, kernel = 'radial')

tuneResult_gold <- tune(svm, Y ~ X1 + X2,  data = train_data_gold,
                          ranges = list(epsilon = seq(0,0.02,0.001), cost = 2^(8:10))
)

print(tuneResult_gold)
plot(tuneResult_gold)

tuneModel_gold <- tuneResult_gold$best.model

#TEST
set.seed(210)
arguments <- 40
area2 <- matrix(data = runif(arguments,-2,2), nrow=arguments/2, ncol=2)
y2 <- generateGoldPrice(area2)
test_data = data.frame(X1 = area2[,1], X2 = area2[,2], Y = y2)

predictions_gold = predict(tuneModel_gold, newdata = test_data)

MAE_gold <- mae(predictions_gold - test_data$Y)
MSE_gold_T <- mse(predictions_gold - test_data$Y)
RMSE_gold_T <- rmse(predictions_gold - test_data$Y)

##################################################################################################
