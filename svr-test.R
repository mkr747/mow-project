library(e1071)
source("functions.R")

set.seed(42)

arguments <- 2000
area <- matrix(data = runif(arguments,-2,2), nrow=arguments/2, ncol=2)
y1 <- generateGoldPrice(area)
data_output = data.frame(X1 = area[,1], X2 = area[,2], Y = y1)
model <- svm(Y ~ X1 + X2 , data_output)

arguments2 <- 200
set.seed(28)
area2 <- matrix(data = runif(arguments2,-2,2), nrow=arguments2/2, ncol=2)
y2 <- generateGoldPrice(area2)
data2 = data.frame(X1 = area2[,1], X2 = area2[,2])
predicted_function <- predict(model, newdata = data2)

error <- y2 - predicted_function  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778
predictionMSE <- mse(error)

plot(c(1:(arguments2/2)), y2)
points(x= c(1:(arguments2/2)), y = predicted_function, col = "red", pch=4)

# perform a grid search
tuneResult <- tune(svm, Y ~ X1 + X2,  data = data_output,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

tuneModel <- tuneResult$best.model
tuneModelY <- predict(tuneModel, data_output)
error <- data_output$Y - tuneModelY  
tunedModelRMSE <- rmse(error)

plot(c(1:(arguments/2)), y1)
points(x= c(1:(arguments/2)), y = tuneModelY, col = "red", pch=4)
