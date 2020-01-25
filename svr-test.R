library(e1071)
library(globalOptTests)

require(datasets)

#GOLDSTEIN-PRICE FUNCTION
#[-2,2] - x range
#func1 <- function(x1, x2) (1 + (x1 + x2 +1)^2(19-14*x1+3*x2-14*x2+6*x1*x2+3*x2^2)) x (30+(2*x1-3*x2)^2*(18-32*x1+12*x1^2+48*x2-36*x1*x2+27*x2^2))


y1 <- 0
arguments <- 2000
area <- matrix(data = runif(arguments,-2,2), nrow=arguments/2, ncol=2)
for(row in 1:nrow(area)){
  y1[row] <- globalOptTests::goTest(c(area[row, 1], area[row, 2]), "GoldPrice")
}

data_output = data.frame(X1 = area[,1], X2 = area[,2], Y = y1)
model <- svm(Y ~ X1 + X2 , data_output)


predicted_function <- predict(model, newdata = data_output, progress = "window")

rmse <- function(error)
{
  sqrt(mse(error))
}

mse <- function(error)
{
  mean(error^2)
}

error <- model$residuals  # same as data$Y - predictedY
predictionRMSE <- rmse(error)   # 5.703778
predictionMSE <- mse(error)

plot(c(1:1000), y1)
points(x= c(1:1000), y = predicted_function, col = "red", pch=4)

# perform a grid search
tuneResult <- tune(svm, Y ~ X1 + X2,  data = data_output,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)

tuneModel <- tuneResult$best.model
tuneModelY <- predict(tuneModel, data_output)
tunedModelRMSE <- rmse(error)

plot(c(1:1000), y1)
points(x= c(1:1000), y = tuneModelY, col = "red", pch=4)
