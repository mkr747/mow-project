rmse <- function(error)
{
  sqrt(mse(error))
}

mse <- function(error)
{
  mean(error^2)
}

#GOLDSTEIN-PRICE FUNCTION
#[-2,2] - x range
#func1 <- function(x1, x2) (1 + (x1 + x2 +1)^2(19-14*x1+3*x2-14*x2+6*x1*x2+3*x2^2)) x (30+(2*x1-3*x2)^2*(18-32*x1+12*x1^2+48*x2-36*x1*x2+27*x2^2))
generateGoldPrice <- function(arguments){
  library(globalOptTests)
  result <- 0
  for(row in 1:nrow(arguments)){
    result[row] <- goTest(c(arguments[row, 1], arguments[row, 2]), "GoldPrice")
  }
  
  return <- result
}

generateAluffiPentini <- function(arguments){
  library(smoof)
  fn = makeAluffiPentiniFunction()
  fn = addCountingWrapper(fn)
  result <- 0
  for(row in 1:nrow(arguments)){
    result[row] <- fn(arguments[row,])
  }

  return <- result
}

generateShiftedAndRotatedAckley <- function(arguments){
  library(cec2013)
  result <- 0
  for(row in 1:nrow(area)) {
    result[row] <- cec2013(10, arguments[row,])
  }
  
  return <- result
}
