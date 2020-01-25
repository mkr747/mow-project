library(ggplot2)
funct1 <- function(x) sin(x^2 + 2) *cos(x-1)
p <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) + stat_function(fun = funct1)
plot(x, y)


set.seed(42)
x <- runif(100, min = -4, max = 4) # uniform distribution 100 samples

y <- funct1(x) 


#curve(sin(x^2 + 2) * cos(x-1), -4, 4,add=TRUE)

library(nnet)

  nn <- nnet(x, y, size=20, maxit=500, linout=TRUE)
  
  grid <- seq(-4, 4, by=0.1)
  lines(grid, predict(nn, data.frame(x=grid)), col="blue")
  

library(cec2013)
#Generacja probek -> tylko w jakim zakresie do funkcji celu?
#PROBKI
set.seed(42)
#x <- runif(100, min = -4, max = 4) # uniform distribution 100 samples
#Wektor dla cec2013
x <- runif(50, min=-10, max=10)

y <- cec2013(10,x)

#POLYNOM DLA SMOOF
install.packages('polynom')
library(smoof)
library(polynom)
set.seed(42)
func <- function(x1,x2) 0.25*x1^4 - 0.5*x1^2 + 0.1*x1 + 0.5*x2^2 #Aluffi-Pentini function
x1 <- runif(50, min=-10, max=10) #zakres obydwu zmiennych
x2 <- runif(50, min=-10, max=10)
y <- func(x1, x2) # wartości dla danych zmiennych
pred <- lm(y ~ poly(x1,x2, degree = 6, raw=TRUE)) # tu trzeba sporzadzic dataset y x1 x2
intervals <- predict(pred, data.frame(x1=x1,x2=x2), interval='confidence', level = 0.9)
Ynominal <- data.frame(y=y)

library(cec2013)
#inputCec <- cbind(c())
y1 <- cec2013(6,x1)
pred1 <- lm(y~poly(x1, degree=10, raw =TRUE))
intervals <- predict(pred1, data.frame(x1=x1), interval='confidence', level = 0.9)




  
  
  

