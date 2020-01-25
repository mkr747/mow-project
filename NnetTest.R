library("ggplot2")
library("nnet")

curve(sin(x^2 + 2) * cos(x-1), -4, 4)
funct1 <- function(x) sin(x^2 + 2) *cos(x-1)
set.seed(42)
x <- runif(10000, min = -10, max = 10) 
y <- funct1(x) 
p <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) + stat_function(fun = funct1)

nn <- nnet(x, y, size=40, maxit=1000, linout=TRUE, skip=TRUE)
grid <- seq(-4, 4, by=0.1)
lines(grid, predict(nn, data.frame(x=grid)), col="blue")

plot.nnet()

detach("package:ggplot2")

#nnet function from nnet package
library(nnet)
set.seed(seed.val)
mod1<-nnet(rand.vars,resp,data=dat.in,size=10,linout=T)

#neuralnet function from neuralnet package, notice use of only one response
library(neuralnet)
form.in<-as.formula('Y1~X1+X2+X3+X4+X5+X6+X7+X8')
set.seed(seed.val)
mod2<-neuralnet(form.in,data=dat.in,hidden=10)

#mlp function from RSNNS package
library(RSNNS)
set.seed(seed.val)
mod3<-mlp(rand.vars, resp, size=10,linOut=T)