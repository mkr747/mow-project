library(cec2013)
library(smoof)
library(globalOptTests)

#x <- c(2,3,4,5,6)
#x <- -2 :2 
#x <- matrix(-10000:10000,-20000:0,0:0,nrow=20001, ncol=5)

#toDraw <- smoof::makeBohachevskyN1Function(1)
#toDraw <- cec2013::cec2013(2, x)
toDraw <- globalOptTests::getGlobalOpt("GoldPrice")
#x <- toDraw[["lower"]]
#y <- toDraw[["upper"]]
c("Rosenbrock")
plot(toDraw)

detach("package:cec2013")
