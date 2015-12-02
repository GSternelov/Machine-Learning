
### Assignment 1



### Assignment 2
## 2.1
FruitFly <- read.csv2("C:/Users/Gustav/Documents/Machine-Learning/Lab 8/mortality.csv", sep=";")
FruitFlySz <- data.frame(scale(FruitFly, scale = TRUE, center=TRUE))

## 2.2
library(ggplot2)
qplot(y=LMR,x=Day, data=FruitFlySz, geom = "line", xlab="Day",ylab="LMR") 

## 2.3
library(neuralnet)
set.seed(7235)
network2_3 <- neuralnet(LMR ~ Day, data = FruitFlySz, hidden = 1, act.fct =
            "tanh", stepmax = 1e6, threshold = 0.1)
# a)
plot(network2_3)

# b)
plot()
points(unlist(network2_3$net.result), col="blue")


