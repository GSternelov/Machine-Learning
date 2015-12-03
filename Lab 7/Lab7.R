# Assignment 1
# 1.2
PiecewiseCBS <- function(data, k){
  y <- matrix(0, ncol=1, nrow=length(data[,1]))
  for (j in 1:length(data[,1])){
    if(data[j,1] < k[1]){
      y[j] <-  mean(subset(data, data[,1] < k[1])[,2])
    }else{y[j] <- y[j]  }
  }
  
  for (i in 2:length(k)){
    for (j in 1:length(data[,1])){
      
      if(k[i] > k[1] & data[j,1] < k[i] & data[j,1] > k[i-1] ){
        y[j] <- mean(subset(data, data[,1] >= k[i-1] & data$x < k[i])[,2])
      }else{y[j] <- y[j]  }
      
      if(data[j,1] >= k[length(k)]){
        y[j] <- mean(subset(data, data[,1] > k[i])[,2])
      }else{y[j] <- y[j]  }}}
  
  YhY <- cbind(data[,2], y, data[,1])
  YhY <- YhY[order(YhY[,3]),] 
  
  plot(x=cube$x, y=cube$y, pch=21, bg="darkorange")
  points(x=YhY[,3], y=YhY[,2], col="seagreen", type="l", lwd=3)
}
cube <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 7/cube.csv", sep=";", header=TRUE)

testData <- data.frame(x=cube$x, y =cube$y)
PiecewiseCBS(data = testData, k = c(3,6))
influenza <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 7/influenza.csv", sep=";", header=TRUE)
# 2.1
library(ggplot2)
library(gridExtra)
# time versus mortality
Mort <- ggplot(influenza, aes(x=Time, y=Mortality),) + geom_point() +
  theme(axis.text.x = element_blank()) + labs(x = "Time (weekly data, 1995-2003)")+ ggtitle("Time vs Mortality")
# time versus influenza
Infl <- ggplot(influenza, aes(x=Time, y=Influenza),) + geom_point() +
  theme(axis.text.x = element_blank()) + labs(x = "Time (weekly data, 1995-2003)")+ ggtitle("Time vs Influenza")
grid.arrange(Mort, Infl, ncol=2)
library(mgcv)
model2_2 <- gam(formula=Mortality ~ Year + s(Week, k=51), data=influenza)
# Evaluate the fit
plot(x=as.numeric(influenza$Time), y=influenza$Mortality, pch=21, bg="red")
points(x=influenza$Time, y=model2_2$fitted.values, col="blue", pch=21, bg="blue")

# Investigate output
summary(model2_2)

# Visualize the spline component
plot(model2_2)
# How penalty factor(k) affects the devaince. 
# Compares for two different values of K (2, 30)
model2_4v1 <- gam(formula=Mortality ~ Year + s(Week, k=3), data=influenza)
model2_4v2 <- gam(formula=Mortality ~ Year + s(Week, k=52), data=influenza)
# Compares fitted and original values for each case
par(mfrow=c(2,1))
# v1
plot(x=as.numeric(influenza$Time), y=influenza$Mortality, pch=21, bg="red", main="k=3")
points(x=influenza$Time, y=model2_4v1$fitted.values, col="blue", pch=21, bg="blue")
# v2
plot(x=as.numeric(influenza$Time), y=influenza$Mortality, pch=21, bg="red", main = "k=52")
points(x=influenza$Time, y=model2_4v2$fitted.values, col="blue", pch=21, bg="blue")
par(mfrow=c(1,1))

# 2.5
# Residuals and influenza values plotted against time
plot(x=as.numeric(influenza$Time), y=model2_2$residuals, pch=21, bg="blue")
points(x=influenza$Time, y=influenza$Influenza, col="red", pch=21, bg="red")
# 2.6
# Mortality described as spline functions of year, week and influenza. 
model2_6 <- gam(formula=Mortality ~ s(Week, k=52) + s(Year, k=9) + s(Influenza, k=85), data=influenza)
# Use output to test whether or not mortality is influenced by influenza. 
summary(model2_6)
# plot fitted against original values
plot(x=as.numeric(influenza$Time), y=influenza$Mortality, pch=21, bg="red")
points(x=influenza$Time, y=model2_6$fitted.values, col="blue", pch=21, bg="blue")
# compute SSE
SSE2_6 <- sum((influenza$Mortality-model2_6$fitted.values)^2)
SSE2_6
## 
