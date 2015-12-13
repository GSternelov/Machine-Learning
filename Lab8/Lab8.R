
### Assignment 1
## 1.1
# "lambda" is the learning rate, "until" the parameter for the stop condition
spl <- function(x, y, lambda, weights, until){
  yHat <- 0
  MisClass1 <- 0
  MisClass2 <- 1
  # Loop that runs until stopping condition is met. 
  while(abs(MisClass2 - MisClass1) > until){
    for(i in 1:nrow(x)){
      # The weights are multiplied with i:th row
      linComb <- sum(weights * x[i, ])
      # Uses the sign function to classify the observation
      yHat[i] <- sign(linComb)
      # Updates the weights
      weights <- weights + lambda*(y[i] - yHat[i]) * x[i, ]
    }
    # Calculates the misclassification value
    # The misclass value for the latest iteration is saved for comparison with
    # the next misclass value
    MisClass2 <- MisClass1
    confMat <- table(yHat, y)
    MisClass1 <- 1 - sum(diag(confMat)) / sum(confMat) 
  }
  RetU <- list(Wt = weights, miscTr = MisClass1)
  return(RetU)
}


## 1.2
Spambase <- read.csv2("C:/Users/Gustav/Documents/Machine-Learning/Lab8/spambaseshort.csv", sep=";")
n=dim(Spambase)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.7))
train=Spambase[id,]
test=Spambase[-id,]

## 1.3
inputs <- data.frame(scale(train[, 1:57], center = TRUE, scale = TRUE))
inputs <- data.frame(X=rep(1, 322), inputs)
testInput <- data.frame(scale(test[, 1:57], center = TRUE, scale = TRUE))
testInput <- data.frame(X=rep(1, 138), testInput)

set.seed(7235)
init_weights <- rnorm(n = ncol(train))

testSPL <- spl(x=inputs, y=train[,58], lambda=0.1, weights=init_weights, until=0.01)

# The classify the test set with the obtained model the following code is used
yHatTest <- 0
for(i in 1:nrow(testInput)){
  # Uses the weights returned by the function on every row 
  linCombTest <- sum(testSPL$Wt * testInput[i,])
  yHatTest[i] <- sign(linCombTest)
}
confMatTest <- table(yHatTest, test$Spam)
MisClass3 <- 1 - sum(diag(confMatTest)) / sum(confMatTest) 
MisClass3

## 1.4



## 1.5
set.seed(846)
init_weightsv2 <- rnorm(n = ncol(train))
testSPLv2 <- spl(x=inputs, y=train[,58], lambda=0.1, weights=init_weightsv2, until=0.01)
yHatTest <- 0
for(i in 1:nrow(testInput)){
  linCombTest <- sum(testSPLv2$Wt * testInput[i,])
  yHatTest[i] <- sign(linCombTest)
}
confMatTest <- table(yHatTest, test$Spam)
MisClass4 <- 1 - sum(diag(confMatTest)) / sum(confMatTest) 
testSPLv2$miscTr
MisClass4

## 1.6
train[train==-1] <- 0
test[test==-1] <- 0

logi <- glm(Spam~.,data=train, family=binomial())
length(predict(logi, newdata=test))
pred_logi <- data.frame(test$Spam, fitted=predict(logi, newdata=test, type="response"))
for(i in 1:length(pred_logi[,1])){
  if(pred_logi[i, 2] > 0.5){
    pred_logi[i, 2] = 1
  }else{
    pred_logi[i, 2] = 0
  }
}
confMat <- table(pred_logi)
MisClass <- 1 - sum(diag(confMat)) / sum(confMat)


### Assignment 2
## 2.1
FruitFly <- read.csv2("C:/Users/Gustav/Documents/Machine-Learning/Lab8/mortality.csv", sep=";")
DaySz <- data.frame(Day =scale(FruitFly$Day, scale = TRUE, center=TRUE))
FruitFlySz <- data.frame(LMR = FruitFly$LMR, Day = DaySz)

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
plot(y=FruitFlySz$LMR, x=FruitFlySz$Day, type="l", lwd=2)
points(x=FruitFlySz$Day, unlist(network2_3$net.result), col="darkorange", type="l",
       lwd=2)

## 2.4
set.seed(7235)
network2_4 <- neuralnet(LMR ~ Day, data = FruitFlySz, hidden = 2, act.fct =
                          "tanh", stepmax = 1e6, threshold = 0.1)
# a)
plot(network2_4)
# b)
plot(y=FruitFlySz$LMR, x=FruitFlySz$Day, type="l", lwd=2)
points(x=FruitFlySz$Day, unlist(network2_4$net.result), col="darkorange", type="l",
       lwd=2)

## 2.5
# Four hidden neurons
set.seed(7235)
network2_5a <- neuralnet(LMR ~ Day, data = FruitFlySz, hidden = 4, act.fct =
                          "tanh", stepmax = 1e6, threshold = 0.1)
plot(network2_5a)
plot(y=FruitFlySz$LMR, x=FruitFlySz$Day, type="l", lwd=2)
points(x=FruitFlySz$Day, unlist(network2_5a$net.result), col="darkorange", type="l",
       lwd=2)
# Five hidden neurons
set.seed(7235)
network2_5b <- neuralnet(LMR ~ Day, data = FruitFlySz, hidden = 5, act.fct =
                           "tanh", stepmax = 1e6, threshold = 0.1)
plot(network2_5b)
plot(y=FruitFlySz$LMR, x=FruitFlySz$Day, type="l", lwd=2)
points(x=FruitFlySz$Day, unlist(network2_5b$net.result), col="darkorange", type="l",
       lwd=2)



