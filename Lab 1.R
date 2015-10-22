
# Assignment 1
dat <- read.csv("spambase.csv", sep=";", header = TRUE)
dat <- as.matrix(dat)

n=dim(dat)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=dat[id,]
test=dat[-id,]

# 2
x <- as.matrix(train[,1:48])
y <- as.matrix(test[,1:48])

x_transformed <- t(apply(x, 1, function(x) x / sqrt(sum(x^2))))
x_transformed[is.na(x_transformed)] <- 0

y_transformed <- t(apply(y, 1, function(y) y / sqrt(sum(y^2))))
y_transformed[is.na(y_transformed)] <- 0

numerator <- (x_transformed) %*% t(y_transformed)
d_xy <- data.frame(1 - numerator)


knearest <- function(data, K,p=0.5 ,newdata){
  Predict <- 0
  ProbOne <- 0
  ProbZero <- 0
  
  x <- as.matrix(data[,1:48])
  y <- as.matrix(newdata[,1:48])
  x_transformed <- t(apply(x, 1, function(x) x / sqrt(sum(x^2))))
  x_transformed[is.na(x_transformed)] <- 0
  y_transformed <- t(apply(y, 1, function(y) y / sqrt(sum(y^2))))
  y_transformed[is.na(y_transformed)] <- 0
  
  numerator <- (x_transformed) %*% t(y_transformed)
  d_xy <- data.frame(1 - numerator)
  
  for(i in 1:length(d_xy)){
    LabelsTrain <- data[order(d_xy[,i]), 49][1:K]
    ProbOne[i] <- mean(LabelsTrain)
    ProbZero[i] <- 1-ProbOne[i]
    if(ProbOne[i] > p){
      Predict[i] = 1
    }else{
      Predict[i] = 0
    }
  }
  newdata <- data.frame(newdata)
  newdata$predict <- Predict
  newdata$prob_one <- ProbOne
  newdata$prob_zero <- ProbZero
  return(newdata)
}

testish <- knearest(train, 1, 0.5, test)
table(testish$predict, testish$Spam)

testish2 <- knearest(train, 1, 0.5, train)
table(testish2$predict, testish2$Spam)

# Compares with package kknn
library(kknn)

test_kknn <- kknn(Spam~., train, test, k=5)
fitted_v <- test_kknn$fitted.values


for (i in 1:length(fitted_v)){
  if(fitted_v[i] > 0.5 ){
    fitted_v[i] = 1
  }else{
    fitted_v[i] = 0
  }
}
conf_mat <- data.frame(cbind(fitted_v, test[,49]))
table(conf_mat[,2], conf_mat[,1])

# 6
# kknn wfor different decision boundaries
train <- data.frame(train)
test <- data.frame(test)
test_kknn <- kknn(Spam~., train, test, k=5)
fitted_v <- test_kknn$fitted.values
phis <- seq(0.05, 0.95, by=0.05)
h <- 0
classify <- matrix(ncol=19, nrow=2301)
predic <- 0

for (j in phis){
  for (i in 1:length(fitted_v)){
    if(fitted_v[i] > j ){
      predic[i] = 1
    }else{
      predic[i] = 0
    }
  }
  h <- h+1
  classify[,h] <- predic
}

# For knearest()
Knear_class <- matrix(ncol=19, nrow=2301)
h <- 0
for (j in phis){
  testish <- knearest(train, 5, p=j, test)
  h <- h+1
  Knear_class[,h] <- testish$predict
}


# Assignment 2
machine <- read.csv("machines.csv", sep=";", header = TRUE)

# 2.2
n <- length(machine[,1])
theta <- seq(0.01, 5, by=0.01)

loglike_Theta <- n * log(theta) - theta *sum(machine[,1])
plot(loglike_Theta, type="l")

theta[which(loglike_Theta==max(loglike_Theta))]

# 3
# Choose the first 6 observations from data machine
machine_six <- data.frame(machine[1:6, 1])
n_six <- length(machine_six[,1])

theta <- seq(0.01, 5, by=0.01)

loglike_ThetaSix <- n_six * log(theta) + -theta *sum(machine_six[,1])

plot(loglike_ThetaSix, type="l")
theta[which(loglike_ThetaSix==max(loglike_ThetaSix))]

par(mfrow=c(1,2))
plot(loglike_Theta, type="l")
plot(loglike_ThetaSix, type="l")
par(mfrow=c(1,1))
# 4

lambda <- 0.5
theta <- seq(0.01, 3, by=0.01)
x <- machine[,1]

#l_theta <- log(lambda) + log(theta) - theta*(x+lambda)
l_theta <- n*log(lambda) + n*log(theta) - n*theta - theta * sum(x)
plot(l_theta, type="l")
theta[which(l_theta==max(l_theta))]


# 2.5
R_exp <-rexp(50, 0.53)

par(mfrow=c(1,2))
hist(machine[,1])
hist(R_exp)
par(mfrow=c(1,1))
