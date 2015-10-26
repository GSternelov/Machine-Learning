
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
table(conf_mat)
table(conf_mat[,1], conf_mat[,2])


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

# Computing sensitivity and specificity
# Adding true labels to kknn and knearest matrices
classify <- data.frame(classify)
classify$Spam <- test[,49]
Knear_class <- data.frame(Knear_class)
Knear_class$Spam <- test[,49]

sensitivity_kknn <- 0
specificity_kknn <- 0
sensitivity_knearest <- 0
specificity_knearest <- 0
for (i in 1:19){
  table_kknn <- table(classify[,20], classify[,i])
  sensitivity_kknn[i] <- table_kknn[2,2] /sum(table_kknn[,2])
  specificity_kknn[i] <- table_kknn[1,1] /sum(table_kknn[,1])
  table_knearest <- table(Knear_class[,20], Knear_class[,i])
  sensitivity_knearest[i] <- table_knearest[2,2] /sum(table_knearest[,2])
  specificity_knearest[i] <- table_knearest[1,1] /sum(table_knearest[,1])
}
kknn_ROC <- data.frame(cbind(1 - specificity_kknn, sensitivity_kknn))
knearest_ROC <- data.frame(cbind(1 - specificity_knearest, sensitivity_knearest))

plot(kknn_ROC, type="l", col="blue", xlim=c(0.02,0.2), ylim=c(0.7,0.97),
     xlab="1-Specificity (false positive rate)", ylab="Sensitivity (True positive rate)",
     main="ROC curves for kknn and knearest functions")
lines(knearest_ROC, type="l", col="red")
legend(0.15,0.9,c("kknn","knearest"),
lty=c(1,1),
lwd=c(2.5,2.5),col=c("blue","red"))


# Assignment 2
machine <- read.csv("machines.csv", sep=";", header = TRUE)

# 2.2
n <- length(machine[,1])
theta <- seq(0.01, 5, by=0.01)

loglike_Theta <- n * log(theta) - theta *sum(machine[,1])
plot(loglike_Theta, type="l")

theta[which(loglike_Theta==max(loglike_Theta))]

# Zoomed in
theta_zoom <- seq(1.08, 1.18, by=0.01)

loglike_Theta_zoom <- n * log(theta_zoom) - theta_zoom *sum(machine[,1])
plot(loglike_Theta_zoom, type="l")

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
theta <- seq(0.6, 1.8, by=0.01)
x <- machine[,1]

# Is this right?
l_theta <- theta^n * exp(-theta*sum(x)) * (0.5*exp(-0.5*theta))
plot(l_theta, type="l")
theta[which(l_theta==max(l_theta))]


# 2.5
set.seed(121989)
R_exp <-rexp(50, 1.13)
par(mfrow=c(1,2))
hist(machine[,1])
hist(R_exp, breaks=8)
par(mfrow=c(1,1))
