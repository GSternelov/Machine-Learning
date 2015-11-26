mrt_rate <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 5/mortality_rate.csv", sep=";")

library(ggplot2)
mrt_rate$LMR <- log(mrt_rate$Rate)
ggplot(mrt_rate, aes(y=LMR, x=Day)) + geom_point()
# 1.2
NadWat <- function(X, Y, Xtest, lambda){
  # Wants to go through all x for every Xtest
  # Compute the value for every run and sum the 136 values
  # Do this for every value in xtest
  NdaWat <- 0
  K <- 0
  h <- 0
  for (i in 1:length(Xtest)){ 
    for (j in 1:length(X)){
     if(abs(X[j]-Xtest[i]) < lambda ){
       K[j] <- 3/4 * (1 - (abs(X[j] - Xtest[i]) / lambda)^2)
     }else{
       K[j] <- 0
     }
    }
    h <- h+1
   NdaWat[h] <- sum(K*Y) / sum(K)
  }
  return(NdaWat)
}
# a) A very smooth curve
tryA <- NadWat(mrt_rate$Day, mrt_rate$LMR, mrt_rate$Day, 150)
plot(mrt_rate$Day, mrt_rate$LMR, pch=21, bg="orange")
points(tryA, x=mrt_rate$Day, type="l", lwd=5)
MSEA <- sum((mrt_rate$LMR - tryA)^2 / 136)
# b) A wiggly curve
tryB <- NadWat(mrt_rate$Day, mrt_rate$LMR, mrt_rate$Day,3)
plot(mrt_rate$Day, mrt_rate$LMR, pch=21, bg="orange")
points(tryB, x=mrt_rate$Day, type="l", lwd=5)
MSEB <- sum((mrt_rate$LMR - tryB)^2 / 136)
# c) A good fit
tryC <- NadWat(mrt_rate$Day, mrt_rate$LMR, mrt_rate$Day,10)
plot(mrt_rate$Day, mrt_rate$LMR, pch=21, bg="orange")
points(tryC, x=mrt_rate$Day, type="l", lwd=5)
MSEC <- sum((mrt_rate$LMR - tryC)^2 / 136)
library(kernlab)
set.seed(12345)
epsi <- seq(0.1, 3, 0.2)
j <- 1
MSE_SVM <- 0
for(i in epsi){
  LMR_SVM <- ksvm(LMR ~ Day, mrt_rate, type="eps-svr", 
                  kernel="rbfdot", epsilon=i, scaled=FALSE)
  MSE_SVM[j] <- sum(((LMR_SVM@fitted - mrt_rate$LMR)^2)/ 136)
  j <- j+1
}

plot(epsi, MSE_SVM, type="b", pch=21, bg="orange")


eps_mod <- seq(0.1, 3, 0.2)
eps_mod <- eps_mod[c(1,6,12)]
eps_SVM <- matrix(ncol=3, nrow=136)
j <- 0
set.seed(12345)
for (i in eps_mod){
  j <- j+1
  eps_SVMR <- ksvm(LMR ~ Day, mrt_rate, type="eps-svr", 
                  kernel="rbfdot", epsilon=eps_mod[i], scaled=FALSE)  
  eps_SVM[,j] <- eps_SVMR@fitted[,1]
}
plot(mrt_rate$Day, mrt_rate$LMR, pch=21, bg="orange", ylim=c(-6.5, 1))
points(eps_SVM[,1], x=mrt_rate$Day, type="l", lwd=3, col="blue")
points(eps_SVM[,2], x=mrt_rate$Day, type="l", lwd=3, col="brown")
points(eps_SVM[,3], x=mrt_rate$Day, type="l", lwd=3, col="purple")
legend(120,-4,c("epsilon=0.1","epsilon=1.1", "epsilon=2.3"), lty=c(1,1), 
       lwd=c(5,5),col=c("blue","brown", "purple"),  cex=0.65)
# Compares original and fitted values, epsilion =0.1
set.seed(12345)
LMR_SVM <- ksvm(LMR ~ Day, mrt_rate, type="eps-svr", 
                kernel="rbfdot", epsilon=0.1, scaled=FALSE)

MSE_SVM <- sum(((LMR_SVM@fitted - mrt_rate$LMR)^2)/ 136)
plot(mrt_rate$Day, mrt_rate$LMR, pch=21, bg="orange")
points(tryA, x=mrt_rate$Day, type="l", lwd=3, col="blue")
points(tryB, x=mrt_rate$Day, type="l", lwd=3, col="brown")
points(tryC, x=mrt_rate$Day, type="l", lwd=3, col="purple")
points(LMR_SVM@fitted[,1], x=mrt_rate$Day, type="l", lwd=3, col="seagreen")
legend(120,-4,c("lambda=150","lambda=3", "lambda=10", "epsilon=0.1"), lty=c(1,1), 
       lwd=c(5,5),col=c("blue","brown", "purple", "seagreen"),  cex=0.65) 
library(fANCOVA)
loessLMR <- loess.as(mrt_rate$Day, mrt_rate$LMR, 1, family = "gaussian", plot=FALSE, 
                     criterion="gcv")
predLoess <- predict(loessLMR, se=TRUE)
upper <- predLoess$fit + predLoess$se.fit * 2
lower <- predLoess$fit - predLoess$se.fit * 2
plot(loessLMR, type="l")
points(loessLMR$x, upper, type="l", col="orange")
points(loessLMR$x, lower, type="l" ,col="orange")
olive <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 5/olive.csv", sep=",")
library(kernlab)
olive$R2 <- 0
for (i in 1:572){
  if(olive$Region[i] == 2){
    olive$R2[i] = 1
  }else{
    olive$R2[i] = 0
  }
}
ggplot(olive, aes(x=linoleic, y=oleic)) + geom_point(aes(col=R2))

set.seed(12345)
linearSVM <- ksvm(R2 ~ oleic+linoleic, olive, type="C-svc", kernel="vanilladot")
plot(linearSVM, data=olive)
set.seed(12345)
rbfSVM <- ksvm(R2 ~ oleic+linoleic, olive, type="C-svc", 
               kernel="rbfdot")
plot(rbfSVM, data=olive)
set.seed(12345)
rbf_penSVM <- ksvm(R2 ~ oleic+linoleic, olive, type="C-svc", 
               kernel="rbfdot", C=100)
plot(rbf_penSVM, data=olive)
set.seed(12345)
rbf_bwitdhSVM <- ksvm(R2 ~ oleic+linoleic, olive, type="C-svc", 
                   kernel="rbfdot", kpar=list(sigma=10))
plot(rbf_bwitdhSVM, data=olive)
set.seed(12345)
olive_acid <- olive[, c(2, 4:11)]
rbf_spocSVM <- ksvm(Region ~. , olive_acid, type="spoc-svc", 
                      kernel="vanilladot", cross=10)
## 
