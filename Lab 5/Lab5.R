
# Assignment 1
mrt_rate <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 5/mortality_rate.csv", sep=";")

# 1.1
# The logarithm of rate
mrt_rate$LMR <- log(mrt_rate$Rate)
library(ggplot2)
ggplot(mrt_rate, aes(y=LMR, x=Day)) + geom_point()

# 1.2

NadWat <- function(X, Y, Xtest, lambda){
  
}


# 1.3



# 1.4
library(kernlab)
set.seed(12345)
epsi <- seq(0.1, 3.5, 0.2)
j <- 1
MSE_SVM <- 0
for(i in epsi){
  LMR_SVM <- ksvm(LMR ~ Day, mrt_rate, type="eps-svr", 
                  kernel="rbfdot", epsilon=i)
  MSE_SVM[j] <- sum(((LMR_SVM@fitted - mrt_rate$LMR)^2)/ 136)
  j <- j+1
}

plot(epsi, MSE_SVM, type="b", pch=21, bg="orange")
epsi[12]
# Compares original and fitted values, epsilion =2.3
LMR_SVM <- ksvm(LMR ~ Day, mrt_rate, type="eps-svr", 
                kernel="rbfdot", epsilon=2.3)

fittedV <- data.frame(LMR=LMR_SVM@fitted)
originalV <- data.frame(LMR=mrt_rate$LMR)
ValuSVM <- cbind(rbind(originalV, fittedV),Type=c(rep("OrigV", 136), rep("FittedV", 136)), Day=mrt_rate$Day)
ggplot(ValuSVM, aes(y=LMR, x=Day)) + geom_point(aes(col=Type))


# 1.5
library(fANCOVA)

loess.as



{# Assignment 2
olive <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 5/olive.csv", sep=",")
# 2.1
# construct variable R2 which is equal to 1 if the oil comes from region 2 and 0 otherwise.
olive$R2 <- 0

for (i in 1:572){
  if(olive$Region[i] == 2){
    olive$R2[i] = 1
  }else{
    olive$R2[i] = 0
  }
}
library(ggplot2)
ggplot(olive, aes(x=linoleic, y=oleic)) + geom_point(aes(col=R2))
# By just looking at the plot it is easy to identify the oils from region 2,
# it may be harder to find a suitable model. 

# 2.2
library(kernlab)
set.seed(12345)
# a)
linearSVM <- ksvm(R2 ~ oleic+linoleic, olive, type="C-svc", 
                  kernel="vanilladot")
plot(linearSVM, data=olive)


# b)
rbfSVM <- ksvm(R2 ~ oleic+linoleic, olive, type="C-svc", 
               kernel="rbfdot")
plot(rbfSVM, data=olive)


# c)
rbf_penSVM <- ksvm(R2 ~ oleic+linoleic, olive, type="C-svc", 
               kernel="rbfdot", C=100)
plot(rbf_penSVM, data=olive)


# d)
rbf_bwitdhSVM <- ksvm(R2 ~ oleic+linoleic, olive, type="C-svc", 
                   kernel="rbfdot", kpar=list(sigma=10))
plot(rbf_bwitdhSVM, data=olive)



# 3
olive_acid <- olive[, c(2, 4:11)]
rbf_spocSVM <- ksvm(Region ~. , olive_acid, type="spoc-svc", 
                      kernel="vanilladot", cross=10)

rbf_spocSVM@nSV

rbf_spocSVM@error
rbf_spocSVM@fitted
10/ sum(table(rbf_spocSVM@fitted, olive_acid$Region))

}