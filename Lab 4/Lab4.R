state <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 4/State.csv", sep=";")
library(ggplot2)
ggplot(state, aes(x=MET, y=EX)) + geom_point()
library(tree)
set.seed(12345)
fit2 <- tree(EX ~ MET, data=state, control=tree.control(nobs=48, minsize=2))
cv_fit <- cv.tree(fit2)
plot(cv_fit$size, cv_fit$dev, type="b")
pruneFit2 <- prune.tree(fit2, best=3)
plot(pruneFit2)
text(pruneFit2, pretty=0)
cv_pred <- predict(pruneFit2)
fit2_resid <- state$EX - cv_pred
myhist <- hist(fit2_resid, breaks=14) 
par(mfrow=c(1,2))
plot(state$MET, state$EX, col="blue")
points(state$MET, cv_pred, col="red")
 
multiplier <- myhist$counts / myhist$density
mydensity <- density(fit2_resid)
mydensity$y <- mydensity$y * multiplier[1]

plot(myhist)
lines(mydensity)  
par(mfrow=c(1,1))
# 1.3 
# Non-parametric bootstrap
# 95 % confidence bands
library(boot)
data2=state[order(state$MET),]#reordering data according to MET
# computing bootstrap samples
f=function(data, ind){
  data1=data[ind,]# extract bootstrap sample
  #fit regression tree
  res=tree(EX ~ MET, data=data1, control=tree.control(nobs=48, minsize=2)) 
  #predict values for all Area values from the original data
  priceP=predict(res,newdata=data2)
  return(priceP)
}
res=boot(data2, f, R=1000) #make bootstrap

# Create lower and upper bound. 
e=envelope(res) 

fit=prune.tree(fit2, best=3) 
priceP=predict(fit)

plot(state$MET, state$EX, pch=21, bg="black", col="red")
points(data2$MET,priceP,type="b", col="blue") #plot fitted line
#plot cofidence bands
points(data2$MET,e$point[2,], type="l", col="red", lwd=2)
points(data2$MET,e$point[1,], type="l", col="red", lwd=2)

# Parametric bootstrap
# 95 % confidence and prediction bands
# Assumes that Y follows the normal distribution
mle=prune.tree(fit2, best=3)
rng=function(data, mle) {
  data1=data.frame(EX=data$EX,
                   MET=data$MET, data=data)
  n=length(data$EX)
  data1$EX=rnorm(n,predict(mle,newdata=data1),sd(mle$y))
  return(data1)
}
f1=function(data1){
  res=tree(EX ~ MET, data=data1, control=tree.control(nobs=48, minsize=2)) 
  predictedP=predict(res, newdata=data2)
  return(predictedP)
}
res=boot(data2, statistic=f1, R=1000, mle=mle, ran.gen=rng , sim="parametric") 
e2=envelope(res) 
fit=prune.tree(fit2, best=3) 
pred2=predict(fit)
plot(state$MET, state$EX, pch=21, bg="black", col="red", ylim=c(100, 550))
points(data2$MET,pred2,type="b", col="blue") #plot fitted line
#plot cofidence bands
points(data2$MET,e2$point[2,], type="l", col="red", lwd=2)
points(data2$MET,e2$point[1,], type="l", col="red", lwd=2)
spectra <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 4/NIRSpectra.csv", sep=";")
data_a <- spectra
data_a$Viscosity=c()
data_a$ID=c()
res=prcomp(data_a)
lambda=res$sdev^2
#proportion of variation explained by each feature
plot(sprintf("%2.3f",lambda/sum(lambda)*100), ylab="Variation explained (%)")
# Scores in coordinates of PC1 and PC2
plot(res$x[,1], res$x[,2], xlab="PC1", ylab="PC2")
U=res$rotation
plot(U[,1], main="Traceplot, PC1")
plot(U[,2],main="Traceplot, PC2")

library(fastICA)

set.seed(12345)
a <- fastICA(data_a, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE) #ICA

W_mat <- a$W
W_mat
KW_mat <- a$K %*% a$W
# plot the columns as trace plots
plot(KW_mat[,1], main="Traceplot, column1")
plot(KW_mat[,2],main="Traceplot, column2")
# The score plot
plot(a$S[,1], a$S[,2])
dat <- as.matrix(spectra)
n=dim(dat)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=dat[id,]
test=dat[-id,]

library(pls)
train <- data.frame(train[, 2:128])
test <- data.frame(test[, 2:128])
set.seed(12345)
pcr.fit=pcr(Viscosity~., data=train, validation="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.fit1=pcr(Viscosity~., 25,data=train, validation="none")
pcr.pred <- predict(pcr.fit1, newdata=test, ncomp = 25)
pcr.mse <- 1/length(test[,1]) * sum((test$Viscosity - pcr.pred)^2, na.rm=TRUE)
pcr.mse
set.seed(12345)
plsr.fit=plsr(Viscosity~., data=train, validation="CV")
validationplot(plsr.fit,val.type="MSEP")
plsr.fit1=plsr(Viscosity~., 12,data=train, validation="none")
plsr.pred <- predict(plsr.fit1, newdata=test, ncomp = 12)
plsr.mse <- 1/length(test[,1]) * sum((test$Viscosity - plsr.pred)^2, na.rm=TRUE)
## 
