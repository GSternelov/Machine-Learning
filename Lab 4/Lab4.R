
# Assignment 1

state <- read.csv("Lab 4/State.csv", sep=";")

# 1.1
library(ggplot2)
plot(state$MET, state$EX)
ggplot(state, aes(x=MET, y=EX)) + geom_point()

# What kind of model? Not sure really. The observations does not follow any real pattern, almost 
# look random. 

# 1.2
# Fits a regression tree
# second try
set.seed(12345)
library(tree)
fit2 <- tree(EX ~ MET, data=state, control=tree.control(nobs=48, minsize=2) )
plot(fit2)
text(fit2)

cv_fit <- cv.tree(fit2)
plot(cv_fit$size, cv_fit$dev, type="b")

pruneFit2 <- prune.tree(fit2, best=3)
cv_pred <- predict(pruneFit2)

plot(state$MET, state$EX, col="blue")
points(state$MET, cv_pred, col="red")
fit2_resid <- state$EX - cv_pred
hist(fit2_resid, breaks=14)   
     

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

e=envelope(res) 
fit=tree(EX ~ MET, data=data2, control=tree.control(nobs=48, minsize=2)) 
priceP=predict(fit)

plot(state$MET, state$EX, pch=21, bg="purple", col="turquoise")
points(data2$MET,priceP,type="l", col="blue") #plot fitted line
#plot cofidence bands
points(data2$MET,e$point[2,], type="l", col="red")
points(data2$MET,e$point[1,], type="l", col="red")

# 1.4
# Parametric bootstrap
# 95 % confidence and prediction bands
# Assumes that Y follows the normal distribution





     
     