aussieCrab <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 3/australian-crabs.csv", sep=",", header = TRUE)
library(ggplot2)
ggplot(aussieCrab, aes(y=CL, x=RW)) + geom_point(aes(color=sex), size=3)

# Creates separte data sets for males and females
male_data <- subset(aussieCrab, sex=="Male")[5:6]
female_data <- subset(aussieCrab, sex=="Female")[5:6]

# Calculate the means and covariances
male_vec <- c(mean(male_data[,1]), mean(male_data[,2]))
female_vec <- c(mean(female_data[,1]), mean(female_data[,2]))

male_cov <- cov(male_data)
female_cov <- cov(female_data)
covHat <- matrix(100/200 * male_cov + 100/200 * female_cov, ncol=2)

# prop priors
prior_male <- 100/200
prior_female <- 100/200

# The discriminant function

W_male <- (as.matrix(aussieCrab[, 5:6]))%*% solve(covHat) %*% as.matrix(male_vec) 
Wo_male <-  (0.5 *  t(as.matrix(male_vec)) %*% solve(covHat) %*% as.matrix(male_vec) +
  log(prior_male))
disc_male <- W_male - as.vector(Wo_male)

W_female <- (as.matrix(aussieCrab[, 5:6]))%*% solve(covHat)%*% as.matrix(female_vec) 
Wo_female <- (0.5 *  t(as.matrix(female_vec)) %*% solve(covHat) %*% as.matrix(female_vec) +
   log(prior_female))
disc_female <-W_female - as.vector(Wo_female)

# For loop that uses the obtained decision boundary
classif <- 0
for(i in 1:200){
  if(disc_male[i] > disc_female[i]){
    classif[i] = "male"
  }else{
    classif[i] = "female"
  }
}

lda_class <- data.frame(aussieCrab, classif)

ggplot(lda_class, aes(y=CL, x=RW)) + 
  geom_point(aes(color=classif,shape=sex), size=4) +
  geom_abline(intercept = -5.06, slope=2.91, colour="red")

table(actual=aussieCrab$sex, predict=classif)


logi_class <- glm(sex~RW+CL,data=aussieCrab, family=binomial())
pred_logi <- data.frame(aussieCrab$RW,aussieCrab$CL,aussieCrab$sex , predict(logi_class, type="response"))

class <- rep(0,200)
pred <- cbind(round(pred_logi[,4]), class)
for(i in 1:200){
    if(pred[i] == 1){
        pred[i,2] <- "Male"
    }else{
        pred[i,2] <- "Female"
    }
}
pred_logi$pred <- pred[,2]

ggplot(pred_logi, aes(y=aussieCrab.CL, x=aussieCrab.RW)) + 
  geom_point(aes(color=pred,shape=aussieCrab.sex), size=4) +
  geom_abline(intercept = -2.94, slope=2.713, colour="red")

# Assignment 2
data <- read.csv("C:/Users/Gustav/Documents/Machine-Learning/Lab 3/creditscoring.csv", sep=";", header = TRUE) 

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]

library(tree)
tree.dev <- tree(good_bad ~., train, split="deviance")
misclass <- function(data, measure, ...){
    set.seed(12345)
    x <- table(data$good_bad, predict(measure, newdata=data,...))
    n <- sum(x)
    return(1 - sum(diag(x))/n)
}

misclass(train, tree.dev, type="class")
misclass(test, tree.dev, type="class")


tree.gini <- tree(good_bad ~., train, split="gini")
misclass(train, tree.gini, type="class")
misclass(test, tree.gini, type="class")
train.score <- NULL
valid.score <- NULL

for(i in 2:15){
    pruned = prune.tree(tree.dev, best=i)
    pred = predict(pruned, newdata=valid, type="tree")
    train.score[i] = deviance(pruned)
    valid.score[i] = deviance(pred)
}
plot(2:15, train.score[2:15], type="b", ylim=c(0,560))
points(2:15, valid.score[2:15], type="b", col="purple")
legend("bottomleft",c("training","validation"),col=c("black","purple"), lwd=c(1,1))

finalTree <- prune.tree(tree.dev, best=4)
# The variables used
# Model tested on test data
NewFit <- predict(finalTree, newdata=test, type="class")
plot(finalTree)
text(finalTree, pretty=0)
NewFitTable <- table(test$good_bad, NewFit)
library(e1071)

fitBayes<-naiveBayes(good_bad~., data=train)

YfitBayesTrain<-predict(fitBayes, newdata=train)
YfitBayesTrain <- table(YfitBayesTrain,train$good_bad)
YfitBayesTrain
YfitBayesTest<-predict(fitBayes, newdata=test)
YfitBayesTest <- table(YfitBayesTest,test$good_bad)
YfitBayesTest
# Repeating 2.4 but with a defined loss matrix. 
raws <- data.frame(predict(fitBayes, newdata=train, type="raw"))

preds <- 0
for (i in 1:500){
  if((raws[i,1]/raws[i,2]) > 0.1){
    preds[i] = "bad"
  }else{
    preds[i] ="good"
  }  
}

table(train$good_bad, preds)
(table(train$good_bad, preds) [2,1] + table(train$good_bad, preds)[1,2]) / sum((table(train$good_bad, preds)))


rawTest <- predict(fitBayes, newdata=test, type="raw")
preds <- 0
for (i in 1:250){
  if((rawTest[i,1]/rawTest[i,2]) > 0.1){
    preds[i] = "bad"
  }else{
    preds[i] ="good"
  }  
}

table(test$good_bad, preds)
(table(test$good_bad, preds) [2,1] + table(test$good_bad, preds)[1,2]) / sum((table(test$good_bad, preds)))

## 
