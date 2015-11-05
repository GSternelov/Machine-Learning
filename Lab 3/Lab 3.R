

# Assignment 1
aussieCrab <- read.csv("Lab 3/australian-crabs.csv", sep=",", header = TRUE)

# 1.1
library(ggplot2)
ggplot(aussieCrab, aes(y=RW, x=CL)) + geom_point(aes(color=sex), size=3)

# Is this data easy to classify by linear discriminant analysis?

# 1.2

male_data <- subset(aussieCrab, sex=="Male")[5:6]
female_data <- subset(aussieCrab, sex=="Female")[5:6]

c(mean(male_data[,1]), mean(male_data[,2]))
male_vec <- c(mean(male_data[,1]), mean(male_data[,2]))
female_vec <- c(mean(female_data[,1]), mean(female_data[,2]))

male_cov <- cov(male_data)
female_cov <- cov(female_data)

covHat <- matrix(100/200 * male_cov + 100/200 * female_cov, ncol=2)

# prop priors
prior_male <- 100/200
prior_female <- 100/200



# Instead calculate the discriminant function

W_male <- (as.matrix(aussieCrab[, 5:6]))%*% solve(covHat) %*% as.matrix(male_vec) 
Wo_male <-  (0.5 *  t(as.matrix(male_vec)) %*% solve(covHat) %*% as.matrix(male_vec) +
  log(prior_male))
disc_male <- W_male - as.vector(Wo_male)

W_female <- (as.matrix(aussieCrab[, 5:6]))%*% solve(covHat)%*% as.matrix(female_vec) 
Wo_female <- (0.5 *  t(as.matrix(female_vec)) %*% solve(covHat) %*% as.matrix(female_vec) +
   log(prior_female))
disc_female <-W_female - as.vector(Wo_female)
classif <- 0
for(i in 1:200){
  if(disc_male[i] > disc_female[i]){
    classif[i] = "male"
  }else{
    classif[i] = "female"
  }
}

lm(classif~RW+CL, lda_class)
table(classif, aussieCrab$sex)

# 1.3
lda_class <- data.frame(aussieCrab, classif)

ggplot(lda_class, aes(y=CL, x=RW)) + 
  geom_point(aes(color=classif,shape=sex), size=4) +
  geom_abline(intercept = -5.06, slope=2.91, colour="red")


# 1.4
logi_class <- glm(sex~RW+CL,data=aussieCrab, family=binomial())
summary(logi_class)
exp(coef(logi_class))
pred_logi <- data.frame(aussieCrab$RW,aussieCrab$CL,aussieCrab$sex , predict(logi_class, type="response"))

for (i in 1:length(pred_logi[,4])){
if(pred_logi[,4][i] <0.5){
  pred_logi[,4][i] = 0
}else{
  pred_logi[,4][i] = 1
} }
  
ggplot(pred_logi, aes(y=aussieCrab.CL, x=aussieCrab.RW)) + 
  geom_point(aes(color=pred_logi[,4],shape=aussieCrab.sex), size=4) +
  geom_abline(intercept = -2.94, slope=2.713, colour="red")

table(pred_logi[,4], pred_logi$aussieCrab.sex)

# Assignment 2
#2.1
creditScore <- read.csv("Lab 3/creditscoring.csv", sep=";", header = TRUE) 

# create train set
n=dim(creditScore)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=creditScore[id,]
test=creditScore[-id,]

# create valid and test
n=dim(test)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
valid=test[id,]
test=test[-id,]

# 2.2
library(tree)
# a) - deviance
deviFit <- tree(good_bad~., data=train, split="deviance")
plot(deviFit)
text(deviFit, pretty=0)
deviFit
summary(deviFit)
# b) gini index
giniFit <- tree(good_bad~., data=train, split="gini")
summary(giniFit)

# Lower misclassification for deviance

# 2.3 
trainScore <- rep(0,15)
testScore <- rep(0,15)

for (i in 2:15){
  prunedTree <- prune.tree(deviFit, best=i)
  pred <- predict(prunedTree, newdata=valid,
               type="tree")
  trainScore[i] <- deviance(prunedTree)
  testScore[i] <- deviance(pred)
}

plot(2:15, trainScore[2:15], type="b", col="red", ylim=c(250,550))
points(2:15, testScore[2:15], type="b", col="blue")
# Which is the optimal number of leaves?
# A try with optimal number of leaves set to 12
finalTree <- prune.tree(deviFit, best=12)
# The variables used
summary(finalTree)
# Model tested on test data
NewFit <- predict(finalTree, newdata=test, type="class")
table(test$good_bad, NewFit)

# 2.4 Naive-bayes
library(e1071)
# bayes classifier based on training data
bayesFit <- naiveBayes(good_bad~., data=train)
# Fitted valuse for training and test based on classifier
bayesPredTrain <- predict(bayesFit, newdata=train)
bayesPredTest <- predict(bayesFit, newdata=test)
# confusion matrices for train and test
bayesTrain <-table(bayesPredTrain, train$good_bad)
bayesTest <- table(bayesPredTest, test$good_bad)

# 2.5 
raws <- data.frame(predict(bayesFit, newdata=train, type="raw"))

preds <- 0
for (i in 1:500){
  if((raws[i,1]/raws[i,2]) > 0.1){
    preds[i] = "bad"
  }else{
    preds[i] ="good"
  }  
}

table(train$good_bad, preds)

rawTest <- predict(bayesFit, newdata=test, type="raw")
preds <- 0
for (i in 1:250){
  if((rawTest[i,1]/rawTest[i,2]) > 0.1){
    preds[i] = "bad"
  }else{
    preds[i] ="good"
  }  
}

table(test$good_bad, preds)



