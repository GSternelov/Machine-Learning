aussieCrab <- read.csv("australian-crabs.csv", sep=",", header = TRUE)

library(ggplot2)
ggplot(aussieCrab, aes(y=CL, x=RW)) + geom_point(aes(color=sex), size=3)

# 1.2

male_data <- subset(aussieCrab, sex=="Male")[5:6]
female_data <- subset(aussieCrab, sex=="Female")[5:6]

male_vec <- c(mean(male_data[,1]), mean(male_data[,2]))
female_vec <- c(mean(female_data[,1]), mean(female_data[,2]))

male_cov <- cov(male_data)
female_cov <- cov(female_data)

covHat <- matrix(100/200 * male_cov + 100/200 * female_cov, ncol=2)

# prop priors
prior_male <- 100/200
prior_female <- 100/200

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

#table(classif, aussieCrab$sex)
lda_class <- data.frame(aussieCrab, classif)

ggplot(lda_class, aes(y=CL, x=RW)) + 
  geom_point(aes(color=classif,shape=sex), size=4) +
  geom_abline(intercept = -5.06, slope=2.91, colour="red")

logi_class <- glm(sex~RW+CL,data=aussieCrab, family=binomial())
#summary(logi_class)
#exp(coef(logi_class))
pred_logi <- data.frame(aussieCrab$RW,aussieCrab$CL,aussieCrab$sex , predict(logi_class, type="response"))

for (i in 1:length(pred_logi[,4])){
if(pred_logi[,4][i] <0.5){
  pred_logi[,4][i] = 0
}else{
  pred_logi[,4][i] = 1
} }

for (i in 1:length(pred_logi[,4])){
  if(pred_logi[,4][i] == 0){
    pred_logi[,4][i] = "Female"
  }else{
    pred_logi[,4][i] = "Male"
  } }

ggplot(pred_logi, aes(y=aussieCrab.CL, x=aussieCrab.RW)) + 
  geom_point(aes(color=pred_logi[,4],shape=aussieCrab.sex), size=4) +
  geom_abline(intercept = -2.94, slope=2.713, colour="red")


#table(pred_logi[,4], pred_logi$aussieCrab.sex)
creditScore <- read.csv("creditscoring.csv", sep=";", header = TRUE) 

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
library(tree)
# a) - deviance
# train
deviFit <- tree(good_bad~., data=train, split="deviance")
#plot(deviFit)
#text(deviFit, pretty=0)
#deviFit
devi_summary <- summary(deviFit)
deviPred <- predict(deviFit, newdata=test, type="class")
deviTable <- table(test$good_bad, deviPred)

# b) gini index
# train
giniFit <- tree(good_bad~., data=train, split="gini")
gini_summary <- summary(giniFit)
giniPred <- predict(giniFit, newdata=test, type="class")
giniTable <- table(test$good_bad, giniPred)

# Lower misclassification for deviance
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
finalTree <- prune.tree(deviFit, best=4)
# The variables used
plot(finalTree)
text(finalTree, pretty=0)
# Model tested on test data
NewFit <- predict(finalTree, newdata=test, type="class")
NewFitTable <- table(test$good_bad, NewFit)
library(e1071)
# bayes classifier based on training data
bayesFit <- naiveBayes(good_bad~., data=train)
# Fitted valuse for training and test based on classifier
bayesPredTrain <- predict(bayesFit, newdata=train)
bayesPredTest <- predict(bayesFit, newdata=test)
# confusion matrices for train and test
bayesTrain <-table(train$good_bad, bayesPredTrain)
bayesTest <- table(test$good_bad, bayesPredTest)
bayesTrain
bayesTest
# Repeating 2.4 but with a defined loss matrix. 
raws <- data.frame(predict(bayesFit, newdata=train, type="raw"))

preds <- 0
for (i in 1:500){
  if((raws[i,1]/raws[i,2]) > 0.1){
    preds[i] = "bad"
  }else{
    preds[i] ="good"
  }  
}

loss_train <- table(train$good_bad, preds)
loss_train

rawTest <- predict(bayesFit, newdata=test, type="raw")
preds <- 0
for (i in 1:250){
  if((rawTest[i,1]/rawTest[i,2]) > 0.1){
    preds[i] = "bad"
  }else{
    preds[i] ="good"
  }  
}

loss_test <- table(test$good_bad, preds)
loss_test
## 
