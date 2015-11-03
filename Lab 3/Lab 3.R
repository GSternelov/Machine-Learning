

# Assignment 1
aussieCrab <- read.csv("Lab 3/australian-crabs.csv", sep=",", header = TRUE)

# 1.1
library(ggplot2)
ggplot(aussieCrab, aes(y=RW, x=CL)) + geom_point(aes(color=sex), size=3)

# Is this data easy to classify by linear discriminant analysis?

# 1.2
# estimate coefficients (c means compute value for c classes)
# Inputs to estimate for are RW and CL
myC <- 1/Nc *sum(xi)
covC <- 1/Nc * sum((xi - myC)*t(xi-myC))
covHat <- 1/N sum(Nc*covC)
propPriorC <- Nc / N

# calculate the parts of softmax
Woi <- -1/2 *t(myC)*solve(covHat)*myC + log(propPriorC)
Wi <- solve(covHat)*myC

# Then softmax equals p(y = ....), which equals
t(Wi)*x + Woi
# this is the expression that is used to classify the observations

# Genomförande
myC[i] <- 1/Nc *sum(xi)



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
# Repeating 2.4 but with a defined loss matrix. 
lossMat <- matrix(c(0,10,1,0), ncol=2)

lossMat * bayesTrain
lossMat * bayesTest

# If I set the zeros in the loss matrix to zero, will the right confusion matrix be
# obtained? Like this
lossMat2 <- matrix(c(1,10,1,1), ncol=2)

lossMat2 * bayesTrain
lossMat2 * bayesTest




