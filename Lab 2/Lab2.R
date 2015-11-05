## ---- echo=FALSE, eval=TRUE----------------------------------------------
library(MASS)

# Reads in data set Longley
data(longley)
data <- longley

longley.x <- data.matrix(longley[, 1:6])
longley.y <- longley[, "Employed"]

ridgereg_nfoldCV <- function(x, y, lambda, nfolds){
  # Create the folds and initialize CV vector
  n <- length(y)
  seques <- floor(n/nfolds)
  reps <- n%%nfolds
  groups <- rep(seq(1,nfolds, 1), seques)
  end_values <- rep(nfolds, reps)
  folds <- c(groups, end_values)
  folds <- sort(folds) 
  
  #x <- cbind(rep(1, nrow(x)), x)
  CV <- 0
  for (i in 1:nfolds){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData_x <- x[testIndexes, ]
    testData_y <- y[testIndexes]
    trainData_x <- x[-testIndexes, ]
    trainData_y <- y[-testIndexes]
    
    # find the mean value for columns in train, use that mean to scale the values 
    # in test. For example, if mean in column 1 in train is 3, then use that value
    # to scale column 1 in test data. 
    if (class(testData_x) == "numeric"){
      testData_x <- data.frame(t(testData_x))
    }else{
      testData_x <- data.frame(testData_x)
    }
    mean_x <- 0
    center_test <- matrix(ncol=ncol(testData_x), nrow=nrow(testData_x))
    for (j in 1:ncol(trainData_x)){
      mean_x[j] <- mean(trainData_x[,j])
      center_test[,j] <- testData_x[,j] - mean_x[j]
    } 
    testData_y <- testData_y - mean(trainData_y)
    trainData_x <- scale(trainData_x, center=TRUE, scale=FALSE)
    trainData_y <- scale(trainData_y, center=TRUE, scale=FALSE)
    testData_x <- as.matrix(center_test)
    
        
    # Perform ridge regression on train data
    x_t <- t(trainData_x)
    I <- diag(ncol(trainData_x))
    BetaRidge <- solve(x_t %*% trainData_x + lambda * I) %*% x_t %*% trainData_y
    # Test regression on test data and compare with true values for test data
    y_hat_test <- testData_x %*% BetaRidge
    # Calculates CV
    CV[i] <- sum((testData_y - y_hat_test)^2)
  }
  CV_score <- (1/nfolds) * sum(CV)
  return(CV_score)
}
CvScore <- 0
for (i in 1:7){
  CvScore[i] <- ridgereg_nfoldCV(longley.x, longley.y, lambda=i, nfolds=10)
}
plot(CvScore, type="l", xlab="Lambda")

## ---- echo=FALSE---------------------------------------------------------
tecator <- read.csv("tecator.csv", sep=";", header = TRUE)

plot(tecator$Protein, tecator$Moisture)

## ---- echo=FALSE---------------------------------------------------------
n=dim(tecator)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=tecator[id,]
test=tecator[-id,]

# Extract data sets for moisture and protein data from train and test
Protein_train <- train$Protein
Moisture_train <- train$Moisture
Protein_test <- test$Protein
Moisture_test <- test$Moisture

poly_modelTrain <- list()
MSE_train <- 0
MSE_test <- 0

for (i in 1:6){
poly_modelTrain[[i]] <-  lm(Moisture_train ~ poly(Protein_train, i, raw=TRUE))
MSE_train[i] <- 1/length(Moisture_train) * sum(poly_modelTrain[[i]]$residuals^2)
y_hat_test <- predict(lm(Moisture_train ~ poly(Protein_train, i)), data.frame(Protein_train=Protein_test))
MSE_test[i] <- 1/length(Moisture_test)  * sum((y_hat_test-Moisture_test)^2)
}


plot(1:6, MSE_train, type="l", ylim=c(30,35), col="blue", ylab="MSE", xlab="Model")
lines(1:6, MSE_test, type="l", col="red")
legend(5.25,33.5,c("Train","Test"), lty=c(1,1), 
  lwd=c(2.5,2.5),col=c("blue","red"),  cex=0.6) 

## ---- echo=FALSE---------------------------------------------------------
AIC <- 0
for (i in 1:6){
  poly_modelTrain[[i]] <-  lm(tecator$Moisture ~ poly(tecator$Protein, i, raw=TRUE))
  n <- length(tecator$Moisture)
  RSS <- sum(poly_modelTrain[[i]]$residuals^2)  
  AIC[i] <- 2*(i+1) + n * log(RSS/n)
}
plot(AIC, type="l", xlab="Model")

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(glmnet)
vars <- data.frame(tecator[,2:102])
# Create matrix with x variables 
mat_vars <- as.matrix(vars[,1:100])
# y variable
mat_y <- as.matrix(vars[,101])

ridge_mod <- glmnet(mat_vars, mat_y, alpha=0, family = "gaussian")
plot(ridge_mod, xvar="lambda",label=TRUE)

## ---- echo=FALSE---------------------------------------------------------
lasso_mod <- glmnet(mat_vars, mat_y, alpha=1, family = "gaussian")
plot(lasso_mod, xvar="lambda",label=TRUE)

## ---- echo=FALSE---------------------------------------------------------
set.seed(12345)
lasso_cv <- cv.glmnet(mat_vars, mat_y, alpha=1, family = "gaussian")
plot(lasso_cv)

## ----code=readLines(knitr::purl('C:/Users/Gustav/Documents/Machine-Learning/Lab 2/Lab2.Rmd', documentation = 1)), eval = FALSE----
## 

