
# Assignment 1
library(MASS)

# Reads in data set Longley
data(longley)
data <- longley

longley.xx <- data.matrix(longley[, 1:6])
longley.y <- longley[, "Employed"]

longley.y <- longley.y - mean(longley.y)
for(i in 1:6){
  longley.x[,i] <- longley.x[,i] - mean(longley.x[,i]) 
  
}

# Implement ridgereg function
ridgereg_nfoldCV <- function(x, y, lambda, nfolds){
  # Create the folds and initialize CV vector
  n <- length(y)
  seques <- floor(n/nfolds)
  reps <- n%%nfolds
  groups <- rep(seq(1,nfolds, 1), seques)
  end_values <- rep(nfolds, reps)
  
  folds <- c(groups, end_values)
  folds <- sort(folds) 
  for (i in 1:nfolds){
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData_x <- x[testIndexes, ]
    testData_y <- y[testIndexes]
    trainData_x <- x[-testIndexes, ]
    trainData_y <- y[-testIndexes]
    
    # Perform ridge regression on train data
    x_t <- t(trainData_x)
    I <- diag(ncol(trainData_x))
    BetaRidge <- solve(x_t %*% trainData_x + lambda * I) %*% x_t %*% trainData_y
    #y_hat <- trainData_x %*% BetaRidge
    # Test regression on test data and compare with true values for test data
    y_hat_test <- testData_x %*% BetaRidge
    # Calculates CV
    CV[i] <- sum((testData_y - y_hat_test)^2)
  }
  CV_score <- (1/nrow(longley.x)) * sum(CV)
  return(CV_score)
}
for (i in 1:100){
  print(ridgereg_nfoldCV(longley.x, longley.y, lambda=i, nfolds=10))
}

# Assignment 2
# 2.1
tecator <- read.csv("Lab 2/tecator.csv", sep=";", header = TRUE)

plot(tecator$Moisture, tecator$Protein)

# By looking at the plot it does not seem completely of the charts to, at least to start with,
# consider a linear model. 

# 2.2
# How the Mi model looks up to the power n.
# lm(Moisture ~ poly(Protein, n, raw=TRUE))


# 2.3 
# Divides data into train and test
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


plot(1:6, MSE_train, type="l", ylim=c(30,35), col="blue")
lines(1:6, MSE_test, type="l", col="red")

# 2.4
AIC <- 0
for (i in 1:6){
  poly_modelTrain[[i]] <-  lm(Moisture_train ~ poly(Protein_train, i, raw=TRUE))
  n <- length(Moisture_train)
  RSS <- sum(poly_modelTrain[[i]]$residuals^2)  
  AIC[i] <- 2*(i+1) + n * log(RSS/n)
}

# 2.5
vars <- data.frame(tecator[,2:102])

FullModel <- lm(Fat ~ ., data=vars)
Step_selec <- stepAIC(FullModel, direction = "both")
Step_selec$coefficients


# 2.6 - 2.7
library(glmnet)
# Create matrix with x variables 
mat_vars <- as.matrix(vars[,1:100])
# y variable
mat_y <- as.matrix(vars[,101])

ridge_mod <- glmnet(mat_vars, mat_y, alpha=0, family = "gaussian")
plot(ridge_mod, xvar="lambda",label=TRUE)

lasso_mod <- glmnet(mat_vars, mat_y, alpha=1, family = "gaussian")
plot(lasso_mod, xvar="lambda",label=TRUE)

# 2.8
set.seed(12345)
lasso_cv <- cv.glmnet(mat_vars, mat_y, alpha=1, family = "gaussian")

lasso_cv$lambda.min
plot(lasso_cv)
coef(lasso_cv, s = "lambda.min")

lasso_cv$cvm

# # # # # # #



