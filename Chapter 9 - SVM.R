### CH 9 Support Vector Machines Lab
library(e1071)
set.seed(1)
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y == 1,] <- x[y == 1,] + 1
plot(x, col = (3-y))

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)

svmfit <- svm(y~., data = dat, kernel = "linear", cost = .1, scale = FALSE)
plot(svmfit, dat)

# We can do k-fold cross validation with the tune() function in the e1071 package
set.seed(1)
tune_out <- tune(
    svm, 
    y~., 
    data = dat, 
    kernel = "linear", 
    ranges = list(cost = c(.001,.01,.1,1,5,10,100))
    )
summary(tune_out)

best_svm <- tune_out$best.model
summary(best_svm)

xtest <- matrix(rnorm(20*2), ncol = 2)
ytest = sample(c(-1, 1), 20, replace = TRUE)
xtest[ytest == 1,] <- xtest[ytest == 1,] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))
ypred <- predict(best_svm, testdat)
table(predict=ypred, truth=testdat$y)

# Now a test when the classes are linearly separable
x[y == 1,] <- x[y == 1,] + .5
plot(x, col = (y+5)/2, pch = 19)
dat = data.frame(x = x, y = as.factor(y))
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)

# Support Vector Machines - nonlinear kernel
set.seed(1)
x <-  matrix(rnorm(200*2), ncol = 2)
x[1:100,] <-  x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1,150), rep(2,50))
dat = data.frame(x = x, y = as.factor(y))
plot(x, col = y)

train <- sample(200, 100)
svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])

# If we increase the cost we will get less rainng error, but with the risk of overfitting data
svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])

# CV for cost and gamma
set.seed(1)
tune_out <- tune(
    svm, 
    y~., 
    data = dat[train,], 
    kernel = "radial", 
    ranges = list(cost = c(.1,1,10,100,1000),
                  gamma = c(.5,1,2,3,4))
    )
summary(tune_out)

table(true = dat[-train, "y"], pred = predict(tune_out$best.model, newdata = dat[-train,]))

# ROC Curves
# We can plot ROC curves with the ROCR package
library(ROCR)
rocplot <- function(pred, truth, ...){
    predob <- prediction(pred, truth)
    perf <- performance(predob, "tpr", "fpr")
    plot(perf, ...)
}

# SVM returns labels for classes, if we want values we fit svm() with decision.values = TRUE
svmfit_opt <- svm(y~., data = dat[train,], kernel = "radial", gamma = 2, cost = 1, decision.values = TRUE)
fitted <- attributes(predict(svmfit_opt, dat[train,], decision.values = TRUE))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], main = "Training Data")

svmfit_flex <- svm(y~., data = dat[train,], kernel = "radial", gamma = 50, cost = 1, decision.values = TRUE)
fitted <- attributes(predict(svmfit_flex, dat[train,], decision.values = TRUE))$decision.values
rocplot(fitted, dat[train, "y"], main = "Training Data")

# ROC on test data
par(mfrow = c(1,1))
fitted <- attributes(predict(svmfit_opt, dat[-train,], decision.values = TRUE))$decision.values
rocplot(fitted, dat[-train,"y"], main = "Test Data")

fitted <- attributes(predict(svmfit_flex, dat[-train,], decision.values = TRUE))$decision.values
rocplot(fitted, dat[-train,"y"], add = T, col = "red")

# SVM with multiple classes
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0,50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat = data.frame(x = x, y = as.factor(y))
plot(x, col = (y+1))

svmfit <- svm(y~., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

# Use case on gene expression
library(ISLR)
names(Khan)
dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
out <- svm(y~., data = dat, kernel = "linear", cost = 10)
summary(out)
table(out$fitted, dat$y)

dat_test <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred_test <- predict(out, dat_test)
table(pred_test, dat_test$y)
