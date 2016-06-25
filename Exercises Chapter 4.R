### ISLR - Ch 4 Applied exercises
library(ISLR)
dim(Weekly)
str(Weekly)
summary(Weekly)
plot(Weekly)
plot(Weekly$Year, Weekly$Volume)

# Try a logistic regression on full data
lr <- glm(Direction~., data = Weekly, family = binomial, control = list(maxit = 50))
summary(lr)
prob <- predict(lr, Weekly, type = "response")
pred <- rep("Down", 1089)
pred[prob > .5] <- "Up"
table(pred, Weekly$Direction)

# Fit an LDA to the data
train <- Weekly[Weekly$Year <= 2008,]
test <- Weekly[Weekly$Year > 2008, ]

library(MASS)
library(class)
ldaFit <- lda(Direction~Lag2, data = train)
ldaFit
pred <- predict(ldaFit, test)
predClass <- pred$class
table(predClass, test$Direction)

# Do the same with QDA
qdaFit <- qda(Direction~Lag2, data = train)
qdaFit
pred <- predict(qdaFit, test)$class
table(pred, test$Direction) 

# The same with KNN
trainX <- Weekly[Weekly$Year <= 2008,-9]
trainY <- Weekly[Weekly$Year <= 2008,9]
testX <- Weekly[Weekly$Year > 2008, -9]
testY <- Weekly[Weekly$Year > 2008, 9]
knnFit <- knn(trainX, testX, trainY, k = 1)
mean(testY != knnFit)
mean(testY != "Up")

k33 <- knn(trainX, testX, trainY, k = 33)
mean(testY != k33)
mean(testY != "Up")

# Try to predict gas mileage by car
Auto$mpg01 <- 0
Auto$mpg01[Auto$mpg > median(Auto$mpg)] <- 1
summary(Auto)
plot(mpg01~., data = Auto)
boxplot(Auto$mpg01, Auto$weight)
dim(Auto)
ts <- sample(nrow(Auto), 250)
test <- Auto[ts,]
train <- Auto[-ts,]

# Try with LDA
cor(Auto[,-9])
ldaFit <- lda(mpg01~cylinders+displacement+horsepower+weight, data = train)
pred <- predict(ldaFit, test)
pred <- pred$class
table(pred, test$mpg01)
mean(pred != test$mpg01)

# Go with QDA
qdaFit <- qda(mpg01~cylinders+displacement+horsepower+weight, data = train)
pred <- predict(qdaFit, test)
pred <- pred$class
table(pred, test$mpg01)
mean(pred != test$mpg01)

# Go with logistic regression
lr <- glm(mpg01~cylinders+displacement+horsepower+weight, data = train, family = binomial)
prob <- predict(lr, test, type = "response")
pred <- rep(0,250)
pred[prob > .5] <- 1
table(pred, test$mpg01)
mean(pred != test$mpg01)

# And now KNN
sqrt(nrow(Auto))
testX <- scale(test[,-c(1,6:10)])
trainX <- scale(train[,-c(1,6:10)])
testY <- test$mpg01
trainY <- train$mpg01
K19 <- knn(trainX, testX, trainY, k = 19)
table(K19, testY)
mean(K19 != testY)
K10 <- knn(trainX, testX, trainY, k = 10)
table(K10, testY)
mean(K10 != testY)


### Function writing 
Power <- function(x, a){
    p <- x^a
    return(p)
}
Power(2,3)
Power(10,3)
Power(8,17)

# From the Boston dataset predict the crime rate 
cor(Boston)
Boston$crim2 <- rep(0, nrow(Boston))
Boston$crim2[Boston$crim > median(Boston$crim)] <- 1

# Logistic regression
set.seed(367)
ind <- sample(nrow(Boston), 406)
train <- Boston[ind,]
test <- Boston[-ind,]
lr <- glm(crim2~indus+nox+rad+tax+lstat+medv, data = train, family = binomial)
prob <- predict(lr, test, type = "response")
pred <- rep(0, nrow(test))
pred[prob > .5] <- 1
table(pred, test$crim2)

lr <- glm(crim2~nox+rad+tax, data = train, family = binomial)
prob <- predict(lr, test, type = "response")
pred <- rep(0, nrow(test))
pred[prob > .5] <- 1
table(pred, test$crim2)

# LDA
ldaFit <- lda(crim2~nox+rad+tax, data = train)
pred <- predict(ldaFit, test)$class
table(pred, test$crim2)

ldaFit <- lda(crim2~., data = train[,-1])
pred <- predict(ldaFit, test)$class
table(pred, test$crim2)
mean(pred != test$crim2)

# QDA
qdaFit <- qda(crim2~nox+rad+tax, data = train)
pred <- predict(qdaFit, test)
table(pred$class, test$crim2)
mean(pred$class != test$crim2)

qdaFit <- qda(crim2~., data = train[,-1])
pred <- predict(qdaFit, test)
table(pred$class, test$crim2)
mean(pred$class != test$crim2)

# KNN
sqrt(nrow(train))
trainX <- scale(train[,-c(1, 15)])
trainY <- train$crim2
testX <- scale(test[,-c(1,15)])
testY <- test$crim2
K20 <- knn(trainX, testX, trainY, k = 20)
table(K20, test$crim2)
mean(K20 != test$crim2)
K10 <- knn(trainX, testX, trainY, k = 10)
table(K10, test$crim2)
mean(K10 != test$crim2)
K5 <- knn(trainX, testX, trainY, k = 5)
table(K5, test$crim2)
mean(K5 != test$crim2)
K3 <- knn(trainX, testX, trainY, k = 3)
table(K3, test$crim2)
mean(K3 != test$crim2)
