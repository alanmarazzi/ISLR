### Exercises CH 6
# I will generate simulated randoma data and perform best subset selection
set.seed(9)
x <- rnorm(100)
e <- rnorm(100)
y <- 2 + 3*x + .5*x^2 + 4*x^3 + e
plot(x,y)

xy <- data.frame(x = x, y = y)
library(leaps)
mat <- model.matrix(y~x, data = xy)
fit <- regsubsets(mat~.)


## 9
# Predict the number of applications in the College dataset
library(ISLR)
col <- College

set.seed(5)
smpl <- sample(1:nrow(col), nrow(col)/2)
train <- col[smpl,]
test <- col[-smpl,]

linMod <- lm(Apps~., data = train)
summary(linMod)
pred <- predict(linMod, test)
mean((pred - test$Apps)^2)

# Ridge Regression with CV
library(glmnet)
x <- model.matrix(Apps~., data = train)[,-1]
y <- train$Apps
testX <- model.matrix(Apps~., data = test)[,-1]

ridge <- cv.glmnet(x, y, alpha = 0)
plot(ridge)
best <- ridge$lambda.min
prdRidge <- predict(ridge, s = best, newx = testX)
mean((prdRidge - test$Apps)^2)

# Lasso with CV
lasso <- cv.glmnet(x, y, alpha = 1)
plot(lasso)
best <- lasso$lambda.min
prdLasso <- predict(lasso, s = best, newx = testX)
mean((prdLasso-test$Apps)^2)
