### Chapter 6: Linear Model Selection and Regularization
# Best Subset Selection
# We wish to predict a baseball player's salary on his stats
library(ISLR)
names(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)

# We will perform a Best Subset Selection on the data using the regsubsets() function from leaps library
library(leaps)
fitFull <- regsubsets(Salary~.,Hitters)
summary(fitFull)

# Per default regsubsets() only fits up to 8 models, but we want to test every predictor
fitFull <- regsubsets(Salary~.,Hitters, nvmax = 19)
fitSummary <- summary(fitFull)
fitPlot <- data.frame(
    R2 = fitSummary$rsq, RSS = fitSummary$rss, AdjR2 = fitSummary$adjr2,
    Cp = fitSummary$cp, BIC = fitSummary$bic
    )
fitPlot$id <- seq(1,19,1)
fitPlot <- reshape2::melt(fitPlot, "id")
library(ggplot2)
ggplot(fitPlot, aes(id, value, color = variable))+
    facet_grid(variable~., scales = "free")+
    geom_line(size = .8)+
    scale_x_continuous(breaks = seq(0,20,1))

par(mfrow = c(2,2))
plot(fitFull, scale = "r2")
plot(fitFull, scale = "adjr2")
plot(fitFull, scale = "Cp")
plot(fitFull, scale = "bic")

## Forward and Backward Stepwise Selection
# To perform a stepwise regression we can always use the same regsubsets() with the method we want to adopt
fitFwd <- regsubsets(Salary~., Hitters, nvmax = 19, method = "forward")
summary(fitFwd)
fitBwd <- regsubsets(Salary~., Hitters, nvmax = 19, method = "backward")
summary(fitBwd)


## Choosing models with CV
# I will use the validation set approach to choose the best model
set.seed(1)
train <- sample(c(T,F), nrow(Hitters), rep = T)
test <- !train

best <- regsubsets(Salary~., data = Hitters[train,], nvmax = 19)

# To select the best fit I will start by creating an X matrix from the model data
mtrx <- model.matrix(Salary~., data = Hitters[test,])

# Now I run a loop and I extract the coefficients for the best model of that size
errors <- rep(NA, 19)
for(i in 1:19){
    coefi <- coef(best, id = i)
    pred <- mtrx[,names(coefi)]%*%coefi
    errors[i] <- mean((Hitters$Salary[test]-pred)^2)
}

coef(best, 10)

###### Since there is no predict() method for regsubsets() I will write my own predict function
pred_regsubsets <- function(object, newdata, id, ...){
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[,xvars]%*%coefi
}

# Now i will do k-folds cross-validation
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = TRUE)
errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for(j in 1:k){
    best <- regsubsets(Salary~., Hitters[folds != j,], nvmax = 19)
    for(i in 1:19){
        pred <- pred_regsubsets(best, Hitters[folds == j,], id = i)
        errors[j,i] <- mean((Hitters$Salary[folds == j] - pred)^2)
    }
}

mean_errors <- apply(errors, 2, mean)
par(mfrow = c(1,1))
plot(mean_errors, type = "b")

#### Ridge Regression and Lasso 
# I will use the glmnet package with the glmnet() function to perform ridge and lasso regression. It works differently than glm(): I need to feed glmnet() with a matrix and a vector without giving it the formula.
library(glmnet)
x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

# model.matrix() is really useful because it autoamtically turns factors into dummy values and glnet() can take only numeric values

### Ridge Regression
# With alpha = 0|1 I can decide to fit a ridge or a lasso regression
grid <- 10^seq(10, -2, length = 100)
ridge <- glmnet(x, y, alpha = 0, lambda = grid)

# glmnet() has default values for lambda, but here I fit lambda from 10^10 to 10^-2 covering the full range of scenarios from the null model.
# Note that glmnet() standardizes the variables per default. In case I don't want that I have to set standardize = FALSE
dim(coef(ridge))

# I can access single results
ridge$lambda[50]
coef(ridge)[,50]
coef(ridge)[,60]

# I can use the predict function for many purposes, for instance I can also try a new lambda
predict(ridge, s = 50, type = "coefficients")[1:20,]

# Validation set approach
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
yTest <- y[test]

ridge <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
pred <- predict(ridge, s = 4, newx = x[test, ])
# MSE
mean((pred-yTest)^2)

# Let's try with a higher lambda
pred <- predict(ridge, s = 1e10, newx = x[test, ])
mean((pred-yTest)^2)

# Compare ridge regression with least squares
pred <- predict(ridge, s = 0, newx = x[test,], exact = T)
mean((pred-yTest)^2)

# The better approach is to use cross validation
set.seed(1)
cv <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv)
best <- cv$lambda.min
best
pred <- predict(ridge, s = best, newx = x[test,])
mean((pred-yTest)^2)

# So I will go wit lambda = 212
out <- glmnet(x,y,alpha=0)
predict(out, type = "coefficients", s = best)


#### The Lasso
# I will always use glmnet() but with alpha = 1
lasso <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso)

# Cross Validation
set.seed(1)
cv <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv)
best <- cv$lambda.min
pred <- predict(lasso, s = best, newx = x[test,])
mean((pred-yTest)^2)

# The result is similar to ridge, though is higher lasso has put to 0 many variables simplyfying the model
out <- glmnet(x, y, alpha = 1, lambda = grid)
predict(out, type = "coefficients", s = best)[1:20,]
