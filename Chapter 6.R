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
