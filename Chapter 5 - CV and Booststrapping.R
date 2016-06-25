### ISLR: Chapter 5 Cross-Validation and Bootstrapping

## Validation Set Approach
# We will split randomly a dataset in a training and test set
library(ISLR)
set.seed(1)
train <- sample(392,196)

# Perform a linear regression on the Auto dataset
regr <- lm(mpg~horsepower, data = Auto, subset = train)

# Now look at the results on the test set
mean((Auto$mpg-predict(regr, Auto))[-train]^2)

# Try with different degrees polynomial regressions
regr2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((Auto$mpg-predict(regr2, Auto))[-train]^2)

regr3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((Auto$mpg-predict(regr3, Auto))[-train]^2)

# See what happens with a different random split
set.seed(2)
train <- sample(392,196)
regr <- lm(mpg~horsepower, data = Auto, subset = train)
mean((Auto$mpg-predict(regr, Auto))[-train]^2)
regr2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((Auto$mpg-predict(regr2, Auto))[-train]^2)
regr3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((Auto$mpg-predict(regr3, Auto))[-train]^2)

## Leave-One-Out CV (LOOCV)
# We will use the cv.glm() function to perform LOOCV in the boot package
library(boot)
regr <- glm(mpg~horsepower, data = Auto)
cv <- cv.glm(Auto, regr)
cv$delta

# I can repeat the process to test for increasing degrees of polynomials for example
cv <- rep(0,5)
for(i in 1:10){
    regr <- glm(mpg~poly(horsepower,i), data = Auto)
    cv[i] <- cv.glm(Auto, regr)$delta[1]
}
cv <- data.frame(Error = cv, Degree = seq(1,10,by = 1))

library(ggplot2)
ggplot(cv, aes(Degree, Error))+
    geom_line(color = "red", size = 1)+
    geom_point(aes(Degree, Error), color = "red")+
    theme_bw()+
    scale_x_continuous(breaks = seq(1,10,1))

## K-fold CV
# To perform K-fold we use a similar approach as before
set.seed(17)
cv <- rep(0,10)
for(i in 1:10){
    regr <- glm(mpg~poly(horsepower,i), data = Auto)
    cv[i] <- cv.glm(Auto, regr, K = 10)$delta[1]
}
cv

## Bootstrap
# We want to minimize the risk of an investment portfolio made of 2 items. We will use bootstrapping
A <- function(data, index){
    X = data$X[index]
    Y = data$Y[index]
    return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
# Estimation without bootstrapping
A(Portfolio, 1:100)

# Let's say we want to be more confident on the result. We will use bootstrapping. First let's do just one sampling: bootstrapping does the same thing repeated n times
set.seed(1)
A(Portfolio, sample(100, 100, replace = TRUE))

# Now the real bootstrapping with 1000 reps
boot(Portfolio, A, R = 1000)

# If we want to do bootstrapping on the parameters of a lm we can do like this
fnc <- function(data, index)return(coef(lm(mpg~horsepower, data = data, subset = index)))
fnc(Auto, 1:392)
boot(Auto, fnc, R = 1000)
