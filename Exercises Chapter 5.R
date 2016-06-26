### CH 5: Applied
library(ISLR)
str(Default)

# 5
set.seed(2)
train <- sample(nrow(Default), nrow(Default)/2)
mdl <- glm(default~income+balance, data = Default, subset = train, family = binomial)
summary(mdl)
probs <- predict(mdl, Default[-train,])
pred <- rep("No", nrow(Default[-train,]))
pred[probs > .5] <- "Yes"
table(pred, Default$default[-train])
res1 <- mean(pred != Default$default[-train])

mdl2 <- glm(default~income+balance+student, data = Default, subset = train, family = binomial)
probs <- predict(mdl2, Default[-train,])
pred <- rep("No", nrow(Default[-train,]))
pred[probs > .5] <- "Yes"
table(pred, Default$default[-train])
res2 <- mean(pred != Default$default[-train])

# 6
set.seed(5)
summary(glm(default~income+balance,data = Default,family = binomial))
fnc <- function(data, index)return(coef(glm(default~income+balance, data = Default, subset = index, family = binomial)))
library(boot)
boot(Default, fnc, R = 1000)

# 7
data(Weekly)
mdl1 <- glm(Direction~Lag1+Lag2, data = Weekly, family = binomial)
mdl2 <- glm(Direction~Lag1+Lag2, data = Weekly, family = binomial, subset = 2:1089)
prob <- predict(mdl2, Weekly[1,])
pred <- "Down"
pred[prob > .5] <- "Up"
pred
Weekly[1,"Direction"]

for(i in 1:nrow(Weekly)){
    prob <- vector("double", nrow(Weekly))
    pred <- rep("Down", nrow(Weekly))
    error <- rep(0, nrow(Weekly))
    mdl <- glm(Direction~Lag1+Lag2, data = Weekly[-i,], family = binomial)
    prob[i] <- predict(mdl, Weekly[i,])
    pred[i][prob[i] > .5] <- "Up"
    error[i][pred[i] != Weekly[i, "Direction"]] <- 1
}

# 8
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x-2*x^2+rnorm(100)
plot(x,y)

set.seed(5)
p <- data.frame(x = x, y = y)
cv <- c(1:4)
for(i in 1:4){
    regr <- glm(y~poly(x,i),data=p)
    cv[i] <- cv.glm(p, regr)$delta[1]
}

set.seed(123)
cv <- c(1:4)
for(i in 1:4){
    regr <- glm(y~poly(x,i),data=p)
    cv[i] <- cv.glm(p, regr)$delta[1]
}
library(ggplot2)
ggplot(p, aes(x,y))+
    geom_point()+
    geom_smooth(method = "lm", formula = y~poly(x,2))

# 9
library(MASS)
m <- mean(Boston$medv)
se <- sd(Boston$medv)/sqrt(nrow(Boston))
f <- function(d,i){
    d2 <- d[i]
    return(mean(d2))
}
boot(Boston$medv, f, R = 1000)
