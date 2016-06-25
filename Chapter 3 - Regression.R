#### ISLR 3 Linear Regression

# Lab
setwd("~/R/ISLR")
library(MASS)
library(ISLR)

# Predict median house value (medv) in the Boston dataset by using avg rooms and poor households
data("Boston")
names(Boston)

lm.fit <- lm(medv ~ lstat, Boston)
summary(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "confidence", level = .98)
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "prediction", level = .98)

plot(Boston$lstat, Boston$medv)
abline(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow = c(1, 1))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

lm.fit <- lm(medv~lstat+age, data = Boston)
summary(lm.fit)

lm.fit <- lm(medv~., data = Boston)
summary(lm.fit)

library(car)
vif(lm.fit)

lm.fit1 <- lm(medv~. -age, data = Boston)
summary(lm.fit1)


summary(lm(medv ~ lstat*age, Boston))

lm.fit2 <- lm(medv ~ lstat + I(lstat^2), Boston)
summary(lm.fit2)
lm.fit <- lm(medv ~ lstat, Boston)
anova(lm.fit, lm.fit2)
plot(lm.fit2)

lm.fit5 <- lm(medv ~ poly(lstat, 5), Boston)
summary(lm.fit5)

summary(lm(medv~log(rm), Boston))


data("Carseats")
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)


# Applied Exercises
# 8)
data("Auto")
summary(lm(mpg~horsepower, Auto))
predict(lm(mpg~horsepower, Auto), data.frame(horsepower=98), interval = "confidence")
predict(lm(mpg~horsepower, Auto), data.frame(horsepower=98), interval = "prediction")

# 9)
plot(Auto)
cor(Auto[, -9])
