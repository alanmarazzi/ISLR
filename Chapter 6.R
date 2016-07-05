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
