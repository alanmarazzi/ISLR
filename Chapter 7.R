### Chapter 7: Non-linear Modeling
library(ISLR)
w <- Wage

## Polynomial Regression and Step Functions
fit <- lm(wage~poly(age, 4), data = w)

# I create a grid of values for age at which we want predictions
agelims <- range(w$age)
ageGrid <- seq(from = agelims[1], to = agelims[2])
pred <- predict(fit, newdata = list(age = ageGrid), se = TRUE)
se <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
par(mfrow = c(1,2), mar = c(4.5, 4.5, 1, 1), oma = c(0,0,4,0))
plot(w$age, w$wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree 4 Poly", outer = TRUE)
lines(ageGrid, pred$fit, lwd = 2, col = "blue")
matlines(ageGrid, se, lwd = 1, col = "blue", lty = 3)

library(ggplot2)
library(hexbin)
library(viridis)
library(ggthemes)
ggplot(w, aes(age, wage))+
    geom_hex(alpha = .75, bins = 50)+
    scale_fill_viridis()+
    geom_smooth(method = "lm", formula = y~poly(x, 4), color = "red", size = 1, fill = "blue")+
    theme_tufte()

# For selecting the degree of the polynomial I can use ANOVA to test that a model M1 is sufficient to explain the data instead of a more complex one.
# In order to use ANOVA the models must be nested: the predictors in M1 must be a subset of the predictors in M2, and so on.
fit <- list()
for(i in 1:5){
    fit[[i]] <- list(paste0("fit", i))
    fit[[i]] <- lm(wage~poly(age, i), data = w)
}
anova(fit[[1]], fit[[2]], fit[[3]], fit[[4]], fit[[5]])
