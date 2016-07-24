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

# Instead of using ANOVA I could have just use poly() which creates orthogonal polynomials.
coef(summary(lm(wage~poly(age, 5), data = w)))

# As a next step I try to predict who is going to get more than 250k per year. I will use glm() to create a dummy variable
fit <- glm(I(wage > 250) ~ poly(age, 4), data = w, family = binomial)
pred <- predict(fit, newdata = list(age = ageGrid), se = T)

# For logistic regression I get the logit of predictions and errors, so for better interpretability I will transform them
pfit <- exp(pred$fit)/(1 + exp(pred$fit))
se_bands_logit <- cbind(pred$fit + 2 * pred$se.fit, pred$fit + 2 * pred$se.fit)
se_bands <- exp(se_bands_logit)/(1 + exp(se_bands_logit))

### Note that I could have used the "type = response" in predict() to have the same probbilities, but the confidence intervals would not have been sensible because I would have ended up with negative probabilities!
plot(w$age, I(w$wage > 250), xlim = agelims, type = "n", ylim = c(0,.2))
points(jitter(w$age), I((w$wage>250)/5), cex = .5, pch = "|", col = "darkgrey")
lines(ageGrid, pfit, lwd = 2, col = "blue")
matlines(ageGrid, se_bands, lwd = 1, col = "blue", lty = 3)

# To fit a step function I can use the cut() function
table(cut(w$age, 4)) # cut() splits data automatically, I can change them with breaks
fit <- lm(wage~cut(age, 4), data = w)
coef(summary(fit))


## Splines
# In order to fit a spline I can use the splines library. To consruct a matrix of basis functions I can use the bs() function.
library(splines)
fit <- lm(wage~bs(age, knots = c(25, 40, 60)), data = w)
pred <- predict(fit, newdata = list(age = ageGrid), se = T)
plot(w$age, w$wage, col = "gray")
lines(ageGrid, pred$fit, lwd = 2)
lines(ageGrid, pred$fit + 2 * pred$se.fit, lty = "dashed")
lines(ageGrid, pred$fit - 2 * pred$se.fit, lty = "dashed")

# I can choose the knots as previously or let R choose them by itself, plus the bs() function also has a degree argument in order to fit higher degree splines

# To fit a natural spline use the ns() function
fit2 <- lm(wage~ns(age, df = 4), data = w)
pred2 <- predict(fit2, newdata = list(age=ageGrid), se = T)
lines(ageGrid, pred2$fit, col = "red", lwd = 2)

# In order to fit a smoothing spline use the smooth.spline() function
plot(w$age, w$wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(w$age, w$wage, df = 16)
fit2 <- smooth.spline(w$age, w$wage, cv = TRUE)
fit2$df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)

# To perform local regression I use the loess() function
plot(w$age, w$wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage~age, span = .2, data = w)
fit2 <- loess(wage~age, span = .5, data = w)
lines(ageGrid, predict(fit, data.frame(age=ageGrid)), col = "red", lwd = 2)
lines(ageGrid, predict(fit2, data.frame(age=ageGrid)), col = "blue", lwd = 2)

## GAM
# I will predict wage using natural splines of year and age variables, treating education as a qualitative predictor. Since this is just basically a linear model I will use lm() with ns().
gam1 <- lm(wage~ns(year, 4) + ns(age, 5) + education, data = w)

# To do the same with smoothing splines I will need the gam() function from the gam package. With s() I indicate that I want to use smoothing splines
library(gam)
gam2 <- gam(wage~s(year, 4) + s(age, 5) + education, data = w)
par(mfrow = c(1,3))
plot(gam2, se = TRUE, col = "blue") # Since this is a gam object I simply call plot
plot.gam(gam1, se = TRUE, col = "red") # This is always a gam

# Since the function of year seems almost linear I will run an ANOVA test to determine which model is best: exclusion from the model of year, linear function of year or a spline function of year.
gam1 <- gam(wage~s(age, 5)+education, data = w)
gam2 <- gam(wage~year+s(age, 5)+education, data = w)
gam3 <- gam(wage~s(year, 4) + s(age, 5) + education, data = w)
anova(gam1, gam2, gam3, test = "F")
# A GAM with a linear function of year is better than one without year at all. Moreover, there's no evidence that a spline function of year is better than the linear one.

# I can use predict() as usual
pred <- predict(gam2, w)

# To use local regression fits in a GAM I can use the lo() function
gam_lo <- gam(wage~lo(year, age, span = .5) + education, data = w)

# I can plot gam_lo with the akima package
library(akima)
par(mfrow = c(1,2))
plot(gam_lo)

# I can fit a logistic regression GAM as well by simpli using I()
gam_lr <- gam(I(wage>250)~year+s(age, df = 5)+education, data = w, family = binomial)
par(mfrow = c(1,3))
plot(gam_lr, se = T, col = "green")

# Since there are not high earners in the <HS category (as per below) I fit a logistic regression without that category
table(w$education, I(w$wage > 250))
gam_lr <- gam(I(wage > 250)~year+s(age, df = 5)+education, family = binomial, data = w, subset = (education != "1. < HS Grad"))
plot(gam_lr, se = T, col = "green")
