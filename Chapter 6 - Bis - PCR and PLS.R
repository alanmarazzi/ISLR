##### PCR and PLS Regression
### PCR
# To perform PCR I can use pcr() from pls() package
library(pls)
library(ISLR)
Hitters <- na.omit(Hitters)
set.seed(2)
fit <- pcr(Salary~., data = Hitters, scale = TRUE, validation = "CV")
summary(fit)

validationplot(fit, val.type = "MSEP")

# Here we see that the minimum MSE is reached with 16 components, but this would amount to a simple regression considering that we were starting from a total of 19. 
# However, MSE is close to the minimum also with just 1 component! This is a huge dimensionality reduction

# Validation Set 
set.seed(1)
x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary
train <- sample(1:nrow(Hitters), nrow(Hitters)/2)
test <- (-train)
fit <- pcr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(fit, val.type = "MSEP")

# Now the lowest error occurs when M = 7
# Check the test MSE
pred <- predict(fit, Hitters[test, ], ncomp = 7)
mean((pred - Hitters$Salary[test])^2)

# The error rate is competitive with Ridge and Lasso. But the final model with PCR is less interpretable because it doesn't perform variable selection or coefficients.
# Fit PCR on full data using M = 7
fit <- pcr(y~x, scale = TRUE, ncomp = 7)
summary(fit)

#### Partial Least Squares
# To perform PLS I use the plsr() function in the pls package.
set.seed(1)
fit <- plsr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(fit)
# The lowest error occurs when M = 2.
pred <- predict(fit, x[test, ], ncomp = 2)
mean((pred - y[test])^2)
