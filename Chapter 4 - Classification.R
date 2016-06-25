#### ISLR: Chapter 4 Classification

## LAB
library(ISLR)
names(Smarket) # Dataset of S&P 500 records of trading volume and value
dim(Smarket)
summary(Smarket)

# Look for correlations in the data
library(corrplot)
corrplot(cor(Smarket[,-9]), addCoef.col = "black", addCoefasPercent = TRUE)

# There is only correlation between Volume and Year, let's look at the Volume variable
plot(Smarket$Volume)

## Logistic Regression
# We will try to predict Direction (up or down) using all variables
lr <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(lr)

# The result is not that good, the lowest p-value is for Lag1 at 0.15, but still pretty large
# Let's look at the coefficients
coef(lr)

# Lok at the predictions of the model
lr_prob <- predict(lr, type = "response")
head(lr_prob, 10)

# The probabilities are associated to the "Up" variable as we can see with contrasts()
contrasts(Smarket$Direction)

# We want to convert the predicted probabilities into Up or Down variables
lr_pred <- rep("Down", 1250)
lr_pred[lr_prob > .5] <- "Up"

# Now build a confusion matrix to understand if the model is really that bad
table(lr_pred, Smarket$Direction)
mean(lr_pred == Smarket$Direction)

# The model is able to predict correctly in 52.2% of days, but we trained and tested it on the same data. Now let's try the same but splitting it: all points before 2005 for training and 2005 for testing.
train <- (Smarket$Year < 2005)
test <- Smarket[!train,]
direction_test <- Smarket$Direction[!train]

# Now let's run a logistic regression on the training set and try it on the test set
lr <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
lr_probs <- predict(lr, test, type = "response")
lr_pred <- rep("Down", 252)
lr_pred[lr_probs > .5] <- "Up"
table(lr_pred, direction_test)
mean(lr_pred != direction_test) # Test set error rate

# The test set error rate is .52 which is worse than random guessing!!!! The only thing we can try is to remve some variable to reduce variance and see if we can have some improvement.
lr <- glm(Direction~Lag1+Lag2, data = Smarket, family = binomial, subset = train)
lr_probs <- predict(lr, test, type = "response")
lr_pred <- rep("Down", 252)
lr_pred[lr_probs > .5] <- "Up"
table(lr_pred, direction_test)
mean(lr_pred != direction_test)

# We see a small improvement in the error rate, especially for the "Up" prediction

### LDA (Linear Discriminant Analysis)
# We can fit a LDA with the lda() function in the MASS package
library(MASS)
ldaFit <- lda(Direction~Lag1+Lag2,data=Smarket, subset=train)
ldaFit

# The prior tells us the prior probabilities for the variables: 49.2% of the times the market went down
ldaPred <- predict(ldaFit, test)
ldaClass <- ldaPred$class
table(ldaClass, direction_test)


### QDA (Quadratic Discriminant Analysis)
# The qda() function is always in the MASS package
qdaFit <- qda(Direction ~ Lag1+Lag2, data = Smarket, subset = train)
qdaFit
qdaClass <- predict(qdaFit, test)$class
table(qdaClass, direction_test)
mean(qdaClass != direction_test)

# The QDA reaches an impressive error rate of 40%, which is really remarkable considering we're modeling stock market data.

### KNN
# knn() is part of the class library. KNN works differently from the other classifiers: we will need a matrix with the predictors for the trainng data, a matrix with the predictors for the test, a vector containing the class labels for the training observations and a value for K to be used for splitting the data into K+1 groups.
library(class)
trainX <- cbind(Smarket$Lag1,Smarket$Lag2)[train,] # Training matrix
testX <- cbind(Smarket$Lag1, Smarket$Lag2)[!train,] # Test matrix
trainClass <- Smarket$Direction[train]

# Now let's fit KNN, but remember to set the seed!
set.seed(1)
pred <- knn(trainX, testX, trainClass, k = 1)
table(pred, direction_test)
mean(pred != direction_test)

# The result is equal to random guessing, which is really bad. But probably K = 1 is overly flexible for the data. Let's try with K = 3
pred <- knn(trainX, testX, trainClass, k = 3)
table(pred, direction_test)
mean(pred != direction_test)

# Apply KNN to the Caravan dataset: 85 predictors with demographic characteristics for more than 5800 individuals, the response variable is the purchase of an insurance policy
dim(Caravan)
summary(Caravan$Purchase)
sum(Caravan$Purchase == "Yes")/sum(Caravan$Purchase == "No")

# In order to have the same comparable scale for all variables we standardize the data in a way that all variables will have mean 0 and SD 1
standard <- scale(Caravan[,-86]) # Column 86 is Purchase, we don't standardize it
var(Caravan[,1])
var(standard[,1])

# Split the set in test and train
testX <- standard[1:1000,]
trainX <- standard[1001:nrow(standard),]
trainY <- Caravan$Purchase[1001:nrow(Caravan)]
testY <- Caravan$Purchase[1:1000]

set.seed(1)
pred <- knn(trainX, testX, trainY, k = 1)
mean(testY != pred)
mean(testY != "No")

# 12% can look as a good error rate, but since only 6% of the individuals ever buy insurance we would lower the error to 6% by always predicting "No".
table(pred, testY)
9/(68+9)
# But if we look at the error rate for the "Yes" class we have 11,7% of customers correctly predicted, this would double rate of correct estimate against random guessing
# Let's try with different values of K
pred <- knn(trainX, testX, trainY, k = 3)
table(pred, testY)
5/26
pred <- knn(trainX, testX, trainY, k = 5)
table(pred, testY)
4/15
# With k=5 we reach a 26% of correct predictions for the "Yes" class

# Let's compare the results with a logistic regression
test <- 1:1000
lr <- glm(Purchase~., data = Caravan, family = binomial, subset = -test)
prob <- predict(lr, Caravan[test,], type = "response")
pred <- rep("No", 1000)
pred[prob > .5] <- "Yes"
table(pred, testY)

# With a 50% probability we are unable to predict correctly any "Yes" response, let's try with a lower probability
pred <- rep("No", 1000)
pred[prob > .25] <- "Yes"
table(pred, testY)
# In this way we reach 33% of correct predictions!!!