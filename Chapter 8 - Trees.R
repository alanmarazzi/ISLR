# ISLR - CH 8 Decision Trees

# We use classification trees to analyze the Carseats data set.
library(tree)

# First let's transform Sales into a binary variable
library(ISLR)
high <-  ifelse(Carseats$Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, high)

# Now fit a classification tree that predicts high by using all variables except Sales
carseats_tree <- tree(high ~ . - Sales, Carseats)
summary(carseats_tree)
plot(carseats_tree)
text(carseats_tree, pretty = 0)

library(ggplot2)
library(ggdendro)
ddata <- dendro_data(carseats_tree)
ggplot()+
    geom_segment(data = ddata$segments, aes(x = x, y = y, xend = xend, yend = yend))+
    geom_text(data = ddata$labels, aes(x, y, label = label), size = 4, vjust = -0.5)+
    geom_text(data = ddata$leaf_labels, aes(x, y, label = label, size = 4, vjust = 1))+
    theme_dendro()

# Validation set
set.seed(2)
train <- sample(nrow(Carseats), 200)
test_carseats <- Carseats[-train, ]
test_high <- high[-train]
carseats_tree <- tree(high~.-Sales, Carseats, subset = train)
pred <- predict(carseats_tree, test_carseats, type = "class")
table(pred, test_high)

# CV to decide how to prune the tree
set.seed(3)
cars_cv <- cv.tree(carseats_tree, FUN = prune.misclass)
cars_cv

par(mfrow = c(1,2))
plot(cars_cv$size, cars_cv$dev, type = "b")
plot(cars_cv$k, cars_cv$dev, type = "b")

carseats_pruned <- prune.misclass(carseats_tree, best = 9)
par(mfrow = c(1,1))
plot(carseats_pruned)
text(carseats_pruned, pretty = 0)

pred <- predict(carseats_pruned, test_carseats, type = "class")
table(pred, test_high)

# Regression Trees
library(MASS)
set.seed(1)
train <- sample(nrow(Boston), nrow(Boston)/2)
boston_tree <- tree(medv~., Boston, subset = train)
summary(boston_tree)
plot(boston_tree)
text(boston_tree, pretty = 0)
boston_cv <- cv.tree(boston_tree)
plot(boston_cv$size, boston_cv$dev, type = "b")
pred <- predict(boston_tree, Boston[-train,])
boston_test <- Boston[-train, "medv"]
plot(pred, boston_test)
abline(0,1)
mean((pred-boston_test)^2)
# The test set MSE is 25, the sqrt(25) is around 5 indicating that this model leads to test predictions around 5k$ the correct one

# Bagging
library(randomForest)
set.seed(1)
boston_bag <- randomForest(
    medv~., 
    Boston, 
    subset = train, 
    mtry = 13, # All 13 predictors will be considered for each split of the trees
    importance = TRUE
    )
boston_bag

pred <- predict(boston_bag, Boston[-train,])
plot(pred, boston_test)
abline(0,1)
mean((pred-boston_test)^2)
# The MSE is almost half the one of the single tree

# RandomForest
# The only difference is in mtry value, per default rf uses p/3 variables for regression and sqrt(p) for classification
set.seed(1)
boston_rf <- randomForest(medv~., Boston, subset = train, mtry = 6, importance = TRUE)
pred <- predict(boston_rf, Boston[-train,])
mean((pred-boston_test)^2)
importance(boston_rf)
varImpPlot(boston_rf)

# Boosting
library(gbm)
set.seed(1)
boston_boost <- gbm(
    medv~., 
    Boston[train,], 
    distribution = "gaussian", # For binary classification we would use "bernoulli"
    n.trees = 5000, # Number of trees tyo grow
    interaction.depth = 4 # Max depth of every tree
    )
summary(boston_boost)

par(mfrow=c(1,2))
plot(boston_boost, i = "rm")
plot(boston_boost, i = "lstat")

pred <- predict(boston_boost, Boston[-train,], n.trees = 5000)
mean((pred-boston_test)^2)

# We can modify the shrinkage parameter, per default .001, let's try with .2
boston_boost <- gbm(medv~., Boston[train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = .2, verbose = FALSE)
pred <- predict(boston_boost, Boston[-train,], n.trees = 5000)
mean((pred-boston_test)^2)
