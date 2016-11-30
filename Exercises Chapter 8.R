# ISLR Exercises CH 8

library(randomForest)
library(MASS)

set.seed(1)
train <- sample(nrow(Boston), nrow(Boston)/2)
boston_test <- Boston[-train, "medv"]

rf_mse <- function(mtry, ntree) {
    mean(
        (predict(
            randomForest(
                medv~., Boston[train,], mtry = mtry, ntree = ntree, importance = TRUE), 
            Boston[-train,])
         -boston_test)^2)
}


results <- data.frame(mtry = 2:11, ntree = c(rep(100, 10), rep(200, 10), rep(300, 10), rep(400, 10), rep(500, 10), rep(600, 10), rep(700, 10), rep(800, 10), rep(900, 10), rep(1000,10)), mse = mapply(rf_mse, 2:11, c(rep(100, 10), rep(200, 10), rep(300, 10), rep(400, 10), rep(500, 10), rep(600, 10), rep(700, 10), rep(800, 10), rep(900, 10), rep(1000,10))))

library(ggplot2)
ggplot(results, aes(1:nrow(results), mse))+
    geom_line(color = "blue", size = 1)+
    geom_point(
        data = results[which.min(results$mse),], 
        aes(which.min(results$mse), mse), 
        color = "red", 
        shape = 1, 
        size = 4, 
        stroke = 1.5)+
    theme_bw()
results[which.min(results$mse),]

library(e1071)
set.seed(1)
tune.randomForest(medv~., data = Boston[train,], mtry = 2:11, ntree = seq(100, 1000, 100))
