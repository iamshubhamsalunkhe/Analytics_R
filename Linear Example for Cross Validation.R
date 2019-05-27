rm(list=ls())

library(MASS)

data <- Boston
# ?Boston

library(psych)

#set.seed(100)

train <- sample(1:506, 400)
# train

test <- -train

training_data <- Boston[train,]
testing_data <- Boston[test,]

model <- lm(medv ~ ., data = training_data)

summary(model)

testing_y <- testing_data$medv

predicted_y <- predict(model, testing_data)

head(predicted_y)

error <- testing_y - (predicted_y)
error_squared <- error^2

MSE = (mean(error_squared))
MSE
RMSE = sqrt(mean(error_squared))
RMSE
