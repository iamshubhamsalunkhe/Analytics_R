rm(list=ls())
# install.packages("mlbench")
# install.packages("e1071")

library(mlbench)
library(e1071)

#load HouseVotes84 dataset
?HouseVotes84
data(HouseVotes84, package = "mlbench")
View(HouseVotes84)

## Build the model

model <- naiveBayes(Class ~ ., data = HouseVotes84)

predict(model, HouseVotes84[1:10,])

predict(model, HouseVotes84[1:10,], type = "raw")

pred <- predict(model, HouseVotes84)

table(pred, HouseVotes84$Class)

pred

accuracy <- (238+155)/435
accuracy

