rm(list=ls())

library(tree)

library(ISLR)


attach(Carseats)
?Carseats
summary(Carseats)
high <- ifelse(Sales <= 8 , "NO" , "Yes")

Carseats <- data.frame(Carseats , high)

tree.carseats <- tree(high ~ . - Sales , Carseats)
summary(tree.carseats)

plot(tree.carseats)

text(tree.carseats , pretty = 0)

tree.carseats



tree.pred = predict(tree.carseats , Carseats ,type = "class" )
table(tree.pred , high)

accuracy <- mean(tree.pred == high)
accuracy

misclassfiacation  <- mean(tree.pred != high)
misclassfiacation

###############################

set.seed(2)
train = sample (1 : nrow(Carseats) , 200)
Carseats.test = Carseats[-train , ]
high.test = high[-train]

tree.carseats = tree(high ~ . -Sales , Carseats , subset = train)

tree.pred = predict(tree.carseats , Carseats.test , type = "class")
table(tree.pred , high.test)
accuracy <- mean(tree.pred == Carseats.test$high)
accuracy

misclassification <- mean(tree.pred != Carseats.test$high)
misclassification



######################
set.seed(3)
cv.carseats = cv.tree(tree.carseats , FUN = prune.misclass)
names(cv.carseats)
#[1] "size"   "dev"    "k"      "method"


cv.carseats$size
cv.carseats$dev


par(mfrow=c(1,2))
plot(cv.carseats$size , cv.carseats$dev , type = "b")

plot(cv.carseats$k , cv.carseats$dev , type = "b")


##we now apply the prune.misclass()


prune.carseats = prune.misclass(tree.carseats , best = 9)
par(mfrow =c(1,1))
plot(prune.carseats)
text(prune.carseats , pretty = 0)


tree.pred = predict(prune.carseats , Carseats.test , type = "class")
table(tree.pred , high.test)

accuracy <- mean(tree.pred == Carseats.test$high)
accuracy

misclassification <- mean(tree.pred != Carseats.test$high)
misclassification


#training data..


library(tree)
library(MASS)


set.seed(1)

train =  sample(1:nrow(Boston) , nrow(Boston)/2)

tree.boston = tree(medv ~ . , Boston , subset = train)

summary(tree.boston)


ncol(Boston)

plot(tree.boston)
text(tree.boston , pretty = 0)
tree.boston

cv.boston = cv.tree(tree.boston)
plot(cv.boston$size , cv.boston$dev , type = "b")

prune.boston =  prune.tree(tree.boston , best = 7)

plot(prune.boston)

text(prune.boston,  pretty = 0)


library(MASS)
set.seed(1)
train = sample(1:nrow(Boston) , nrow(Boston)/2)
ncol(Boston)



library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv ~ . , data = Boston , subset = train , mtry = 13 , importance = TRUE)
bag.boston

yhat.bag = predict(bag.boston , newdata = Boston[-train ,])
boston.test = Boston[-train , "medv"]
mean((yhat.bag-boston.test)^2)

bag.boston = randomForest(medv ~ ., data = Boston , subset = train , mtry = 13 , ntree = 25)

yhat.bag =  predict(bag.boston , newdata = Boston[-train,])
mean((yhat.bag -boston.test)^2)


rf.boston = randomForest(medv ~ ., data = Boston , subset = train , mtry = 6 , importance = TRUE)

yhat.rf = predict(rf.boston , newdata = Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)


