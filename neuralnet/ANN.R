rm(list=ls())

## Neural networks resemble black boxes a lot: explaining their outcome is much more difficult than 
## explaining the outcome of simpler model such as a linear model. Therefore, depending on the kind 
## of application you need, you might want to take into account this factor too. Furthermore, as you 
## have seen above, extra care is needed to fit a neural network and small changes can lead to 
## different results.

set.seed(500)
library(MASS)
data <- Boston

## check that no datapoint is missing
apply(data,2,function(x) sum(is.na(x)))

## randomly splitting the data into a train and a test set, then we fit a linear regression model and
## test it on the test set.

## The sample(x,size) function simply outputs a vector of the specified size of randomly selected 
## samples from the vector x. By default the sampling is without replacement: index is essentially 
## a random vector of indeces.

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)

## Since we are dealing with a regression problem, we are going to use the mean squared error (MSE) 
## as a measure of how much our predictions are far away from the real data.

MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
MSE.lm
## 21.6297593507225

## Before fitting a neural network, some preparation need to be done. Neural networks are not that 
## easy to train and tune.
head(data)

maxs <- apply(data, 2, max) # 2 indicates columns
maxs
mins <- apply(data, 2, min)
mins

head(scale(data, center = mins, scale = maxs - mins))
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

## scale returns a matrix that needs to be coerced into a data.frame

##Since this is a toy example, we are going to use 2 hidden layers with this configuration: 13:5:3:1. 
## The input layer has 13 inputs, the two hidden layers have 5 and 3 neurons and the output layer has, 
## of course, a single output since we are doing regression.

library(neuralnet)
n <- names(train_)
n
paste(n[n != "medv"], collapse = " + ")
paste("medv ~", paste(n[n != "medv"], collapse = " + "))
as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = "+")))

f <- as.formula(paste("medv ~", paste(n[n != "medv"], collapse = " + ")))
f

nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

## Notes:
## For some reason the formula y~. is not accepted in the neuralnet() function. You need to first 
## write the formula and then pass it as an argument in the fitting function.
## The hidden argument accepts a vector with the number of neurons for each hidden layer, while the 
## argument linear.output is used to specify whether we want to do regression linear.output=TRUE or 
## classification linear.output=FALSE

## plot(nn)

## The black lines show the connections between each layer and the weights on each connection while 
## the blue lines show the bias term added in each step. The bias can be thought as the intercept of 
## a linear model.

pr.nn <- compute(nn,test_[,1:13])

## Now we can try to predict the values for the test set and calculate the MSE. Remember that the net 
## will output a normalized prediction, so we need to scale it back in order to make a meaningful 
## comparison (or just a simple prediction).

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+
  min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
MSE.nn

## compare the two MSEs

print(paste(MSE.lm,MSE.nn))

## the net is doing a better work than the linear model at predicting medv. Once again, be careful 
## because this result depends on the train-test split performed above. Below, after the visual plot, 
## we are going to perform a fast cross validation in order to be more confident about the results.

## A first visual approach to the performance of the network and the linear model on the test set is
## plotted below

par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',
     pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',
     pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', 
       bty='n', cex=.95)

## By visually inspecting the plot we can see that the predictions made by the neural network are 
## (in general) more concetrated around the line (a perfect alignment with the line would indicate a 
## MSE of 0 and thus an ideal perfect prediction) than those made by the linear model.

par(mfrow=c(1,1))
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN & LM',
     pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

## We are going to implement a fast cross validation using a for loop for the neural network and 
## the cv.glm() function in the boot package for the linear model.

library(boot)
set.seed(200)
lm.fit <- glm(medv~.,data=data)
cv.glm(data,lm.fit,K=10)$delta[1]

## the 10 fold cross validated MSE for the linear model: 23.83560156

## there is no built-in function in R to perform cross validation on this kind of neural network
## the data is split in the following way: 90% train set and 10% test set in a random way for 10 times.
## Initializing a progress bar using the plyr library to keep an eye on the status of the process 
## since the fitting of the neural network may take a while.

set.seed(450)
cv.error <- NULL

k <- 10

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

for(i in 1:k){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+
    min(data$medv)
  
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+
    min(data$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  
  pbar$step()
}

## the average MSE
mean(cv.error)
## 10.32697995

cv.error
## 17.640652805  6.310575067 15.769518577  5.730130820 10.520947119  6.121160840
## 6.389967211  8.004786424 17.369282494  9.412778105

## plot the results as a boxplot
boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)

## the average MSE for the neural network (10.33) is lower than the one of the linear model although 
## there seems to be a certain degree of variation in the MSEs of the cross validation. This may 
## depend on the splitting of the data or the random initialization of the weights in the net. By 
## running the simulation different times with different seeds you can get a more precise point 
## estimate for the average MSE.

## Neural networks resemble black boxes a lot: explaining their outcome is much more difficult than 
## explaining the outcome of simpler model such as a linear model. Therefore, depending on the kind 
## of application you need, you might want to take into account this factor too. Furthermore, as you 
## have seen above, extra care is needed to fit a neural network and small changes can lead to 
## different results.