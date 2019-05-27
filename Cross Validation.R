rm(list=ls())
# The Validation Set Approach

library(ISLR)
set.seed(1)
train=sample(392,196)
?Auto

summary(Auto)
dim(Auto)

#cor(Auto[-9])

# library(corrplot)
# corrplot(cor(Auto[-9]), "number")

# library(psych)
# pairs.panels(Auto)

# (Here we use a shortcut in the sample command; see ?sample for 
# details.) We then use the subset option in lm() to ???t a linear 
# regression using only the observations corresponding to the training set. 

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
summary(lm.fit)

# train=sample(392,196)
# training_data <- Auto[train,]
# testing_data <- Auto[-train,]

# We now use the predict() function to estimate the response for 
# all 392 observations, and we use the mean() function to calculate 
# the MSE of the 196 observations in the validation set. Note that 
# the -train index below selects only the observations that are not 
# in the training set.

attach(Auto)
E11 <- mean((mpg-predict(lm.fit,Auto))[-train]^2)
E11

# testing_y <- mpg
# predicted_y <- predict(lm.fit,Auto[-train,])
# error_sqd = (testing_y - predicted_y)^2
# MSE <- mean(error_sqd)

# Therefore, the estimated test MSE for the linear regression ???t 
# is 26.14. We can use the poly() function to estimate the test 
# error for the polynomial and cubic regressions. 

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
summary(lm.fit2)
E12 <- mean((mpg-predict(lm.fit2,Auto))[-train]^2)
E12

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
summary(lm.fit3)
E13 <- mean((mpg-predict(lm.fit3,Auto))[-train]^2)
E13

# mpg[1:5]
# predict(lm.fit3,Auto)[1:5]
# (mpg-predict(lm.fit3,Auto))[1:5]
# (mpg-predict(lm.fit3,Auto))[-train][1:5]
# ((mpg-predict(lm.fit3,Auto))[-train]^2)[1:5]
# mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# 
# length((mpg-predict(lm.fit3,Auto)))
# length((mpg-predict(lm.fit3,Auto))[-train])

E <- c(E11, E12, E13)
E

# These error rates are 19.82 and 19.78, respectively. If we choose 
# a diff???erent training set instead, then we will obtain somewhat 
# diffi???erent errors on the validation set.

set.seed(2)
train=sample(392,196)

lm.fit=lm(mpg~horsepower,subset=train)
summary(lm.fit)
E21 <- mean((mpg-predict(lm.fit,Auto))[-train]^2)
E21

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
summary(lm.fit2)
E22 <- mean((mpg-predict(lm.fit2,Auto))[-train]^2)
E22

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
summary(lm.fit3)
E23 <- mean((mpg-predict(lm.fit3,Auto))[-train]^2)
E23

E1 <- c(E11,E12,E13)
E2 <- c(E21,E22,E23)

Error <- cbind(E1, E2)
Error

# Using this split of the observations into a training set and a 
# validation set, we ???nd that the validation set error rates for 
# the models with linear, quadratic, and cubic terms are 23.30, 
# 18.90, and 19.26, respectively. These results are consistent 
# with our previous ???ndings: a model that predicts mpg using a 
# quadratic function of horsepower performs better than a model 
# that involves only a linear function of horsepower, and there 
# is little evidence in favor of a model that uses a cubic function 
# of horsepower.


# Leave-One-Out Cross-Validation

# The LOOCV estimate can be automatically computed for any generalized 
# linear model using the glm() and cv.glm() functions. In the lab for 
# Chapter 4, we used the glm() function to perform logistic regression 
# by passing in the family="binomial" argument. But if we use glm() to 
# ???t a model without passing in the family argument, then it performs 
# linear regression, just like the lm() function. So for instance, 

glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)

#and 

lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

# yield identical linear regression models. In this lab, we will perform 
# linear regression using the glm() function rather than the lm() function 
# because the latter can be used together with cv.glm(). Thecv.glm() function
# is part of the boot library.

library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit) #no k is mentioned means it is running LOOCV
cv.err$delta
cv.err$K

# The cv.glm() function produces a list with several components. The two numbers 
# in the delta vector contain the cross-validation results. In this case the 
# numbers are identical (up to two decimal places) and correspond to the LOOCV 
# statistic. Below, we discuss a situation in which the two numbers 
# diff???er. Our cross-validation estimate for the test error is approximately 24.23.

# We can repeat this procedure for increasingly complex polynomial ???ts. To automate 
# the process, we use the for() function to initiate a for loop for() for loopwhich 
# iteratively ???ts polynomial regressions for polynomials of order i =1 to i = 5, 
# computes the associated cross-validation error, and stores it in the ith element 
# of the vector cv.error. We begin by initializing the vector. This command will 
# likely take a couple of minutes to run.

cv.error = rep(0,5)
cv.error
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

# we see a sharp drop in the estimated test MSE between the linear 
# and quadratic ???ts, but then no clear improvement from using
# higher-order polynomials.

# k-Fold Cross-Validation

# The cv.glm() function can also be used to implementk-fold CV. 
# Below we use k = 10, a common choice for k, on theAuto data set. 
# We once again set a random seed and initialize a vector in which 
# we will store the CV errors corresponding to the polynomial ???ts 
# of orders one to ten.

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

# the two numbers associated with delta are essentially the same 
# when LOOCV is performed. When we instead perform k-fold CV, then 
# the two numbers associated with delta di???er slightly. The???rst is
# the standard k-fold CV estimate. On this data set, the two estimates are 
# very similar to each other.
