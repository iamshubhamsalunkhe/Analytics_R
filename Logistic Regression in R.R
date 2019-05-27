rm(list=ls())
# The Stock Market Data

# We will begin by examining some numerical and graphical summaries 
# of the Smarket data, which is part of the ISLR library. This data 
# set consists of percentage returns for the S&P 500 stock index over 
# 1,250 days, from the beginning of 2001 until the end of 2005. For 
# each date, we have recorded the percentage returns for each of the 
# ???ve previous trading days, Lag1 through Lag5. We have also recorded 
# Volume (the number of shares traded).

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
?Smarket
# The cor() function produces a matrix that contains all of the 
# pairwise correlations among the predictors in a data set. The 
# ???rst command below gives an error message because the Direction 
# variable is qualitative.

cor(Smarket)
cor(Smarket[,-9])

# As one would expect, the correlations between the lag variables 
# and today's returns are close to zero. In other words, there appears
# to be little correlation between today's returns and previous days' 
# returns. The only substantial correlation is between Year and Volume. 
# By plotting the data we see that Volume is increasing over time. In 
# other words, the average number of shares traded daily increased from 
# 2001 to 2005.

attach(Smarket)
par(mfrow=c(1,1))
plot(Volume)

# Logistic Regression

# we will ???t a logistic regression model in order to predict Direction 
# using Lag1 through Lag5 and Volume. Theglm() function ???ts, a class of 
# models that includes logistic regression. The syntax generalized linear 
# modelof the glm() function is similar to that of lm(), except that we 
# must pass in the argument family=binomial in order to tell R to run a 
# logistic regression rather than some other type of generalized linear model.

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,
             family=binomial)
summary(glm.fits)

# The smallest p-value here is associated with Lag1. The negative coe???cien
# t for this predictor suggests that if the market had a positive return 
# yesterday, then it is less likely to go up today. However, at a value 
# of 0.15, the p-value is still relatively large, and so there is no clear 
# evidence of a real association between Lag1 and Direction.

# We use the coef() function in order to access just the coe???cients for this
# ???tted model. We can also use the summary() function to access particular 
# aspects of the ???tted model, such as the p-values for the coe???cients.

coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

# The predict() function can be used to predict the probability that the
# market will go up, given values of the predictors. The type="response" 
# option tells R to output probabilities of the form P(Y =1 |X), as opposed 
# to other information such as the logit. If no data set is supplied to the 
# predict() function, then the probabilities are computed for the training 
# data that was used to ???t the logistic regression model. Here we have printed 
# only the ???rst ten probabilities. We know that these values correspond to the 
# probability of the market going up, rather than down, because the contrasts() 
# function indicates that R has created a dummy variable with a 1 forUp.

glm.probs=predict(glm.fits,type="response")
glm.probs[1:10]
contrasts(Direction)

# In order to make a prediction as to whether the market will go up or down 
# on a particular day, we must convert these predicted probabilities into 
# class labels, Up or Down. The following two commands create a vector of 
# class predictions based on whether the predicted probability of a market 
# increase is greater than or less than 0 .5.

glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"

round(glm.probs[1:10],2)
glm.pred[1:10]

# The ???rst command creates a vector of 1,250 Down elements. The second line 
# transforms to Up all of the elements for which the predicted probability 
# of a market increase exceeds 0.5. Given these predictions, the table() 
# function can be used to produce a confusion matrix in order to determine 
# how many observations were correctly or incorrectly classi???ed.

table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)

# The diagonal elements of the confusion matrix indicate correct predictions, 
# while the o???-diagonals represent incorrect predictions. Hence our model
# correctly predicted that the market would go up on 507 days and that it would 
# go down on 145 days, for a total of 507 + 145 = 652 correct predictions. The 
# mean() function can be used to compute the fraction of days for which the 
# prediction was correct. In this case, logistic regression correctly predicted 
# the movement of the market 52.2% ofthetime. At ???rst glance, it appears that the 
# logistic regression model is working a little better than random guessing. However, 
# this result is misleading because we trained and tested the model on the same set 
# of 1,250 observations. In other words, 100???52.2 = 47 .8% is the training error rate 

# To implement this strategy, we will ???rst create a vector corresponding 
# to the observations from 2001 through 2004. We will then use this vector 
# to create a held out data set of observations from 2005.

train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

# The object train is a vector of 1,250 elements, corresponding to the observations 
# in our data set. The elements of the vector that correspond to observations that 
# occurred before 2005 are set to TRUE, whereas those that correspond to observations 
# in 2005 are set to FALSE. The object train is a Boolean vector, since its elements 
# are TRUE and FALSE. Boolean vectors can be used to obtain a subset of the rows or 
# columns of a matrix. For instance, the command Smarket[train,] would pick out a 
# submatrix of the stock market data set, corresponding only to the dates before 
# 2005, since those are the ones for which the elements of train are TRUE. The! 
#   symbol can be used to reverse all of the elements of a Boolean vector. That is, 
# !train is a vector similar to train, except that the elements that are TRUE in 
# train get swapped to FALSE in !train, and the elements that are FALSE in train 
# get swapped to TRUE in !train. Therefore, Smarket[!train,] yields a submatrix 
# of the stock market data containing only the observations for which train is 
# FALSE-that is, the observations with dates in 2005. The output above indicates 
# that there are 252 such observations. 

# We now ???t a logistic regression model using only the subset of the observations 
# that correspond to dates before 2005, using the subset argument. We then obtain 
# predicted probabilities of the stock market going up for each of the days in our 
# test set-that is, for the days in 2005. 

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,
             family=binomial,subset=train)
summary(glm.fits)

glm.probs=predict(glm.fits,Smarket.2005,type="response")

# Notice that we have trained and tested our model on two completely 
# separate data sets: training was performed using only the dates before 
# 2005, and testing was performed using only the dates in 2005. Finally, 
# we compute the predictions for 2005 and compare them to the actual 
# movements of the market over that time period.

glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)


glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,
             subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
(106+35)/252

106/(106+35) #sensitivity 

76/(36+76) # 1- Specificity

# Now the results appear to be a little better: 56% of the daily movements have been correctly predicted. 

library(pROC)
roccurve <- roc(Smarket.2005$Direction ~ glm.probs)
plot(roccurve)
auc(roccurve)

## Dummy Data
set.seed(63126)
n <- 1000
x <- rnorm(n)
pr <- exp(5*x)/(1+exp(5*x))
y <- 1*(runif(n) < pr)
mod <- glm(y~x, family="binomial")

predpr <- predict(mod,type=c("response"))

roccurve <- roc(y ~ predpr)
plot(roccurve)
