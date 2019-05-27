rm(list=ls())

# Here we apply the best subset selection approach to the Hitters data. We wish to 
# predict a baseball player's Salary on the basis of various statistics associated 
# with performance in the previous year. 
# 
# We check the Salary variable for missing values of the players.

library(ISLR)
attach(Hitters)
?Hitters
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
colSums(is.na(Hitters))

# summary(Hitters)
# 
# Hitters$Salary[is.na(Hitters$Salary)] <- mean(Hitters$Salary, na.rm = TRUE)

# omiting na value

Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

# Hence we see that Salary is missing for 59 players. 

# The regsubsets() function (part of the leaps library) performs best sub
# set selection by identifying the best model that contains a given number 
# of predictors, where best is quanti???ed using RSS. The syntax is the same 
# as for lm(). The summary() command outputs the best set of variables for 
# each model size.

library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)

# An asterisk indicates that a given variable is included in the corresponding model. 
# For instance, this output indicates that the best two-variable model contains only 
# Hits and CRBI. By default, regsubsets() only reports results up to the best eight-
# variable model. But the nvmax option can be used in order to return as many variables 
# as are desired. Here we ???t up to a 19-variable model. 

regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)

summary(regfit.full)

reg.summary=summary(regfit.full)

# The summary() function also returns R2, RSS, adjusted R2, Cp, and BIC. We can examine 
# these to try to select the best overall model.

names(reg.summary)
reg.summary$rsq
reg.summary$adjr2
# we see that the R2 statistic increases from 32%, when only one variable is included in 
# the model, to almost 55%, when all variables are included. As expected, the R2 statistic 
# increases monotonically as more variables are included.

# Plotting RSS, adjusted R2, Cp, and BIC for all of the models at once will help us decide 
# which model to select. Note the type="l" option tells R to connect the plotted points with 
# lines.

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",
     type="l")

# The points() command works like the plot() command, except that it puts points on a plot that 
# has already been created, instead of creating a new plot. The which.max() function can be used 
# to identify the location of the maximum point of a vector. We will now plot a red dot to indicate 
# the model with the largest adjusted R2 statistic

which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

# In a similar fashion we can plot the Cp and BIC statistics, and indicate the models with the 
# smallest statistic using which.min().

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

# The regsubsets() function has a built-in plot() command which can be used to display the selected 
# variables for the best model with a given number of predictors, ranked according to the BIC, Cp, 
# adjusted R2, or AIC. To ???nd out more about this function, type ?plot.regsubsets.

plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

# par(mfrow=c(1,1))

# The top row of each plot contains a black square for each variable selected according to the optimal 
# model associated with that statistic. For instance, we see that several models share a BIC close to 
# ???150. However, the model with the lowest BIC is the six-variable model that contains only AtBat, Hits, 
# Walks, CRBI, DivisionW, andPutOuts. 

# We can use the coef() function to see the coe???cient estimates associated with this model.

coef(regfit.full,6)
  