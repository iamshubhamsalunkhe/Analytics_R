# Support Vector Machine

# In order to ???t an SVM using a non-linear kernel, we once again use the svm() function. However, 
# now we use a di???erent value of the parameter kernel. To ???t an SVM with a polynomial kernel we use
# kernel="polynomial", and to ???t an SVM with a radial kernel we use kernel="radial". In the former 
# case we also use the degree argument to specify a degree for the polynomial kernel and in the latter
# case we use gamma to specify a value of ?? for the radial basis kernel. We ???rst generate some data with 
# a non-linear class boundary, as follows:

library(e1071)

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))

# Plotting the data makes it clear that the class boundary is indeed nonlinear:

plot(x, col=y)

# The data is randomly split into training and testing groups. We then ???t the training 
# data using the svm() function with a radial kernel and ?? = 1:

train=sample(200,100)

svmfit=svm(y~., data=dat[train,], kernel="radial",  gamma=1, cost=1)
plot(svmfit, dat[train,])

# The plot shows that t9he resulting SVM has a decidedly non-linear boundary. The summary() function 
# can be used to obtain some information about the SVM ???t:

summary(svmfit)

# We can see from the ???gure that there are a fair number of training errors in this SVM ???t. If we increase 
# the value of cost, we can reduce the number of training errors. However, this comes at the price of a more 
# irregular decision boundary that seems to be at risk of over???tting the data.

svmfit1=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit1,dat[train,])

# We can perform cross-validation using tune() to select the best choice of ?? and cost for an SVM with a 
# radial kernel:

set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),
                                                                       gamma=c(0.5,1,2,3,4)))
summary(tune.out) #check lowest error

tune.out$best.model

# Therefore, the best choice of parameters involves cost=1 and gamma=2. We can view the test set predictions 
# for this model by applying the predict() function to the data. Notice that to do this we subset the dataframe 
# dat using -train as an index set.

table(true=dat[-train,"y"], pred=predict(tune.out$best.model,newdata=dat[-train,]))

# Application to Gene Expression Data

# We now examine the Khan data set, which consists of a number of tissue samples corresponding 
# to four distinct types of small round blue cell tumors. For each tissue sample, gene expression 
# measurements are available. The data set consists of training data, xtrain and ytrain, and testing 
# data, xtest and ytest. We examine the dimension of the data:

library(e1071)  
library(ISLR)
?Khan
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

# This data set consists of expression measurements for 2,308 genes. The training and test sets consist 
# of 63 and 20 observations respectively.

table(Khan$ytrain)
table(Khan$ytest)

# We will use a support vector approach to predict cancer subtype using gene expression measurements. 
# In this data set, there are a very large number of features relative to the number of observations. 
# This suggests that we should use a linear kernel, because the additional ???exibility that will result 
# from using a polynomial or radial kernel is unnecessary.

dat=data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out=svm(y~., data=dat, kernel="linear",cost=10)
summary(out)
table(out$fitted, dat$y)

# We see that there are no training errors. In fact, this is not surprising, because the large number 
# of variables relative to the number of observations implies that it is easy to ???nd hyperplanes that 
# fully separate the classes. We are most interested not in the support vector classi???er's performance 
# on the training observations, but rather its performance on the test observations.

dat.te=data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

# We see that using cost=10 yields two test set errors on this data.