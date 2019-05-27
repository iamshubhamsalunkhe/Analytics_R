#The validaton Set Approach 

library(ISLR)
set.seed(1)
train = sample(392,196)
?Auto

summary(Auto)
dim(Auto)




#cor(Auto[-9])

#library(corrplot)
#corrplot(cor((Auto[-9]),"number") )


attach(Auto)

lm.fit = lm(mpg ~ horsepower, data = Auto , subset = train)
summary(lm.fit)

E11 <- mean((mpg - predict(lm.fit , Auto))[-train]^2)
E11

 
lm.fit2 = lm(mpg ~ poly (horsepower , 2),data = Auto , subset = train)
summary(lm.fit2)

E12 <- mean((mpg-predict(lm.fit2 , Auto))[-train]^2)
E12


lm.fit3 = lm(mpg ~ poly(horsepower , 3), data = Auto , subset = train)
summary(lm.fit3)


E13 <- mean((mpg-predict(lm.fit3 , Auto))[-train]^2)
E13

E <- c(E11,E12,E13)
E

#mpg[1:5]
#predict(lm.fit3 , Auto)[1:5]
#(mpg-predict(lm.fit3 , Auto))[1:5]
#-train[1:5]
#(mpg-predict(lm.fit3 , Auto))[-train][1:5]
#((mpg-predict(lm.fit3 , Auto))[-train]^2)[1:5]
#mean(((mpg-predict(lm.fit3 , Auto))[-train]^2))
#length((mpg-predict(lm.fit3,Auto)))
#lenght((mpg-predict(lm.fit3,Auto))[-train])
###############
set.seed(2)

lm.fit_ = lm(mpg ~ horsepower, data = Auto , subset = train)
summary(lm.fit)

E11_ <- mean((mpg - predict(lm.fit_ , Auto))[-train]^2)
E11_

lm.fit_2 = lm(mpg ~ poly (horsepower , 2),data = Auto , subset = train)
summary(lm.fit_2)

E12_ <- mean((mpg-predict(lm.fit_2 , Auto))[-train]^2)
E12_

lm.fit_3 = lm(mpg ~ poly(horsepower , 3), data = Auto , subset = train)
summary(lm.fit_3)


E13_ <- mean((mpg-predict(lm.fit_3 , Auto))[-train]^2)
E13_


E_ <- c(E11_,E12_,E13_)
E_


Error <- cbind(E,E_)
Error



##leave one out Cross -Validation

glm.fit = glm(mpg ~ horsepower , data = Auto)
coef(glm.fit)


#and

lm.fit = lm(mpg~horsepower , data = Auto )
coef(lm.fit)

library(boot)

glm.fit = glm(mpg ~ horsepower , data = Auto )
cv.err = cv.glm(Auto , glm.fit) # no k is mentioned means it is running LOOOCV
cv.err$delta
cv.err$K



cv.error = rep (0,5)
cv.error

for(i in 1:5){
  glm.fit = glm(mpg ~ poly(horsepower , i ), data = Auto)
  cv.error[i] = cv.glm(Auto , glm.fit)$delta[1]
}

cv.error




# K- fold

set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10) {
  glm.fit= glm(mpg ~ poly(horsepower , i ), data = Auto)
  cv.error.10[i] = cv.glm(Auto , glm.fit , K = 20)$delta[1]
}

cv.error.10

?poly


?cv.glm

library(MASS)
?iris

colnames(iris)

cor1 <- cor(iris$Sepal.Length , iris$Sepal.Width)
cor1

cor2 <- cor(iris$Petal.Length , iris$Petal.Width)
cor2

cor3 <- cor(iris$Petal.Length , iris$Sepal.Length)
cor3


cor4 <- cor(iris$Petal.Width ,iris$Sepal.Width)
cor4

cor_ <- c(cor1, cor2, cor3,cor4)
cor_
library(corrplot)
corrplot(iris)[-5]
summary(iris)


library(class)

normalize <- function(x)
{
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
  
  
}

iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
summary(iris_norm)

############################################

set.seed(1234)
ind <- sample(2, nrow(iris) , replace = TRUE , prob = c(0.67 , 0.33))
table(ind)



iris.training <- iris[ind == 1,1:4]
iris.training


iris.testing <- iris[ind == 2,1:4]
iris.testing

View(iris)


iris.trainlabels <- iris[ind == 1 , 5]
iris.testlabels <- iris[ind == 2, 5]
#####################


#k = 3



iris.pred <- knn(train = iris.training ,
                 test = iris.testing,
                 cl = iris.trainlabels,
                 k = 3)

iris.pred



#?knn
iris.testlabels

sum(iris.pred == iris.testlabels)
mean(iris.pred == iris.testlabels)
table(iris.pred,iris.testlabels)


















