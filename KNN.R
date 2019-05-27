rm(list = ls())

library(MASS)
?iris

colnames(iris)
cor1 <- cor(iris$Sepal.Length,iris$Sepal.Width)
cor1

cor2 <- cor(iris$Petal.Length,iris$Petal.Width)
cor2

cor3 <- cor(iris$Petal.Length,iris$Sepal.Length)
cor3

cor4 <- cor(iris$Petal.Width,iris$Sepal.Width)
cor4

summary(iris)

# View(iris)

library(class)

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}

iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
summary(iris_norm)

######################

set.seed(1234)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.67, 0.33))
table(ind)

iris.training <- iris[ind==1,1:4]
iris.training

iris.testing <- iris[ind==2,1:4]
iris.testing

iris.trainLabels <- iris[ind==1,5]
iris.testLabels <- iris[ind==2,5]

#########################
#K = 3

iris.pred <- knn(train = iris.training, 
                 test = iris.testing, 
                 cl = iris.trainLabels, 
                 k = 3)

iris.pred
iris.testLabels

sum(iris.pred == iris.testLabels)
mean(iris.pred == iris.testLabels)
table(iris.pred, iris.testLabels)

#########################################################
fgl
str(fgl)

library(ggvis)

fgl %>% ggvis(~RI, 
               ~Mg, 
               fill = ~type) %>% layer_points()

fgl %>% ggvis(~RI, 
              ~Si, 
              fill = ~type) %>% layer_points()

fgl %>% ggvis(~Al, 
              ~Mg, 
              fill = ~type) %>% layer_points()

fgl %>% ggvis(~Na, 
              ~Mg, 
              fill = ~type) %>% layer_points()

fgl %>% ggvis(~Na, 
              ~Fe, 
              fill = ~type) %>% layer_points()
set.seed(1234)
ind <- sample(2, nrow(fgl), replace = TRUE, prob = c(0.70, 0.30))
(ind)
table(ind)

summary(fgl)

fgl.training <- fgl[ind==1,1:9]
fgl.training

fgl.testing <- fgl[ind==2,1:9]
fgl.testing

fgl.trainLabels <- fgl[ind==1,10]
fgl.testLabels <- fgl[ind==2,10]

#K = 1
fgl.pred1 <- knn(train = fgl.training, 
                 test = fgl.testing, 
                 cl = fgl.trainLabels, k = 1)

fgl.pred1
fgl.testLabels

a1 <- sum(fgl.pred1 == fgl.testLabels)
accuracy1 <- (a1/70)
accuracy1
table(fgl.pred1,fgl.testLabels)

#K = 5
fgl.pred5 <- knn(train = fgl.training, 
                 test = fgl.testing, 
                 cl = fgl.trainLabels, k = 5)

fgl.pred5
fgl.testLabels

a5 <- sum(fgl.pred5 == fgl.testLabels)
accuracy5 <- (a5/70)
accuracy5

#K = 7
fgl.pred7 <- knn(train = fgl.training, 
                 test = fgl.testing, 
                 cl = fgl.trainLabels, k = 7)

fgl.pred7
fgl.testLabels

a7 <- sum(fgl.pred7 == fgl.testLabels)
accuracy7 <- (a7/70)
accuracy7

#K = 20
fgl.pred20 <- knn(train = fgl.training, 
                 test = fgl.testing, 
                 cl = fgl.trainLabels, k = 20)

fgl.pred20
fgl.testLabels

a20 <- sum(fgl.pred20 == fgl.testLabels)
accuracy20 <- (a20/70)
accuracy20

#K = 50
fgl.pred50 <- knn(train = fgl.training, 
                  test = fgl.testing, 
                  cl = fgl.trainLabels, k = 50)

fgl.pred50
fgl.testLabels

a50 <- sum(fgl.pred50 == fgl.testLabels)
accuracy50 <- (a50/70)
accuracy50