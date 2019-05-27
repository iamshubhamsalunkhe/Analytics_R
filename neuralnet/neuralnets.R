rm(list = ls())


set.seed(500)

library(MASS)

data <- Boston





##check that no datapoint is missing

apply(data ,2 , function(x) sum(is.na(x)))

index <- sample(1:nrow(data) , round(0.75  * nrow(data)) )
train <- data[index ,]
test <- data[-index,]

lm.fitv <- glm(medv ~ . , data = train)

summary(lm.fitv)

pr.lm <- predict(lm.fitv , test)



mse <- sum((pr.lm - test$medv ) ^ 2) / nrow(test)
mse

##########

maxs <- apply(data, 2 , max) # 2 indicates columns
maxs

mins <- apply(data , 2 , min)
mins





head(scale(data , center = mins , scale = maxs - mins ))

scaled <- as.data.frame(scale(data,center = mins , scale = maxs - mins))


train_ <- scaled[index,]
test_ <- scaled[-index,]


library(neuralnet)


n <- names(train_)  #isolating names 

n


paste(n[n != "medv"] , collapse = " + ")
paste("medv ~" , paste(n[n != "medv"] , collapse = " + "))

as.formula(paste("medv ~" , paste(n[!n %in%"medv"] , collapse = " + ")))

f <- as.formula(paste("medv ~" , paste(n[n != "medv"],collapse = " + ")))
f


nn <- neuralnet(f , data = train_ , hidden = c(5 , 3) , linear.output = T)


pr.nn <- compute(nn , test_ [,1:13])

pr.nn <- pr.nn$net.result * (max(data$medv)-min(data$medv))+min(data$medv)

test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)


mse.nn <- sum((test.r - pr.nn)^2) /nrow(test_)

mse.nn


##compare the two mses


print(paste(mse ,mse.nn))

par(mfrow = c(1,2))

plot(test$medv , pr.nn , col ="red" , main= "Real vs Predicted NN" , 
     pch = 18 , cex = 0.7)


abline(0,1 , lwd = 2)
legend('bottomright' , legend = 'NN' , pch = 18 , col = 'red' , bty = 'n' )

plot(test$medv , pr.lm , col = 'blue' , main = 'Real vs predicted lm' , 
     pch = 18 , cex = 0.7)

abline(0,1,lwd=2)

legend('bottomright' , legend = 'LM' , pch = 18 , col = 'blue' , 
       bty = 'n' , cex = .95)



plot(tes)


####### using boot library #####3


library(boot)
set.seed(200)
lm.fit <- glm(medv ~  ., data = data)

cv.glm(data , lm.fit, K = 10)$delta[1]


set.seed(450)
cv.error <- NULL
k <- 10


library(plyr)

pbar <- create_progress_bar('text')
pbar$init(k)



for(i in 1:k){
  index <- sample(1:nrow(data), round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  nn <- neuralnet(f, data = train.cv, hidden = c(5,2), linear.output = T)
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
  test.cv.r <- (test.cv$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  pbar$step()
  
}


mean(cv.error)

cv.error

