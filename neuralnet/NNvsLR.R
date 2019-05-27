rm(list=ls())

## building a linear regression model on boston dataset 



library(MASS)
data <- Boston


colSums(is.na(data))


index <- sample(1:nrow(data),round(0.75*nrow(data)))
colnames(data)
train <- data[index, c(6,13,14)]
test <- data[-index,c(6,13)]
test_full_lm <- data[-index,]


lm.fit <- lm(medv ~ rm +lstat , data = train)
summary(lm.fit)

pr.lm <- predict(lm.fit , test)

mse.lm <- sum((pr.lm - test_full_lm$medv)^2)/nrow(test)
mse.lm


########################3
head(data)


maxs <- apply(data, 2, max)
maxs


mins <- apply(data , 2 , min)
mins



head(scale(data , center = mins , scale = maxs - mins))

scaled <- as.data.frame(scale(data , center = mins , scale = maxs - mins))


train_ <-scaled[index,c(6,13,14)]
test_<- scaled[-index,c(6,13,14)]
test_full_nn <- scaled[-index,]


######

library(neuralnet)


n <- names(train_)
n




paste(n[n != "medv"] , collapse = " + ")
paste("medv ~" , paste(n[n != "medv"] , collapse = " + "))
as.formula(paste("medv ~" , paste(n[n != "medv"] , collapse = " + ")))


f <- as.formula(paste("medv ~" , paste(n[n != "medv"] , collapse = " + ")))

f



nn <- neuralnet(f , data = train_ , hidden = c(5,3), linear.output = T)

pr.nn <- compute(nn , test_)



pr.nn <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)

test.r <- (test_full_nn$medv)*(max(data$medv)-min(data$medv))+min(data$medv)


mse.nn <- mean((test.r - pr.nn)^2)
mse.nn

test.r[1:5]
pr.nn[1:5]

paste(mse.lm,mse.nn)



##### 
























