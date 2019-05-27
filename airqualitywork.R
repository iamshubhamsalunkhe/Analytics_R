View (airquality)
?airquality
data <- airquality
plot(airquality$Temp )
model1 <- lm(Month ~ . , data)
summary(model1)
regfit.full=regsubsets(Month~.,data , nvmax = 10)
summary(regfit.full)
reg_summary <- summary(regfit.full)
names(reg_summary)

reg_summary$rsq
reg_summary$adjr2
which.max(reg_summary$rss)
plot(reg_summary$rss,xlab="Number of Variables",ylab="RSq",type="l")

points(19,reg_summary$rsq[19], col="red",cex=2,pch=20)
