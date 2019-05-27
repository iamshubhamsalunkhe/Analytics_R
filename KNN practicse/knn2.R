###############################

library(mlbench)
library(class)
library(caret)



# Loading dataset
?PimaIndiansDiabetes
data("PimaIndiansDiabetes")
data <- PimaIndiansDiabetes
data
sum(is.na(data))
#Splitting Dataset

train <- data[1:600,]
test <- data[601:768,]

head(train)


#########
c1 <- train$diabetes

##knn with k  = 10
#knn with k = 10
model <- knn(train[-9] , test[-9] , c1 , k = 10 , prob = FALSE , use.all = TRUE)
model

##finding confusion matrix (Accuracy) 

table <- table(model , test$diabetes)
table


CM <- confusionMatrix(table)

Accuracy <- CM$overall[1]
Accuracy <- as.numeric(Accuracy)
Accuracy


Error_rate <- 1-Accuracy
Error_rate

######################################################

model_prob <- knn(train[-9] , test[-9] , c1 , k = 10 , prob = TRUE , use.all = TRUE)
summary(model_prob)

model_probonly  <- attr(model_prob,"prob")
model_probonly

model_fixedprob <- ifelse((model_prob == "neg"),      ###   this code gets the neg prob and substract it with  prob 
                          1-attr(model_prob, "prob"),    ### 0.8 neg prob minus 1.0 (whole probablity )which is pos 0.2 
                          attr(model_prob , "prob"))

##########################################

?attr




###################################


model_fixedprob

library(ROCR)

pred <- prediction(model_fixedprob , test$diabetes)
length(model_fixedprob)

length(test$diabetes)


perf <- performance(pred , "tpr" , "fpr")


plot(perf , avg ="threshold" , colorize = T , lwd = 3 , main = "ROC Curve")
abline(0,1 ,lty = 10)


AUCLog2 <- performance(pred , measure = "auc")@y.values[[1]]
cat("AUC:" , AUCLog2 , "n")

#################

?performance
?knn
##################
### K fold with k = 10 

control <- trainControl(method = "cv" , number = 10 ,classProbs = TRUE , summaryFunction =  twoClassSummary)


?trainControl


set.seed(7)

fit_knn <- train(diabetes ~ . , data = PimaIndiansDiabetes , 
                 method = "knn" , metric = "ROC" , trControl = control)

print(fit_knn)


















