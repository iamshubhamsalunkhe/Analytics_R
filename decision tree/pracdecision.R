rm(list=ls())

library(tree)


data  <- read.csv(file.choose())
head(data)
summary(data)
View(data)


data$householdincome <- data$ApplicantIncome + data$CoapplicantIncome
colnames(data)
data <- data[ ,-c(1,7,8)]
summary(data)
data <- na.omit(data)

data$Dependents <- ifelse((data$Dependents == 0), 0, 1)


colSums(is.na(data))

data$Dependents <- as.factor(data$Dependents)
data$Credit_History <- as.factor(data$Credit_History)
data$LoanAmount <- as.numeric(data$LoanAmount)
data$Loan_Amount_Term <- as.numeric(data$Loan_Amount_Term)






library(tree)
tree.data <- tree(data$Loan_Status ~ . , data = data)

plot(tree.data)
text(tree.data , pretty = 0)

tree.pred = predict(tree.data , data ,type = "class" )
table(tree.pred , data$Loan_Status)

accuracy <- mean(tree.pred == data$Loan_Status)
accuracy

misclassfiacation  <- mean(tree.pred != data$Loan_Status)
misclassfiacation

#################################33

set.seed(2)

test <- read.csv(file.choose())
head(test)

test$householdincome <- test$ApplicantIncome + test$CoapplicantIncome
colnames(test)
test <- test[ ,-c(1,7,8)]
summary(test)
test <- na.omit(test)

test$Dependents <- ifelse((test$Dependents == 0), 0, 1)

str(test)
colSums(is.na(test))

test$Dependents <- as.factor(test$Dependents)
test$Credit_History <- as.factor(test$Credit_History)
test$LoanAmount <- as.numeric(test$LoanAmount)
test$Loan_Amount_Term <- as.numeric(test$Loan_Amount_Term)

test <- test[,c(test$Gender , test$Married ,test$Dependents,test$Education ,
          test$Self_Employed ,test$LoanAmount ,test$Loan_Amount_Term ,
          test$Credit_History ,test$Property_Area ,test$householdincome,
          test$loanstatus)]
head(test)
test.pred <- predict(tree.data ,test = "class")
table(test.pred,test.loan)

