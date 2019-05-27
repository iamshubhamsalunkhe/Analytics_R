rm(list=ls())



library(FSelector) #it compute information gain entropy all formulas are part of Fselector
library(rpart) #rpart does the partioning of your decision tree
library(caret) #caret helps fot splitting data test and train
library(dplyr) #dplyr is used for mutating data , filtering manipulation 
library(rpart.plot) # rpart.plot will plot the data 

library(caTools) #caTools is a package of general manipulation
library(data.tree) #diplays the data 
library(ElemStatLearn)

#loading the csv file which contains the data

data  <- read.csv(file.choose())

#adding applicantincome and coapplicant income and making it household income

data$householdincome <- data$ApplicantIncome + data$CoapplicantIncome
colnames(data)

# removing not meaningful  columns 

data <- data[ ,-c(1,7,8)]
summary(data)
# removing na values
data <- na.omit(data)

#Binning dependents column into 1 and 0

data$Dependents <- ifelse((data$Dependents == 0), 0, 1)


colSums(is.na(data))

str(data)

# getting data into right structure

data$Dependents <- as.factor(data$Dependents)
data$Credit_History <- as.factor(data$Credit_History)
data$LoanAmount <- as.numeric(data$LoanAmount)
data$Loan_Amount_Term <- as.numeric(data$Loan_Amount_Term)

set.seed(123)

tree <- rpart(data$Loan_Status ~ . , data = data)

summary(tree)

prp(tree)

