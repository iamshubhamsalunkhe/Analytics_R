rm(list=ls())

## svm practise

##credit card fraud dataset from kaggle 
##goal is to classify fraudulent transaction 
##load the dataset file 
library(e1071)
All_data <-  read.csv("creditcard.csv")
head(All_data)
dim(All_data)

##dividing into train and test
index <- sample(1:nrow(All_data),round(0.75*nrow(All_data)))
train <- All_data[index,]
test <- All_data[-index,]
##variale time length differs it will make problem when building the model
##for now we are not considering it later we will think of it
train1 <- train[,-1]
colSums(train1)
#checking for missing and na values
colSums(is.na(All_data))
summary(All_data)
View(All_data)

##fitting a svm mode

model1 <- svm(Class ~ . , data = train , kernel = 'linear', gamma = 1 , cost = 1 )

plot(train$Class)





svm_tune <- tune(svm, train1$Class ~ ., data = train1,ranges = list(epsilon = seq(0,1,0.01), cost = 2^(2:9)))








