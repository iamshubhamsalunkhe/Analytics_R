rm(list = ls())

library(e1071)

#load the dataset 

data <- read.csv(file.choose())

head(data)
# correcting structure of data 

str(data)
data$Dependents <- as.factor(data$Dependents)

data$LoanAmount <- as.numeric(data$LoanAmount)

data$Loan_Amount_Term <- as.numeric(data$Loan_Amount_Term)

#combining columns income into household income


data$Household_income <- data$ApplicantIncome + data$CoapplicantIncome

head(data$Household_income)

# removing na values 

colSums(is.na(data))

data <- na.omit(data)


# bining dependency column

data$Dependents <- ifelse(data$Dependents == 0 , 0,1)
head(data$Dependents)

# bining property area column

data$Property_Area <- ifelse(data$Property_Area == "Rural" , 0,
                             ifelse(data$Property_Area == "Urban" , 1 , 2))


View(data)

#dividing data into  train and test


dim(data)

set.seed(1)
# splitting data into train and test

train <- data[1:400,]

test <- data [401:529,]


colnames(train)

# removing unnecessary columns from train to build better model


train$Loan_ID <- NULL 
train$ApplicantIncome <- NULL
train$CoapplicantIncome <- NULL
test$Loan_ID <- NULL
test$ApplicantIncome <- NULL
test$CoapplicantIncome <- NULL
colnames(train)
colnames(test)
#building naivebayes model  


data_model <- naiveBayes(train$Loan_Status ~ ., data = train)

data_model


#predicting test data by model builded with train data

model_pred <- predict(data_model ,test)

#confusion matrix 

table(model_pred , test$Loan_Status)
22+5+16+86

(22+86)/129

#0.8372093


# can we do better than this 

#loading library mlr
# The mlr package consists of a lot of models and works by creating tasks and 
# learners which are then trained. Let's create a classification task using the 
# titanic dataset and fit a model with the naive bayes algorithm.

library(mlr)

#create a classification task for learning on test data and specify the target feature

task = makeClassifTask(data = test , target = "Loan_Status")

task

#Initialize the Naive Bayes classifier

selected_model = makeLearner("classif.naiveBayes")
selected_model


#train the model
NB_mlr = train(selected_model , task)
NB_mlr

# The summary of the model which was printed in e3071 package is stored in 
# learner model. Let's print it and compare

NB_mlr$learner.model

# The a-priori probabilities and the conditional probabilities for the model 
# are similar to the one calculated by e3071 package as was expected. This means 
# that our predictions will also be the same.

#Predict on the dataset without passing the target feature
# The a-priori probabilities and the conditional probabilities for the model 
# are similar to the one calculated by e3071 package as was expected. This means 
# that our predictions will also be the same.

#Predict on the dataset without passing the target feature
#target feature is Loan_Status

NBmlr_pred <- as.data.frame(predict(NB_mlr , newdata = test[,-10]))


summary(NBmlr_pred)


##Confusion matrix to check accuracy
table(NBmlr_pred [,1],test$Loan_Status)

#N  Y
#N 23  3
#Y 15 88

22+3+88+15
(22+88)/128

# 0.859375 the accuracy is lifted as we  seee 

# giving threshold 

selected_model <- makeLearner("classif.naiveBayes",
                              predict.threshold = 0.6,
                              predict.type = 'prob')

selected_model
 

NB_mlr <- train(selected_model,task)
NB_mlr


#let see learner.model which has summary  

NB_mlr$learner.model

##Predict on the dataset without passing the target feature

NBmlr_pred <- as.data.frame(predict(NB_mlr ,newdata = test[,-10]))

summary(NBmlr_pred)

head(NBmlr_pred)

#confusion matrix tp check accuracy 

table(NBmlr_pred[,3],test$Loan_Status)

#N  Y
#N 23  3
# 15 88
# 23+3+15+88
# 129
#(23+88)/129
# 0.8604651













