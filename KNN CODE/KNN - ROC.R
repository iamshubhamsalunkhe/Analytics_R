rm(list=ls())

########################

## Split the dataset into training dataset and test dataset according to the following logic
## Training dataset should contain first 600 observations
## Testing dataset should contain the remaining observations (601 - 768 observations)

library(mlbench)
library(class)
library(caret)

#Loading dataset
?PimaIndiansDiabetes
data("PimaIndiansDiabetes")
data <- PimaIndiansDiabetes
data
sum(is.na(data))

#Spliting dataset
train <- data[1:600,]
test <- data[601:768,]

head(train)

normalize <- function(x) 
  {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
  }
 
train[-9] <- normalize(train[-9])

head(train[-9])

 #########################################
## Build a kNN model (with k = 10) on the training dataset in R to predict the diabetes 
## (pos or neg). So here we will consider "diabetes" as Class variable. Then test the 
## model on the testing dataset. Calculate accuracy and error rate.

cl <-  train$diabetes

#Knn with k = 10
model <- knn(train[-9], test[-9], cl, k = 10, prob = FALSE, 
             use.all = TRUE)

model

#finding confusion matrix (Accuracy)
table <- table(model,test$diabetes)
table

CM <- confusionMatrix(table)

Accuracy <- CM$overall[1]
Accuracy <- as.numeric(Accuracy)

Accuracy 

Error_rate <- 1-Accuracy
Error_rate

####################################
model_prob <- knn(train[-9], test[-9], cl, k = 10, prob = TRUE,
                  use.all = TRUE)
summary(model_prob)

model_probonly <- attr(model_prob,"prob")
model_probonly

model_fixedprob <- ifelse((model_prob=="neg"),
                          1-attr(model_prob,"prob"),
                          attr(model_prob,"prob"))
model_fixedprob

library(ROCR)
pred <- prediction(model_fixedprob,test$diabetes)
length(model_fixedprob)
length(test$diabetes)
perf <- performance(pred,"tpr","fpr")

plot(perf, avg= "threshold", colorize=T, lwd=3, main="ROC Curve")
abline(0,1,lty = 10)

AUCLog2 <- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC: ",AUCLog2,"n")

# AUC:  0.7857253 n

#####################################
## Perform k-fold validation (with k = 10) on PimaIndiansDiabetes data.

# K fold with k = 10

control <- trainControl(method="cv", number=10, classProbs=TRUE, 
                        summaryFunction=twoClassSummary)

# will compute the sensitivity, specificity and area under the ROC curve

set.seed(7)

fit_knn <- train(diabetes~., data=PimaIndiansDiabetes, method="knn", 
             metric="ROC", trControl=control)


print(fit_knn)

## k-Nearest Neighbors 

## 768 samples
## 8 predictor
## 2 classes: 'neg', 'pos' 

## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 691, 691, 692, 691, 691, 692, ... 
## Resampling results across tuning parameters:
  
##   k  ROC        Sens   Spec     
##  5  0.7423675  0.820  0.5185185
##  7  0.7625997  0.826  0.5441595
##  9  0.7759416  0.832  0.5292023
 
## ROC was used to select the optimal model using  the largest value.
## The final value used for the model was k = 9.