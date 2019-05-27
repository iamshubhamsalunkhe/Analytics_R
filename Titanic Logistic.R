rm(list = ls())
## Load Titanic library to get the dataset
#install.packages("titanic")
library(titanic)

?titanic
## Load the datasets
data("titanic_train")
data("titanic_test")

# titanic_train <- read.csv('titanic_train.csv')
# 
# titanic_test <- read.csv("titanic_test.csv")

summary(titanic_train)
summary(titanic_test)

## Setting Survived column for test data to NA
titanic_test$Survived <- NA

## Combining Training and Testing dataset
complete_data <- rbind(titanic_train, titanic_test)

## Check data structure
str(complete_data)

## Let's check for any missing values in the data
is.na(complete_data)
sum(is.na(complete_data))
colSums(is.na(complete_data))

## Checking for empty values
colSums(complete_data=='')

## Missing values imputation

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# x <-  c(51:60,9,9,9,9,NA,NA,NA)
# x
# x = x[!is.na(x)]
# x
# ux <- unique(x)
# ux
# match(x, ux)
# tabulate(match(x, ux))
# which.max(tabulate(match(x, ux)))
# ux[which.max(tabulate(match(x, ux)))]

Mode(complete_data$Embarked)

complete_data$Embarked[complete_data$Embarked==""] <- "S"

complete_data$Age[is.na(complete_data$Age)] <- 
  median(complete_data$Age,na.rm=T)

## Removing Cabin as it has very high missing values, passengerId, Ticket and Name are not required
library(dplyr)
titanic_data <- complete_data %>% 
  select(-c(Cabin, PassengerId, Ticket, Name))

titanic_data1 <- select(complete_data, -c(Cabin, PassengerId, Ticket, Name))
head(titanic_data1)

## Check number of uniques values for each of the column to find out columns which we can convert to factors
sapply(complete_data, function(x) length(unique(x)))

## Converting "Survived","Pclass","Sex","Embarked" to factors
for (i in c("Survived","Pclass","Sex","Embarked")){
  titanic_data[,i]=as.factor(titanic_data[,i])
}

head(titanic_data)
summary(titanic_data)

View(titanic_data)
## Create dummy variables for categorical variables
library(dummies)
titanic_data <- dummy.data.frame(titanic_data, 
                                 names=c("Pclass","Sex","Embarked"), 
                                 sep="_")

head(titanic_data)
dim(titanic_data)

## Splitting training and test data
train <- titanic_data[1:667,]
test <- titanic_data[668:889,]

## Model Creation
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

## Model Summary
summary(model)

# # Using anova() to analyze the table of devaiance
# anova(model, test="Chisq")

## Predicting Test Data
result <- predict(model,newdata=test,type='response')
result <- ifelse(result > 0.5,1,0)

## Confusion matrix and statistics
# install.packages('ModelMetrics', dependencies = T)
# install.packages('caret', dependencies = T)
# library(caret)
# confusionMatrix(result, set=test$Survived, relative = T)

mean(result == test$Survived)

## ROC Curve and calculating the area under the curve(AUC)
library(ROCR)
predictions <- predict(model, newdata=test, type="response")
ROCRpred <- prediction(predictions, test$Survived)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), 
     print.cutoffs.at = seq(0,1,0.1))

roccurve <- roc(test$Survived ~ result)
auc(roccurve)
