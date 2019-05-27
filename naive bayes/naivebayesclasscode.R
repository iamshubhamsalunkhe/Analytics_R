library(mlbench)
library(e1071)


#load houseVote84 dataset

?HouseVotes84
data(HouseVotes84 , package = "mlbench")
View(HouseVotes84)


data <- HouseVotes84

train <- HouseVotes84[1:300,]
test <- HouseVotes84[301:435,]

dim(train)
dim(test)
## Build the Model


model <- naiveBayes(Class ~ . , data = HouseVotes84)

predict(model , HouseVotes84 [1:10,])




predict(model , HouseVotes84[1:10,] , type = "raw")

pred <- predict(model  , HouseVotes84)

table(pred , HouseVotes84$Class)

pred

accuracy <- (238+155)/435
accuracy

## model of train

modeltrain <- naiveBayes(Class ~ . , data = train)

predict(modeltrain , test[1:10 ,] )

predict(modeltrain , test[1:10,], type = "raw")

predtrain <- predict(modeltrain , test)

table(predtrain , test$Class)


predtrain

accuracytrain <- (68+52)/(68+52+3+12)
accuracytrain

##Next load the Titanic dataset 

data('Titanic')


#save into a data frame and view it

Titanic_df = as.data.frame(Titanic)
?Titanic

summary(Titanic_df)
View(Titanic)

#creating data from table
repeating_sequence = rep.int(seq_len(nrow(Titanic_df)),Titanic_df$Freq)



sum(Titanic_df$Freq)

#create the dataset by row repetition created

Titanic_dataset = Titanic_df[repeating_sequence,]
head(Titanic_dataset)
nrow(Titanic_dataset)


#we no longer need the  frequency , drop feature 

Titanic_dataset$Freq = NULL
head(Titanic_dataset)


row_names <- 1:nrow(Titanic_dataset)
rownames(Titanic_dataset)
rownames(Titanic_dataset) <- row_names
head(Titanic_dataset)

########################################################



#fitting the  naive bayes model

Naive_Bayes_Model = naiveBayes(Survived ~ .,data = Titanic_dataset)


##Prediction on the dataset
NB_Predictions = predict(Naive_Bayes_Model , Titanic_dataset)


#confusion matrix to check accuracy 

table(NB_Predictions , Titanic_dataset$Survived)

#Create a classification task 

library(mlr)




















