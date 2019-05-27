rm(list = ls())

#Getting started with Naive Bayes
#Install the package
#install.packages("e1071")
#Loading the library
library(e1071)

# Next load the Titanic dataset
data('Titanic')

# Save into a data frame and view it
Titanic_df=as.data.frame(Titanic)
?Titanic
summary(Titanic_df)
View(Titanic)

# We see that there are 32 observations which represent all possible combinations of 
# Class, Sex, Age and Survived with their frequency. Since it is summarised, this 
# table is not suitable for modelling purposes. We need to expand the table into 
# individual rows. Let's create a repeating sequence of rows based on the frequencies 
# in the table

# Creating data from table
repeating_sequence = rep.int(seq_len(nrow(Titanic_df)), Titanic_df$Freq) 
repeating_sequence

sum(Titanic_df$Freq)

#This will repeat each combination equal to the frequency of each combination

#Create the dataset by row repetition created
Titanic_dataset=Titanic_df[repeating_sequence,]
head(Titanic_dataset,35)
nrow(Titanic_dataset)

#We no longer need the frequency, drop the feature
Titanic_dataset$Freq=NULL
head(Titanic_dataset)

row_names <- 1:nrow(Titanic_dataset)
rownames(Titanic_dataset)
rownames(Titanic_dataset) <- row_names
head(Titanic_dataset)

# The data is now ready for Naive Bayes to process. Let's fit the model

#Fitting the Naive Bayes model
Naive_Bayes_Model = naiveBayes(Survived ~., data=Titanic_dataset)

#What does the model say? Print the model summary
Naive_Bayes_Model

# The model creates the conditional probability for each feature separately. 
# We also have the a-priori probabilities which indicates the distribution of 
# our data. Let's calculate how we perform on the data.

# Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset)

#Confusion matrix to check accuracy
table(NB_Predictions,Titanic_dataset$Survived)

# NB_Predictions      No      Yes
#             No      1364    362
#             Yes     126     349
1364/1490
349/711
(1364+349)/2201

# We have the results! We are able to classify 1364 out of 1490 "No" cases correctly 
# and 349 out of 711 "Yes" cases correctly. This means the ability of Naive Bayes algorithm 
# to predict "No" cases is about 91.5% but it falls down to only 49% of the "Yes" cases resulting 
# in an overall accuracy of 77.8%

#########################################################################

# Conclusion: Can we Do any Better?

# Naive Bayes is a parametric algorithm which implies that you cannot perform 
# differently in different runs as long as the data remains the same. We will, 
# however, look at another implementation of Naive Bayes algorithm using the 'mlr' 
# package. Let us install and load the package and start fitting a model

#Getting started with Naive Bayes in mlr

#Install the package
#install.packages('mlr')

#Loading the library
library(mlr)

# The mlr package consists of a lot of models and works by creating tasks and 
# learners which are then trained. Let's create a classification task using the 
# titanic dataset and fit a model with the naive bayes algorithm.

#Create a classification task for learning on Titanic Dataset and specify the target feature
task = makeClassifTask(data = Titanic_dataset, target = "Survived")
task

#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")
selected_model

#Train the model
NB_mlr = train(selected_model, task)
NB_mlr

# The summary of the model which was printed in e3071 package is stored in 
# learner model. Let's print it and compare

NB_mlr$learner.model

# The a-priori probabilities and the conditional probabilities for the model 
# are similar to the one calculated by e3071 package as was expected. This means 
# that our predictions will also be the same.

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = Titanic_dataset[,1:3]))
summary(predictions_mlr)

##Confusion matrix to check accuracy
table(predictions_mlr[,1],Titanic_dataset$Survived)

# No      Yes
# No    1364    362
# Yes   126     349

# As we see, the predictions are exactly same. The only way to improve is to have 
# more features or more data. Perhaps, if we have more features such as the exact 
# age, size of family, number of parents in the ship and siblings then we may arrive 
# at a better model using Naive Bayes.
# In essence, Naive Bayes has an advantage of a strong foundation build and is very 
# robust. I know of the 'caret' package which also consists of Naive Bayes function but 
# it will also give us the same predictions and probability.

#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes", 
                             predict.threshold = 0.6, 
                             predict.type = 'prob')
selected_model

#Train the model
NB_mlr = train(selected_model, task)
NB_mlr

# The summary of the model which was printed in e3071 package is stored in 
# learner model. Let's print it and compare

NB_mlr$learner.model

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = Titanic_dataset[,1:3]))
summary(predictions_mlr)
head(predictions_mlr)
##Confusion matrix to check accuracy
table(predictions_mlr[,3],Titanic_dataset$Survived)

# We are able to classify 1246 out of 1490 "No" cases correctly 
# and 417 out of 711 "Yes" cases correctly. This means the ability of Naive Bayes algorithm 
# to predict "No" cases is about 83% and of the "Yes" cases 54%  resulting 
# in an overall accuracy of 73.5%

#################################################

# if the threshold is 0.7, 1211 out of 1490 "no" cases correctly
# and 430 out of 711 'yes' cases correctly. This means the ability of Naive Bayes algorithm 
# to predict "No" cases is about 81% and of the "Yes" cases 61%  resulting 
# in an overall accuracy of 74%

