fullData <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", header=F) 
#import

names(fullData) <- c("age", "workclass", "fnlwgt", "education", "educationnum", "maritalstatus", "occupation", "relationship", "race", "sex", "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "response")
head(fullData)
summary(fullData)
colnames(fullData)

fullData <- fullData[, c(15, 1:13)] # remove a factor with more than 31 levels.
head(fullData)
summary(fullData)
colnames(fullData)

set.seed(100)

train <- sample (1:nrow(fullData), .8*nrow(fullData)) # training row indices

inputData <- fullData[train, ] # training data

testData <- fullData[-train, ] # test data

#Using the tree package
#Step 1: Build the tree

#Fit a 'tree' model on training data and calculate mis-classification error. 
#There could be a possible over-fitting (rules becoming too specific). 
#Pruning the size of the tree could improve the prediction accuracy to an extent.
#It is worthwhile to note that any factor variables in predictors can have
#a maximum of 32 levels, so consider regrouping if your have more than 32 levels.

library(tree)

treeMod <- tree(response ~ ., data = inputData)  # model the tree, including all the variables

plot(treeMod)  # Plot the tree model

text(treeMod, pretty = 0)  # Add text to the plot

out <- predict(treeMod) # Predict the training data
head(out)

input.response <- as.character(inputData$response) # actuals
head(input.response)

pred.response <- colnames(out)[max.col(out, ties.method = c("first"))] # predicted
head(pred.response)

mean (input.response != pred.response) # misclassification %

#Step 2: Prune the tree
#Your tree may need 'pruning' to avoid over-fitting on test data. 
#Some of the rules that are more specific can be relaxed when 
#a higher level rule is good enough to predict the outcome. 
#It is also possible that you may desire more rules when there is a large number of predictors and data is in large volume. In such cases, it is possible that your predictors are not 'good enough' at explaining the response or you need to check the integrity of data .

#As a thumb rule, pick smaller value for rule size so that the rules are less specific
#(using 'best' parameter) without compromising prediction accuracy.

cvTree <- cv.tree(treeMod, FUN = prune.misclass)  # run the cross validation

plot(cvTree)  # plot the CV

treePrunedMod <- prune.misclass(treeMod, best = 3) 
# set size corresponding to lowest value in below plot. try 4 or 16.

plot(treePrunedMod)

text(treePrunedMod, pretty = 0)

#In the above plot, the lower X axis is the number of terminal nodes 
#and the upper X axis is the number of folds 
#(# of pieces the data is split) in the cross validation. 
#It shows how the misclassification error varies against these. 
#So, this plot is very useful in determining the optimal 
#number of terminal nodes at which the decision tree should
#be pruned. In the above plot, the two red lines mark the two 
#options (# terminal nodes) at which you want to prune the data. 
#Ideally, it is best keep the tree as simple as possible 
#(lesser number of nodes) and the misclassification error 
#as low as possible. Given a choice of number of terminal 
#nodes between 4 - 9, all of which giving the same 
#misclassification error, 4 terminal nodes should be the first choice.

#Step 3: Re-calculate the mis-classification error with pruned tree
#Pruning the tree can help improve the accuracy because the rules 
#are now generic enough to fit larger subgroups

out <- predict(treePrunedMod) # fit the pruned tree

pred.response <- colnames(out)[max.col(out, ties.method = c("random"))] # predicted

mean(inputData$response != pred.response) # Calculate Mis-classification error.

# Predict

out <- predict(treePrunedMod, testData)  # Predict testData with Pruned tree
out
