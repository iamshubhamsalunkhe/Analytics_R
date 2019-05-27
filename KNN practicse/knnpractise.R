### KNN practise
library(mlbench)
library(class)
library(caret)





dataknn <- read.csv(file.choose())
dataknn <- read.csv("Prostate_Cancer.csv")

colnames(dataknn)
colSums(is.na(dataknn))
View(dataknn)
attach(dataknn)
corknn <- cor(dataknn$radius , dataknn$texture)
corknn

corknn1 <- cor(dataknn$perimeter , dataknn$area)
corknn1
str(dataknn)
library(corrplot)

df <- read.csv("Prostate_Cancer.csv",1)
#set column as index
attach(df)
rownames(df) <- df$colnm
df2<-subset(df, select = -c(colnm) )

summary(dataknn)
head(dataknn)
corknn <- cor(dataknn[, -c(1,2)])

library(corrplot)
corrplot(corknn )



library(corrplot)
corrplot(corknn )


dim(dataknn)

### splitting dataset train and test 


train <- dataknn[1:70,-1]
test <- dataknn[71:100,-1]


head(train)

normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}
 
train[-1] <- normalize(train[-1])
head(train[-1])



clknn <-  train$diagnosis_result

modelknn <- knn(train[-1], test[-1], clknn, k =10, prob = FALSE, use.all = TRUE)

modelknn


#finding confusion matrix (Accuracy)
tableknn <- table(modelknn,test$diagnosis_result)
tableknn

CMknn <- confusionMatrix(tableknn)

Accuracyknn <- CMknn$overall[1]
Accuracyknn <- as.numeric(Accuracyknn)
Accuracyknn



Error_rate_knn<- 1-Accuracyknn
Error_rate_knn


##########

model_probknn <- knn(train[-1] , test[-1] , clknn , k = 10 , prob = TRUE , use.all = TRUE)
summary(model_probknn)

model_probonly_knn <- attr(model_probknn , "prob")
model_probonly_knn

model_fixed_prob_knn <- ifelse((model_probknn == "neg"),
                               1-attr(model_probknn,"prob"),
                               attr(model_probknn,"prob"))


model_fixed_prob_knn


library(ROCR)
pred_knn <- prediction(model_fixed_prob_knn,test$diagnosis_result)
length(model_fixed_prob_knn)
length(test$diagnosis_result)
perf_knn <- performance(pred_knn,"tpr","fpr")

plot(perf_knn, avg= "threshold", colorize=T, lwd=3, main="ROC Curve")
abline(0,1,lty = 10)

AUCLog2_knn <- performance(pred_knn,measure = "auc")@y.values[[1]]
cat("AUC: ",AUCLog2_knn,"n")


control_knn <- trainControl(method="cv", number=10, classProbs=TRUE,summaryFunction=twoClassSummary)
control_knn


set.seed(7)
attach(dataknn)
fit_knn_cancer <- train(diagnosis_result~., data=dataknn, method="knn",metric="ROC", trControl=control)


print(fit_knn_cancer)
























































