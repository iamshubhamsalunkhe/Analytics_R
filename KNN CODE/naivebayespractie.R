#####
library(dplyr)

datanaivebayes <- read.csv(file.choose())
head(datanaivebayes)
dim(datanaivebayes)
View(datanaivebayes)

datanaivebayes <- datanaivebayes[,-c(24:28)]
head(datanaivebayes)



######ifelse;

datanaivebayes <- (ifelse(datanaivebayes$Observed.Attendance = No "no)






##fitting naive bayes model

modelnaivebayes <- naiveBayes(datanaivebayes$Observed.Attendance ~ . , data = datanaivebayes)

modelnaivebayes


##predict#############

nb_prediction <- predict(modelnaivebayes , datanaivebayes)

##confusion matrix

table(nb_prediction ,datanaivebayes$Observed.Attendance)












