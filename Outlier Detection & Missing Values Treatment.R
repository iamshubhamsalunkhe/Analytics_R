rm(list=ls())

#=========================================
# Load the data 
#=========================================

data <- read.csv('student.csv')
head(data)
summary(data)
str(data)
colnames(data)

# data$famrel <- as.factor(data$famrel)
# data$freetime <- as.factor(data$freetime)
# data$goout <- as.factor(data$goout)
# data$Dalc <- as.factor(data$Dalc)
# data$Walc <- as.factor(data$Walc)
# data$health <- as.factor(data$health)

dim(data)

##############################################################################
######## Treatment of Missing Value ########
##############################################################################

is.na(data)
sum(is.na(data))
colSums(is.na(data))

summary(data)

levels(data$activities)

levels(data$activities)[1] <- NA
levels(data$activities)

summary(data)

colSums(is.na(data))

sum((is.na(data)))

##########################################
######## Deletion Method ########
##########################################

data1 <- data
dim(data)

data1 <- data1[complete.cases(data1), ]
dim(data1)

395 - 182 
# we end up losing 213 rows of information. So, it makes sense to retain 
# this data

##########################################
######## Imputation Method ########
##########################################

#=========================================
# Imputation of Continous Values
#=========================================

head(data$absences)

hist(data$absences,10)

median(data$absences)

median(data$absences, na.rm = TRUE)

data$absences[is.na(data$absences)] <- median(data$absences, na.rm = TRUE)  # not run
 
sum(is.na(data$absences))

#=========================================
# Imputation of Categorical Values
#=========================================

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# x <-  c(1:10,9,9,9,9,NA,NA,NA)
# x
# x = x[!is.na(x)]
# x
# ux <- unique(x)
# ux
# match(x, ux)
# tabulate(match(x, ux))
# which.max(tabulate(match(x, ux)))
# ux[which.max(tabulate(match(x, ux)))]

Mode(data$activities, na.rm = T)

  # install.packages('Hmisc')
library(Hmisc)
?impute

data$activities <- impute(data$activities, Mode)
sum(is.na(data$activities))

##############################################################################
######## Outlier Detection ########
##############################################################################

boxplot(data$absences)

q1 <- quantile(data$absences, 0.25)
q1

q3 <- quantile(data$absences, 0.75)
q3

IQR <- q3 - q1
IQR

lower_threshold <- q1 - 1.5 * IQR
lower_threshold

upper_threshold <- q3 + 1.5 * IQR
upper_threshold

Outlier1 <- data[data$absences < lower_threshold,]
Outlier2 <- data[data$absences > upper_threshold,]

Outlier1
Outlier2
dim(Outlier2)

# we will lose 25 rows. 

# We can retain them by imputing median inplace of outliers

data$absences[data$absences > upper_threshold] <- median(data$absences)

boxplot(data$absences)
