#=========================================
# Load the data 
#=========================================

data <- read.csv('train.csv')
head(data)
summary(data)
colnames(data)

##############################################################################
######## Transformation of Data ########
##############################################################################

#=========================================
# Let us study the distribution of cnt 
#=========================================

hist(data$cnt)

# This has a right skew. let's try log transformation

hist(log(data$cnt))

# This has a left skew.

hist(sqrt(data$cnt))

# It looks like a better distribution after taking the square root. 
# We will be incorporating it in the model.
# Since, the target variable is 'cnt' and not sqrt('cnt'), 
# before looking at the predictions, we need to convert them 
# back to orgnal values of 'cnt' by squaring the predictions.

##############################################################################
######## Binning ########
##############################################################################

#=========================================
# hr 
#=========================================

data$hr_bins <- ifelse((data$hr  < 6), "late_night",
                         ifelse(data$hr < 9, 'office_going',
                                ifelse(data$hr < 15, 'mid_day',
                                       ifelse(data$hr < 20, 'office_returning',
                                              'night'))))

# data$hr_bins <- ifelse((data$hr < 6) , "late night",
#                        ifelse(data$hr < 9 , "office_going",
#                                ifelse(data$hr <15 ,"mid_day",
#                                       ifelse(data$hr < 20  ,"office_returning",
#                                              "night"))))
class(data$hr_bins)
data$hr_bins <- as.factor(data$hr_bins)
head(data)
View(data)

a <- data[, c('hr_bins','cnt','hr')]
View(a)

#=========================================
# temp 
#=========================================

data$temp_bins <- ifelse((data$temp  < 0.3), "low",
                       ifelse(data$temp < 0.7, 'average',
                              'high'))
class(data$temp_bins)
data$temp_bins <- as.factor(data$temp_bins)
head(data)

a <- data[, c('temp_bins','cnt','temp')]
View(a)

#Binning:
# 1. Business Logic - if you want to target customers according to their age group
# 2. Based on the trend seen in Univariate Analysis
# 3. Based on relationship with target variable

##############################################################################
######## Feature/ Variable Creation ########
##############################################################################

#=========================================
# Label Encoder 
#=========================================

# install.packages('CatEncoders', dependencies = T)
library(CatEncoders)

?LabelEncoder.fit

lenc_temp <- LabelEncoder.fit(data$temp_bins)
data$temp_bins <-transform(lenc_temp,data$temp_bins)

head(data)

lenc_hr <- LabelEncoder.fit(data$hr_bins)
data$hr_bins <-transform(lenc_hr,data$hr_bins)

head(data)
?as.factor
#=========================================
# Dummy Creation 
#=========================================

data$atemp_bins <- ifelse((data$atemp  < 0.3), "low",
                         ifelse(data$atemp < 0.7, 'average',
                                'high'))

class(data$atemp_bins)
data$atemp_bins <- as.factor(data$atemp_bins)
head(data)

a <- data[, c('atemp_bins','cnt','atemp')]
View(a)
?
dummy.data.frame
library(dummies)
data <- dummy.data.frame(data,names=c("atemp_bins"), 
                          sep="_")
head(data)
