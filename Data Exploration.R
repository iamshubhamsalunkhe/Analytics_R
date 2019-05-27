rm(list = ls())
#=========================================
# Load the data 
#=========================================

data <- read.csv("train.csv")
head(data)
summary(data)
colnames(data)

##############################################################################
######## Univariate Analysis ########
##############################################################################

##########################################
######## Continuous Data ########
##########################################

#=========================================
# Histogram
#=========================================

hist(data$temp, 10)

# More bikes are rented when the temperature is average. 
# Most people won't rent a bike when it is too hot or too cold.

#=========================================
# Boxplot
#=========================================

boxplot(data$registered)

# Registered users are high during few hours in a day.

##############################################################################
######## Bivariate Analysis ########
##############################################################################

##########################################
########  Continuous & Continuous ########
##########################################

#=========================================
# Scatter Plot
#=========================================

plot(data$temp, data$atemp)

# Correlation - give us an idea about a linear relationship between 2 continuous variables
# Correlation lies between -1 and +1

# This relationship is very obvious as atemp is a derivative of temp

# library(corrplot)
# corrplot(data)
# colnames(data)
# corrplot(cor(data[, -c(1:9)]))

##########################################
######## Categorical & Continuous ########
##########################################

#=========================================
# Boxplot
#=========================================

boxplot(data$temp~data$season)

summary(as.factor(data$season))

# We know, 1:spring, 2:summer, 3:fall, 4:winter

# Season 1 and Season 2 would be more favorable for biking 

##########################################
######## Categorical & Categorical #######
##########################################

#=========================================
# Stacked Barchart
#=========================================

colnames(data)

library(dplyr)
table <- data %>%                         ## The source dataset
         group_by(hr, workingday) %>%     ## Grouping variables
         summarise(cnt = mean(cnt))       ## aggregation of the cnt column Can also use sum
table

table <- as.data.frame(table)
dim(table)

# mean(cnt on workingday 0 i.e. on a non-working day)
workingday0 <- table[table$workingday == 0, 'cnt']
length(workingday0)

# mean(cnt on workingday 1 i.e. on a working day)
workingday1 <- table[table$workingday == 1, 'cnt']
length(workingday1)

hr <- unique(data$hr)

table <- cbind(hr, workingday0, workingday1)
table <- as.data.frame(table)
head(table)

hr <- as.factor(table$hr)
table[,1] <- NULL
rownames(table) <- hr 
head(table)

barplot(t(table), space = 1, legend.text = c('wday0', 'wday1'), col = c('blue', 'green'), 
        main = 'Stacked Bar of Count', sub = NULL, xlab = 'Hour', ylab = 'cnt')

# Shows us hourly distribution of count of rented bikes. 
# It is coloured with respect to the distribution of working and non-working days

# In the hours 0-6, when people will be sleeping, 
# we have low amount of rented bikes. Around 7th to 9th hour 
# and 17th to 19th hour, we see a hike in the number of bikes rented. 
# These would be the hours when people go an dcome back from work on working days.