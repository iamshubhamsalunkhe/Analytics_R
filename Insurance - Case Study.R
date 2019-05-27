rm(list=ls())

###########################################################

# The committee is interested to know each field of the data collected through descriptive 
# analysis to gain basic insights into the data set and to prepare for further analysis. 
# Code 

df_data = read.csv("SwedishMotorInsurance.csv",header= TRUE,sep = ',')

str(df_data) # read variables and attributes 

summary(df_data)  # this describes the daata in detail 

df_data$Zone = as.factor(df_data$Zone) 
df_data$Make = as.factor(df_data$Make) 
df_data$Bonus = as.factor(df_data$Bonus)
df_data$Kilometres = as.factor(df_data$Kilometres)

str(df_data) # read variables and attributes 

summary(df_data)  # this describes the daata in detail 

# summary(scale(df_data$Insured))

head(df_data) # see few reords

colSums(is.na(df_data))
View(df_data)

# # Not using the data which has missing values 
# 
# # Code 
# no_na_data <- na.omit(df_data) # omit the data which has NA or missing values
# summary(no_na_data) # observe the data

###########################################################

# The total value of payment by an insurance company is an important factor 
# to be monitored. So the committee has decided to find whether this payment is 
# related to number of claims and the number of insured policy years. They also 
# want to visualize the results for better understanding. 

# I observe the positive Correlation between Claims and Payment.

# Code

cor(df_data$Claims,df_data$Payment)  # 0.9954003

plot (df_data$Claims,df_data$Payment)

cor(df_data$Insured,df_data$Payment)  # 0.933217

plot (df_data$Insured,df_data$Payment)

###########################################################

# The committee wants to figure out the reasons for insurance payment increase 
# and decrease. So they have decided to find whether distance, location, bonus, 
# make, and insured amount or claims are affecting the payment or all or some of 
# these are affecting it. 
# Here lets apply the linear model and where Payment is dependent variable on 
# the rest of all others. 
# Code

reg<-lm(Payment ~ ., data = df_data)
summary(reg)

###########################################################

# The insurance company is planning to establish a new branch office, 
# so they are interested to find at what location, kilometer, and bonus 
# level their insured amount, claims, and payment get increased. 
# (Hint: Aggregate Dataset) 
# Here we apply the simple regression to see the data.
# code

simple_lm <- lm(Payment ~ Claims+Kilometres+Zone+Bonus+Make, data=df_data)

simple_lm$coefficients #a vector of the coefficients for each variable
simple_lm$residuals #a vector of the residuals for each data point

#summary.lm() similar to lm() but gives summary values for the model
summary_simple_lm <- summary.lm(simple_lm)  
summary_simple_lm$r.squared #R-squared value
summary_simple_lm$sigma #standard error of the regression model
summary(simple_lm)

group1<-apply(df_data[,c(5,6,7)], 2, function(x) tapply(x, df_data$Zone, mean))
group1

group2<-apply(df_data[,c(5,6,7)], 2, function(x) tapply(x, df_data$Kilometres, mean))
group2

group3<-apply(df_data[,c(5,6,7)], 2, function(x) tapply(x, df_data$Bonus, mean))
group3

###########################################################

# The committee wants to understand what affects their claim rates so as 
# to decide the right premiums for a certain set of situations. Hence, they 
# need to find whether the insured amount, zone, kilometer, bonus, or make 
# affects the claim rates and to what extent. 
# Here we can see the affect using tapply function with different variable.

# To see the affect on claim use the linear model again.
# Code
# Dependand Claims Independent rest 

lm_claim <- lm(Claims ~ Insured + Make + Bonus + Zone + Kilometres, data = df_data)
summary(lm_claim)
