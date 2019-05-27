rm(list=ls())

# 1. To record the patient statistics, the agency wants to find the age 
# category of people who frequents the hospital and has the maximum expenditure.  

hops  <- read.csv('HospitalCosts.csv') 
head(hops)
nrow(hops)
summary(hops)

# a. To find the category that has the highest frequency of hospital visit, 
# we can use graphical analysis. A histogram would display the number of 
# occurrences of each age category.  The as.factor() is called to make sure 
# that the categories are not treated as numbers. Outlier treatments: None  
# Code:  

# hops  <- read.csv('HospitalCosts.csv') 
hist(hops$AGE)
summary(as.factor(hops$AGE))  

# Result: From the graph that is displayed, we can see that infants have the maximum 
# frequency of hospital visit, going above 300. The summary of AGE attribute gives 
# the numerical output (after converting the age from numeric to factor) - and we 
# can see that there are 307 entries for those in the range of 0-1 year

# b. To find the category with the maximum expenditure, we need to add the expenditure 
# for each age, and find the maximum value from the sum. We will use the aggregate function 
# to add the values of total expenditure according to the values of age.

# Code:  
hops1 <- hops

hops1$age_bins <- ifelse((hops1$AGE  < 1), "infant",
                         ifelse(hops1$AGE < 3, 'toddler',
                              ifelse(hops1$AGE < 11, 'child', 
                                     'adolescent')))

hops1$age_bins <- as.factor(hops1$age_bins)

head(hops1)

View(hops1)
summary(hops1)

aggregate(TOTCHG ~ AGE, FUN = sum, data = hops)
max(aggregate(TOTCHG ~ AGE, FUN = sum, data = hops))

df <- aggregate(TOTCHG ~ age_bins, FUN = sum, data = hops1)
df

#max(aggregate(TOTCHG ~ age_bins, FUN = sum, data = hops1)$TOTCHG)

df[(df$TOTCHG == max(df$TOTCHG)),]

# Result: From the result we can see that the infant category has maximum hospital costs 
# as well (in accordance with the number or frequency of visit). Following the infants, 
# 15 and 17 year old individuals have high hospitalization costs.

# 2. In order of severity of the diagnosis and treatments and to find out the expensive 
# treatments, the agency wants to find the diagnosis related group that has maximum 
# hospitalization and expenditure. Similar to the previous analysis, we can find the diagnosis 
# related group with maximum hospitalization and expenditure. For this, we will use the aggregate 
# and the histogram functions. The which.max function can be used to get the index of the data frame 
# with the maximum value. The as.factor() is called to make sure that the categories are not treated as numbers.  
# Code:  
head(hops)
summary(as.factor(hops$APRDRG))

which.max(summary(as.factor(hops$APRDRG))) 

df <- aggregate(TOTCHG ~ as.factor(hops$APRDRG), FUN = sum, data = hops)

df[which.max(summary(as.factor(hops$APRDRG))),]

# Result: From the results we can see that the category 640 has the 
# maximum entries of hospitalization, by a huge contrast (267 of 500 entries), 
# and also has the highest total hospitalization cost (437978).

# 3. To make sure that there is no malpractice, the agency needs to analyze if the 
# race of the patient is related to the hospitalization costs. To analyze, first 
# convert the Race variable to factors and perform a summary of the variable. This 
# will help you to find how many patients belonging to the different groups were admitted. 
# Then, to verify if the races made an impact on the costs, perform an ANOVA with the 
# following variables:  
# ANOVA dependent variable: TOTCHG 
# Categorical/grouping variable: RACE Missing values: 1 NA value, use na.omit to remove the NA value   
# 
# Code:  

hops <- na.omit(hops) 
model <- aov(TOTCHG ~ as.factor(RACE), data = hops)  
summary(model)
modeleg <- lm (TOTCHG ~ as.factor(RACE) , data = hops)
summary(modeleg)
 summary(as.factor(hops$RACE)) 

# Result:  The p-value is very high specifying that there is no relation between 
# the race of patient and the hospital cost. From the summary we can also see that 
# the data has 484 patients of Race 1 out of the 500 entries. This will affect the 
# results of ANOVA as well, since the number of observations is very much skewed. 
# In conclusion, there is not enough data to verify if the race of patient is related 
# to the hospitalization cost.

# 4. To properly utilize the costs, the agency has to analyze the severity of the 
# hospital costs by age and gender for proper allocation of resources.  
# To analyze the severity of hospital costs, create a linear regression model with 
# the following variables: Dependent variable: TOTCHG Independent variables: AGE, FEMALE  

# Code:  

model1 <- lm(TOTCHG ~ AGE + as.factor(FEMALE), data = hops)
summary(model1)
summary(as.factor(hops$FEMALE))

# Result: Age is a very important factor in the hospital costs as seen by the 
# significance levels and p-values. The gender also seems to have an impact. 
# When we see a summary function, we find that there is an equal number of male 
# and female patients. The negative coefficient shows that on an average, females 
# incur lesser cost than males.  

# 5. Since the length of stay is the crucial factor for inpatients, the agency 
# wants to find if the length of stay can be predicted from age, gender, and race.  
# Since the length of stay is a continuous variable, we use linear regression to 
# predict the variable.  Dependent variable: LOS Independent variables: AGE, FEMALE, 
# RACE Note that RACE and FEMALE should be converted into factors, whereas AGE is a 
# numerical variable.  

# Code:   

Model2 <- lm(LOS ~ AGE + as.factor(FEMALE) + as.factor(RACE),  data = hops)  
summary(Model2)

Model3 <- lm(LOS ~ as.factor(AGE) + as.factor(FEMALE) + as.factor(RACE), data = hops)  
summary(Model3)

Model4 <- lm(LOS ~ as.factor(AGE) + as.factor(FEMALE) + as.factor(RACE)  + as.factor(APRDRG), data = hops)  
summary(Model4)


# Result: The significance codes are almost null for all the variables, except for 
# the intercept. The very high p-value signifies that there is no linear relationship 
# between the given variables. That is, with just the age, gender, and race, it is not 
# possible to predict the length of stay of a patient.   

# 6. To perform a complete analysis, the agency wants to find the variable that 
# mainly affects the hospital costs. To find the variables that mainly affect the 
# total costs, construct a linear model with all the variables as the causing variables.  
# Dependent variable: TOTCHG Independent variables: All other variables  

# Code:  
summary(hops)

hops$FEMALE <- as.factor(hops$FEMALE)
hops$RACE <- as.factor(hops$RACE)
summary(hops)

Model5 <- lm(TOTCHG ~ ., data = hops)  
summary(Model5)

# Result: We can see that age and length of stay affect the total hospital cost. Length 
# of stay positively affects the cost. That is, with an increase of 1 day, there is an 
# addition of a value of 742 to the total cost.
