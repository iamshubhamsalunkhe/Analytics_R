?`datasets-package`
library(help = "datasets")
summary(sleep)
View(sleep)
?sleep
?beavers
View(beaver1)
?women
View(women)
library(psych)
library(MASS)

data_womenr <- women 
?women
?sample
set.seed(15)
train_women <- sample(1:15)
test_women <- -train_women

training_women <- women[train_women,c("height" , "weight")]
nrow(training_women)
View(training_women)



model_train_women <- lm(height ~ weight , data = training_women)
summary(model_train_beaver)


plot(training_women , training_beaver$height)


plot(log(training_women$weight),log(training_women$height), xlab = "weight - log(" , ylab = "height - log(")


pairs.panels(training_women)

model_women <- lm(log(weight) ~ log(height) , data = training_women)

summary(model_women)


plot(log(training_women$weight),
     log(training_women$height),
     xlab = "weight -log",
     ylab = "height - log",
     col = "red")

abline(model_women, col = "blue" , lwd = 2)


#####hospital dataset

#AGE  : Age of the patient discharged 
#FEMALE : Binary variable that indicates if the patient is female 
#LOS   : Length of stay, in days 
#RACE   : Race of the patient (specified numerically) 
#TOTCHG  : Hospital discharge costs 
#APRDRG  : All Patient Refined Diagnosis Related Groups 


#To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and has the maximum expenditure. 

hospital <- read.csv("HospitalCosts.csv") 
hospital_age <- as.factor(hospital$AGE)
hist(hospital_age)
View(hospital)

#In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis related group that has maximum hospitalization and expenditure. 

groups <- as.factor(hospital$APRDRG)
dos <- max(hospital$LOS)  
dos









