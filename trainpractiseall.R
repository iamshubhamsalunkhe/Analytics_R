## look at the data just an overview
##for load the train data 
#### this code will replac blank space with na values in data <- read.csv("train.csv",na.strings = c(""))


data <- read.csv("train.csv",na.strings = c(""))
colSums(is.na(data))

data <- read.csv("train.csv")
head(data)
summary(data)
colnames(data)
View(data)

str(data)
colSums(is.na(data))
dim(data)

##so missing values in are not that much so we can handle it by imputing mode if neccessary
## credit history was numeric as seen un structure of data we just made it as factor because
## credit history tells us whether it meets the guidelines or not 

data$Credit_History <- as.factor(data$Credit_History) 

## applying feature engineering for applicant and co applicant they both are telling the income
##we can convert them into household income 





#imputing mode on na values 

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

Mode(data$Credit_History, na.rm = T)

library(Hmisc)
data$Credit_History <- impute(data$Credit_History, Mode)
sum(is.na(data$Credit_History))

#imputing median on na values 
## box plot of loanamount to detect outliers 

boxplot(data$LoanAmount)



median(data$LoanAmount,na.rm = T)
data$LoanAmount <- impute(data$LoanAmount , median)
sum(is.na(data$LoanAmount))


### treating na values in loan_amount_term

#### this code will replac blank space with na values in data <- read.csv("train.csv",na.strings = c(""))


data <- read.csv("train.csv",na.strings = c(""))
colSums(is.na(data))


##Treating values Self_Employed 

data$Self_Employed <- impute(data$Self_Employed, Mode)

##Treating values Gender

data$Gender <- impute(data$Gender, Mode)

##treating values Married

data$Married <- impute(data$Married, Mode)

##treating values Dependents

data$Dependents <- impute(data$Dependents, Mode)

## treating values Loan_Amoutn_term

data$Loan_Amount_Term <- impute(data$Loan_Amount_Term, median)


## checking for any missing values 

colSums(is.na(data))

### comibining applicantsincome and coapplicant income into household

data$household <- data$ApplicantIncome + data$CoapplicantIncome
head(data)
summary(data)

### binning dependant columne

data$Dependents <- ifelse((data$Dependents == 0) ,0,1)
data$Dependents <- as.factor(data$Dependents)
head(data$Dependents)
## backup data

data_without_na <- data
##visualize the data and try to detect skewness and outliers

hist(log(data$household),50)

hist(log(data$Loan_Amount_Term),50)


median(data$household)
str(data)
library(corrplot)
corrplot(data)
g 
drop(anov1)


View(data_without_na)







