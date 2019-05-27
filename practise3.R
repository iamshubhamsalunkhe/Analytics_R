#Practice - Data Manipulation

#1. Load the RetailScoreData file as 'Retail.data'.

Retail.data <- read.csv(file.choose(), header = T)
View(Retail.data)

# 2. Find the mean and median values of 'credit.debt' and 'other.debt'

creddebt_mean <- mean(Retail.data$creddebt)
creddebt_mean
creddebt_median <-median(Retail.data$creddebt)
creddebt_median

othedbt_mean ## as same above 

colnames(Retail.data)

 
 
 
# 3. Round of the elements in vector 'total.debt' in multiples of tens.
library(plyr)

Retail.data$totaldebt <- Retail.data$creddebt + Retail.data$othdebt

str(Retail.data)
Retail.data$totaldebt <- round_any(Retail.data$totaldebt , 10)
Retail.data$totaldebt


#4. Paste the elements of the two vectors, 'credit.debt' and 'other.debt' using separator ",".

?paste
paste(Retail.data$creddebt,Retail.data$othdebt , sep = ",")





#5. Create a data.frame 'Retail.3779 with all the observations where ncust is 3779.

Retail.data$ncust <- as.factor(Re)



Retail.3779 <- Retail.data[Retail.data$ncust == 3779 ,]
Retail.3779





#Hint : You need to change the data type of the column 'ncust'

#6. Sort the data.frame 'Retail.3779' in the decreasing order of variable 'age' and assign it to Retail.3779.sort.

Retail.3779.sort <- Retail.3779[order(Retail.3779$age , decreasing =  T), ]
Retail.3779.sort






#7. See how many observations in 'Retail.3779' are employed for more than 10 years.

Retail.3779.more_10_years <- Retail.3779[Retail.3779$address >= 10, ]
Retail.3779.more_10_years




#8. Find the mean of all observations in 'Retail.data' in variables 'creddebt' and 'othdebt' grouped by 'ncust'.

Retail_creddebt_mean <- tapply(Retail.data$creddebt,Retail.data$ncust,mean)
Retail_creddebt_mean

Retail_othe_debt <- tapply(Retail.data$othdebt , Retail.data$ncust , mean)
Retail_othe_debt







