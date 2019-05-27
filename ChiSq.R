rm(list = ls())

# Ho: There is no relationship between the service level and the salary
# Ha: There is relationship between the service level and the salary

tbl3 <- read.csv(file.choose())
tbl3

chisq.test(tbl3) #gives error, need to get data in shape

row_names <- tbl3[,1]
tbl3
tbl3[,1] <- NULL
tbl3

row.names(tbl3) <- row_names 

tbl3

alpha = 0.05

chisq.test(tbl3)

# deg of freedom = (c-1)(r-1) --- (3-1)(3-1) = 4

# Pearson's Chi-squared test
# 
# data:  tbl3
# X-squared = 18.658, df = 4, p-value = 0.0009172

# since, pval < alpha, rej ho

#############################################################################
rm(list = ls())

library(MASS)

?survey

# Test if there is a relationship berween smoking habit and exercise at 0.05 los

# Ho: The is no relationship between smoking habit and exercise
# Ha: The is relationship between smoking habit and exercise

View(survey)

tbl = table(survey$Smoke, survey$Exer)
tbl

chisq.test(tbl)

# Pearson's Chi-squared test
# 
# data:  tbl
# X-squared = 5.4885, df = 6, p-value = 0.4828
# 
alpha = 0.05
pval = 0.4828

pval < alpha # do not rej the null hypothesis

# Result: The is no relationship between smoking habit and exercise

#############################################################################
# Test if there is a relationship berween smoking habit and sex at 0.05 los

# Ho: The is no relationship between smoking habit and sex
# Ha: The is relationship between smoking habit and sex

tbl = table(survey$Smoke, survey$Sex)
tbl

chisq.test(tbl)

alpha = 0.05
pval = 0.3139

pval < alpha

# Result: The is no relationship between smoking habit and sex
#############################################################################

# If there is a relationship berween W.Hnd and Clap, test at 0.05 los

# Ho: The is no relationship between W.Hnd and Clap
# Ha: The is relationship between W.Hnd and Clap

tbl = table(survey$W.Hnd, survey$Clap)
tbl

chisq.test(tbl)

alpha = 0.05
pval = 6.598e-05

pval < alpha

# Result: The is relationship between W.Hnd and Clap
