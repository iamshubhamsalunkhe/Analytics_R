rm(list=ls())

# Ho: There is no significant difference in the group means
# Ha: There is significant difference in the group means

df1 = read.csv("Sorority.csv", header = TRUE)
df1

# Concatenate the data rows of df1 into a single vector
r = c(t(as.matrix(df1)))

df1
as.matrix(df1)
t(as.matrix(df1))
c(t(as.matrix(df1)))

#Isolated the group names, defined the No. of groups and No.observation in each group
f = c("Sorority1", "Sorority2", "Sorority3", "Sorority4")
k = 4       # No. of groups
n = 5       # No.observation in each group

#creating the vebtor of groups corresponding to each element of r
tm = gl(k, 1, n*k, factor(f))
tm

# Apply Anova function

av = aov(r ~ tm)

summary(av)

alpha = 0.05
pvalue = 0.124

pvalue < alpha

# Result: We do not reject the null hypothesis
# There is no significant difference in the group means