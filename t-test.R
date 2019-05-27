#####################################################################
# Upper Tail Test of Population Mean with Unknown Variance
#####################################################################

# Problem
# 
# Suppose the food label on a cookie bag states that there is at most 2 grams 
# of saturated fat in a single cookie. In a sample of 35 cookies, it is found 
# that the mean amount of saturated fat per cookie is 2.1 grams. Assume that the 
# sample standard deviation is 0.3 gram. At .05 significance level, can we reject 
# the claim on food label?

# Ho: saturated fat in a single cookie < = 2 grams; mu <= 2
# Ha: saturated fat in a single cookie > 2 grams; mu > 2

xbar = 2.1            # Sample mean
mu0 = 2               # mean under the hypothesis
s = 0.3               # sample standard deviation
n = 35                # sample size

#Assumption that xbar follows N(mu0, (s/(sqrt(n)) )
t = (xbar???mu0)/(s/sqrt(n))
t

alpha = 0.05

# since the alt hypo is >, the rejection region will lie on the right hand side

# critical value approach

t.alpha = qt(1-alpha, n-1)
t.alpha

t < t.alpha # we will not rej the null, it this is true

# Result: The test statistic is 1.97 which is greater than the critical value 1.69.
# At 0.05 level of significance, we reject the claim.

# pval approach

pval = 1 - pt(t, n-1)
pval

pval = pt(t, n-1, lower.tail = FALSE)
pval
