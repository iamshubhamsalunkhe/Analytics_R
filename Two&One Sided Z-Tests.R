#################################
##### Two Sided Test
#################################

# Formulate the Hypothesis
# 
# Ho:	The bolts are exactly 1 inch thick on an average
# mu = 1
# H1 or Ha	The bolts are not exactly 1 inch thick on an average
# mu != 1

xbar = 1.2            # Sample mean
mu0 = 1               # mean under the hypothesis
sigma = 0.4           # sample standard deviation
n = 100               # sample size

# Assumption: xbar follows N(mu0, sigma/sqrt(n))

z = (xbar-mu0)/(sigma/sqrt(n))
z

### Critical Value Method

alpha = 0.01
alpha/2
1-alpha/2

posi_z.alpha = qnorm(1- (alpha/2))
posi_z.alpha

neg_z.alpha = qnorm(alpha/2)
neg_z.alpha

# if z value lies between neg_z.alpha and posi_z.alpha, do not rej the null hypothesis

# we are checking the conditions for NOT rejecting the NULL HYPOTHESIS

z > neg_z.alpha & z < posi_z.alpha

# Result: Rej Ho, The bolts are not exactly 1 inch thick on an average

### p-value Value Method

pval1 = 1 - pnorm(z) # P(z > 5)
pval2 = pnorm(-z)    # P(z < -5)
pval = pval1 + pval2
pval

pval < alpha

# Result: Rej Ho, The bolts are not exactly 1 inch thick on an average

#################################
##### One Sided Test
#################################

# Problem
# Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours.
# In a sample of 30 light bulbs, it was found that they only last 9,900 hours on average. 
# Assume the population standard deviation is 120 hours. At .05 significance level, 
# can we reject the claim by the manufacturer?

# Formulate the Hypothesis
# 
# Ho:	The mean lifetime of a light bulb is more than 10,000 hours
# mu0 >= 10,000
# H1 or Ha	The mean lifetime of a light bulb is less than 10,000 hours
# mu1 < 10,000

xbar = 9900            # Sample mean
mu0 = 10000               # mean under the hypothesis
sigma = 120           # sample standard deviation
n = 30               # sample size

# Assumption: xbar follows N(mu0, sigma/sqrt(n))

z = (xbar-mu0)/(sigma/sqrt(n))
z # -4.564355

### Critical Value Method

alpha = 0.05

# since the sign of the alt hypothesis is <, the rej region will lie in the left hand side

z.alpha = qnorm((alpha))
z.alpha # - 1.644854

z < z.alpha

# if z < z.zlpha, Rej the null hypothesis

### pvalue method

pval = pnorm(z)
pval

pval < alpha

# Rej Ho

