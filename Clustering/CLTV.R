rm(list=ls())
# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 1 - STATISTICAL SEGMENTATION
# __________________________________________________________
# //////////////////////////////////////////////////////////


# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------


# Load text file into local variable called 'data'
data = read.csv('Purchase_Segment.txt', header = FALSE, sep = '\t', dec = '.')

View(data)
# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase,"%Y-%m-%d")
data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

# Display the data after transformation
head(data)
summary(data)
nrow(data)

# Compute key marketing indicators using SQL language

library(sqldf)

# Compute recency, frequency, and average purchase amount
customers = sqldf("SELECT customer_id,
                  MIN(days_since) AS 'recency',
                  COUNT(*) AS 'frequency',
                  AVG(purchase_amount) AS 'amount'
                  FROM data GROUP BY 1")
#View(customers)

# Explore the data

head(customers)
nrow(customers)
summary(customers)

hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)


# --- PREPARING AND TRANSFORMING DATA ----------------------


# Copy customer data into new data frame
new_data = customers

# Remove customer id as a variable, store it as row names
head(new_data)
row.names(new_data) = new_data$customer_id
new_data$customer_id = NULL
head(new_data)

# Take the log-transform of the amount, and plot
new_data$amount = log(new_data$amount)
hist(new_data$amount)

#View(new_data$amount)

# Standardize variables
new_data = scale(new_data)
head(new_data)

summary(new_data)
summary(customers)

# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------

# Compute distance metrics on standardized data
# This will likely generate an error on most machines
# d = dist(new_data)

nrow(new_data)

# Take a 10% sample
sample = seq(1, 18417, by = 10)
length(sample)

# head(sample)

customers_sample = customers[sample, ]
new_data_sample  = new_data[sample, ]

# Compute distance metrics on standardized data
d = dist(new_data_sample)

# Perform hierarchical clustering on distance metrics
c = hclust(d, method = "ward.D2") # refers to use of euclidean methods sqrt of dist

# Plot de dendogram
plot(c)
rect.hclust(c, k = 9)

# Cut at 9 segments
members = cutree(c, k = 9)

# Show 30 first customers, frequency table
members[1:30]
table(members)

# Show profile of each segment
aggregate(customers_sample[, 2:4], by = list(members), mean)


# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 2 - MANAGERIAL SEGMENTATION
# __________________________________________________________
# //////////////////////////////////////////////////////////


# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------

# --- CODING A MANAGERIAL SEGMENTATION ---------------------

# Compute recency, frequency, and average purchase amount for 2015
customers_2015 = sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'amount'
                        FROM data GROUP BY 1")
head(customers_2015)

# Simple 2-segment solution based on recency alone
customers_2015$segment = ifelse(test = customers_2015$recency > 365*3, yes = "inactive", no = "NA")
table(customers_2015$segment)

aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

# # Simple 2-segment solution using the which statement
# customers_2015$segment = "NA"
# customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
# table(customers_2015$segment)
# aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

# A more complex 3-segment solution based on recency alone
customers_2015$segment = ifelse(test = customers_2015$recency > 365*3,
                                yes = "inactive",
                                no = ifelse(test = customers_2015$recency > 365*2,
                                            yes = "cold",
                                            no = "NA"))
table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

# More complex 4-segment solution using which
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive" # Last Purchase (LP) Dec 2012
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold" # LP in 2013
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm" # LP in 2014
customers_2015$segment[which(customers_2015$recency <= 365)] = "active" # LP in 2015
table(customers_2015$segment)

cust_seg_4 <- table(customers_2015$segment)
cust_seg_4 <- as.data.frame(cust_seg_4)
cust_seg_4$V2 = NULL
cust_seg_4$V2 <- c("LP 2012", "LP 2013", "LP 2014", 'LP 2015')
colnames(cust_seg_4) = c('segments', 'No. of customers', 'segment definition')
cust_seg_4

aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

head(customers_2015)

# Complete segment solution using which, and exploiting previous test as input

customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount < 100)] = "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount >= 100)] = "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount < 100)] = "active low value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount >= 100)] = "active high value"

# head(customers_2015[customers_2015$segment == 'new warm',])

seg_condi <- c('recency > 365*3', 'recency <= 365*3 & recency > 365*2',
               'recency <= 365*2 & $recency > 365*1', 'recency <= 365',
               'warm & first_purchase <= 365*2', 'warm & amount < 100',
               'warm & amount >= 100', 'active & first_purchase <= 365',
               'active & amount < 100', 'active & amount >= 100')

seg_names = c('inactive', 'cold','warm', 'active', 'new warm', 
              'warm low value', 'warm high value', 'new active',
              'active low value', 'active high value')

seg_def <- c("LP 2012", "LP 2013", "LP 2014", 'LP 2015',
             'LP 2014 & FP 2014 (new customer in 2014)',
             'LP 2014 & A < 100', 'LP 2014 & A >= 100',
             'LP 2015 and FP in 2015 (new customer in 2015)',
             'LP 2015 & A < 100', 'LP 2015 & A >= 100' )

seg_info <- cbind(seg_names, seg_condi, seg_def)

seg_info <- as.data.frame(seg_info)
seg_info


table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

# Re-order factor in a way that makes sense
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))

seg_info = seg_info[c(1,2,7,6,5,10,9,8),]
seg_info

table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

# # class(seg_info)
# cust_seg_8 <- table(customers_2015$segment)
# cust_seg_8 <- as.data.frame(cust_seg_8)
# cust_seg_8$V2 = seg_info$seg_def
# cust_seg_8$V3 = seg_info$seg_condi
# # cust_seg_8
# 
# colnames(cust_seg_8) = c('segments', 'No. of customers', 'segment definition', 'segment condition')
# cust_seg_8

# --- SEGMENTING A DATABASE RETROSPECTIVELY ----------------


# Compute key marketing indicators using SQL language
# library(sqldf)

# Compute recency, frequency, and average purchase amount
# data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))

customers_2014 = sqldf("SELECT customer_id,
                       MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM data
                       WHERE days_since > 365
                       GROUP BY 1")

# Complete segment solution using which, and exploiting previous test as input
customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & customers_2014$recency > 365*2)] = "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] = "active"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2)] = "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 100)] = "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 100)] = "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 100)] = "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 100)] = "active high value"

seg_condi_2014 <- c('recency > 365*3', 'recency <= 365*3 & recency > 365*2',
               'recency <= 365*2 & $recency > 365*1', 'recency <= 365',
               'warm & first_purchase <= 365*2', 'warm & amount < 100',
               'warm & amount >= 100', 'active & first_purchase <= 365',
               'active & amount < 100', 'active & amount >= 100')

seg_names_2014 = c('inactive', 'cold','warm', 'active', 'new warm',
              'warm low value', 'warm high value', 'new active',
              'active low value', 'active high value')

seg_def_2014 <- c("LP 2011", "LP 2012", "LP 2013", 'LP 2014',
             'LP 2013 & FP 2013 (new customer in 2013)',
             'LP 2013 & A < 100', 'LP 2013 & A >= 100',
             'LP 2014 and FP in 2014 (new customer in 2014)',
             'LP 2014 & A < 100', 'LP 2014 & A >= 100' )

seg_info_2014 <- cbind(seg_names_2014, seg_condi_2014, seg_def_2014)

seg_info_2014 <- as.data.frame(seg_info_2014)
seg_info_2014

table(customers_2014$segment)
aggregate(x = customers_2014[, 2:5], by = list(customers_2014$segment), mean)

# Re-order factor in a way that makes sense
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))

seg_info_2014 = seg_info_2014[c(1,2,7,6,5,10,9,8),]
seg_info_2014
# seg_info

# Show segmentation results
table(customers_2014$segment)
# pie(table(customers_2014$segment), col = rainbow(24))
aggregate(x = customers_2014[, 2:5], by = list(customers_2014$segment), mean)

table(customers_2014$segment)
table(customers_2015$segment)

nrow(customers_2015)
nrow(customers_2014)

nrow(customers_2015) - nrow(customers_2014) # new customers acquired in 2015

# # class(seg_info)
# cust_seg_8_2014 <- table(customers_2014$segment)
# cust_seg_8_2014 <- as.data.frame(cust_seg_8)
# cust_seg_8_2014$V2 = seg_info$seg_def
# cust_seg_8_2014$V3 = seg_info$seg_condi
# # cust_seg_8_2014
# 
# colnames(cust_seg_8_2014) = c('segments', 'No. of customers', 'segment definition', 'segment condition')
# cust_seg_8_2014


# --- COMPUTING REVENUE GENERATION PER SEGMENT -------------

# Compute how much revenue is generated by segments

data$year_of_purchase = as.numeric(format(data$date_of_purchase, "%Y"))
head(data)
summary(data)

## Revenue generated per customer
revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                     FROM data
                     WHERE year_of_purchase = 2015
                     GROUP BY 1")
head(revenue_2015)
nrow(revenue_2015)
summary(revenue_2015)

# Notice that people with no revenue in 2015 do NOT appear

# Merge 2015 customers and 2015 revenue (correct)
actual = merge(customers_2015, revenue_2015, all.x = TRUE)
head(actual)
nrow(actual)

# data[data$customer_id == 80,]
# data[data$customer_id == 90,]

actual$revenue_2015[is.na(actual$revenue_2015)] = 0
head(actual)

# Show average revenue per customer and per segment
aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)

t <- aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)
t <- as.data.frame(t)
t$V1 = seg_info$seg_def

t

head(actual)
# head(revenue_2015)
# head(customers_2015)

# Merge 2014 customers and 2015 revenue (correct)

head(customers_2014)
head(revenue_2015)

forward = merge(customers_2014, revenue_2015, all.x = TRUE)
head(forward)
nrow(forward)

forward$revenue_2015[is.na(forward$revenue_2015)] = 0
head(forward, 20)
head(actual, 20)

# Show average revenue per customer and per segment
r = aggregate(x = forward$revenue_2015, by = list(customers_2014$segment), mean)
print(r)

r1 <- as.data.frame(r)
r1$V1 = seg_info_2014$seg_def_2014 
r1
t
# Re-order and display results
r = r[order(r$x, decreasing = TRUE), ]
print(r)
barplot(r$x, names.arg = r$Group.1)

# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 3 - SCORING
# __________________________________________________________
# //////////////////////////////////////////////////////////


# --- COMPUTING PREDICTORS AND TARGET VARIABLES ------------

# Compute key marketing indicators using SQL language
library(sqldf)

# Compute RFM variables as of a year ago
customers_2014 = sqldf("SELECT customer_id,
                       MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'avg_amount',
                       MAX(purchase_amount) AS 'max_amount'
                       FROM data
                       WHERE days_since > 365
                       GROUP BY 1")
head(customers_2014)

# Compute revenues generated by customers in 2015
revenue_2015 = sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                     FROM data
                     WHERE year_of_purchase = 2015
                     GROUP BY 1")

# Merge 2014 customers and 2015 revenue
in_sample = merge(customers_2014, revenue_2015, all.x = TRUE)
in_sample$revenue_2015[is.na(in_sample$revenue_2015)] = 0
in_sample$active_2015 = as.numeric(in_sample$revenue_2015 > 0)

# Display calibration (in-sample) data
head(in_sample)
summary(in_sample)


# --- CALIBRATE THE MODELS ---------------------------------


# Calibrate probability model
#install.packages('nnet')
library(nnet)
prob.model = multinom(formula = active_2015 ~ recency + first_purchase + frequency + avg_amount + max_amount,
                      data = in_sample)
coef = summary(prob.model)$coefficients
std  = summary(prob.model)$standard.errors
print(coef)
print(std)
print(coef / std)

# For the monetary model, select only those who made a purchase
z = which(in_sample$active_2015 == 1)
head(in_sample[z, ])
summary(in_sample[z, ])

# Calibrate the monetary model (version 1)
amount.model = lm(formula = revenue_2015 ~ avg_amount + max_amount, data = in_sample[z, ])
summary(amount.model)

# Plot the results of the monetary model
plot(x = in_sample[z, ]$revenue_2015, y = amount.model$fitted.values)

# Re-calibrate the monetary model, using a log-transform (version 2)
amount.model = lm(formula = log(revenue_2015) ~ log(avg_amount) + log(max_amount), data = in_sample[z, ])
summary(amount.model)

# Plot the results of this new monetary model
plot(x = log(in_sample[z, ]$revenue_2015), y = amount.model$fitted.values)


# --- APPLY THE MODELS TO TODAY'S DATA ---------------------


# Compute RFM variables as of today
customers_2015 = sqldf("SELECT customer_id,
                       MIN(days_since) AS 'recency',
                       MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'avg_amount',
                       MAX(purchase_amount) AS 'max_amount'
                       FROM data GROUP BY 1")

# Predict the target variables based on today's data
customers_2015$prob_predicted    = predict(object = prob.model, newdata = customers_2015, type = "probs")
customers_2015$revenue_predicted = exp(predict(object = amount.model, newdata = customers_2015))
customers_2015$score_predicted   = customers_2015$prob_predicted * customers_2015$revenue_predicted
summary(customers_2015$prob_predicted)
summary(customers_2015$revenue_predicted)
summary(customers_2015$score_predicted)
hist(customers_2015$score_predicted)

# How many customers have an expected revenue of more than $50
z = which(customers_2015$score_predicted > 50)
print(length(z))

#View(z)

# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#    MODULE 4 - CUSTOMER LIFETIME VALUE
# __________________________________________________________
# //////////////////////////////////////////////////////////


# --- SEGMENT CUSTOMERS IN 2014 AND 2015 -------------------

# Segment customers in 2015
customers_2015 = sqldf("SELECT customer_id,
                       MIN(days_since) AS 'recency',
                       MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM data GROUP BY 1")
customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount < 100)] = "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount >= 100)] = "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount < 100)] = "active low value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount >= 100)] = "active high value"
customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))

# Segment customers in 2014
customers_2014 = sqldf("SELECT customer_id,
                       MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM data
                       WHERE days_since > 365
                       GROUP BY 1")
customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & customers_2014$recency > 365*2)] = "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] = "active"

customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2)] = "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 100)] = "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 100)] = "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 100)] = "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 100)] = "active high value"
customers_2014$segment = factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))

# --- COMPUTE TRANSITION MATRIX ----------------------------


# Compute transition matrix
new_data = merge(x = customers_2014, y = customers_2015, by = "customer_id", all.x = TRUE)
head(new_data)
transition = table(new_data$segment.x, new_data$segment.y)
print(transition)

# Divide each row by its sum
transition = transition / rowSums(transition)
round(print(transition), 4)

# --- USE TRANSITION MATRIX TO MAKE PREDICTIONS ------------


# Initialize a matrix with the number of customers in each segment today and after 10 periods
segments = matrix(nrow = 8, ncol = 11)

segments[, 1] = table(customers_2015$segment)
colnames(segments) = 2015:2025
row.names(segments) = levels(customers_2015$segment)
print(segments)

# Compute for each an every period
for (i in 2:11) {
  segments[, i] = segments[, i-1] %*% transition
}
round(segments,0)

# Plot inactive, active high value customers over time
barplot(segments[1, ], main = "inactive")
barplot(segments[2, ], main = "cold")
barplot(segments[3, ], main = "warm high value")
barplot(segments[4, ], main = "warm low value")
barplot(segments[5, ], main = "new warm")
barplot(segments[6, ], main = "active high value")
barplot(segments[7, ], main = "active low value")
barplot(segments[8, ], main = "new active")

# Display how segments will evolve over time
print(round(segments))


# --- COMPUTE THE (DISCOUNTED) CLV OF A DATABASE -----------


# Yearly revenue per segment
# This comes directly from module 2, lines 160-161
y = aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)
yearly_revenue = round(y[,2],2)

# Compute revenue per segment
revenue_per_segment = yearly_revenue * segments
print(revenue_per_segment)

# Compute yearly revenue
yearly_revenue = colSums(revenue_per_segment)
print(round(yearly_revenue))
barplot(yearly_revenue, main = 'yearly revenue')

# Compute cumulated revenue
cumulated_revenue = cumsum(yearly_revenue)
print(round(cumulated_revenue))
barplot(cumulated_revenue)

# Create a discount factor
discount_rate = 0.10
discount = 1 / ((1 + discount_rate) ^ ((1:11) - 1))
print(discount)

# Compute discounted yearly revenue
disc_yearly_revenue = yearly_revenue * discount
print(round(disc_yearly_revenue))
barplot(disc_yearly_revenue, main = 'Discounted Yearly Revenue')
lines(yearly_revenue)

# Compute discounted cumulated revenue
disc_cumulated_revenue = cumsum(disc_yearly_revenue)
print(round(disc_cumulated_revenue))
barplot(disc_cumulated_revenue)

# What is the database worth?
print(disc_cumulated_revenue[11] - yearly_revenue[1])
