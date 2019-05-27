rm(list=ls())

# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------

# Load text file into local variable called 'data'
data = read.csv('Purchase_Segment.txt', header = FALSE, 
                sep = '\t', dec = '.')
head(data)

#View(data)
# Add headers and interpret the last column as a date, extract year of purchase

colnames(data) = c('customer_id', 'purchase_amount', 
                   'date_of_purchase')

data$date_of_purchase = as.Date(data$date_of_purchase,
                                "%Y-%m-%d")

summary(data)

data$days_since = as.numeric(difftime(time1 = "2016-01-01",
                                      time2 = data$date_of_purchase,
                                            units = "days"))
summary(data)

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

# Explore the data

# View(customers)
head(customers)
summary(customers)
nrow(customers)

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
hist(new_data$amount, breaks = 10)
hist(new_data$amount, breaks = 50)
hist(new_data$amount, breaks = 100)
summary(new_data)

new_data$amount = log(new_data$amount)
hist(new_data$amount)

#View(new_data$amount)

# Standardize variables
new_data = scale(new_data)

head(new_data)
summary(new_data)

# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------

# Compute distance metrics on standardized data
# This will likely generate an error on most machines
# d = dist(new_data)
# d

# Take a 10% sample
sample = seq(1, 18417, by = 10)
head(sample)
customers_sample = customers[sample, ]
new_data_sample  = new_data[sample, ]

# Compute distance metrics on standardized data
d = dist(new_data_sample)
length(d)

# Perform hierarchical clustering on distance metrics
c = hclust(d, method="ward.D2")

# Plot the dendogram
plot(c)

# Cut at 9 segments
members = cutree(c, k = 9)

# Show 30 first customers, frequency table
members[1:30]
table(members)

# Show profile of each segment
aggregate(customers_sample[, 2:4], by = list(members), mean)
head(customers_sample)

# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------

# Compute recency, frequency, and average purchase amount for 2015

customers = sqldf("SELECT customer_id,
                  MIN(days_since) AS 'recency',
                  MAX(days_since) AS 'first_purchase',
                  COUNT(*) AS 'frequency',
                  AVG(purchase_amount) AS 'amount'
                  FROM data GROUP BY 1")

head(customers)

# 4-segment solution using which
customers$segment = "NA"
customers$segment[which(customers$recency > 365*3)] = 
  "inactive" # Last Purchase (LP) Dec 2012
customers$segment[which(customers$recency <= 365*3 & 
                               customers$recency > 365*2)] = "cold" # LP in 2013
customers$segment[which(customers$recency <= 365*2 & 
                               customers$recency > 365*1)] = "warm" # LP in 2014
customers$segment[which(customers$recency <= 365)] = "active" # LP in 2015

table(customers$segment)

cust_seg_4 <- table(customers$segment)
cust_seg_4 <- as.data.frame(cust_seg_4)
cust_seg_4$V2 <- c("LP 2015", "LP 2013", "LP 2012", 'LP 2014')
colnames(cust_seg_4) = c('segments', 'No. of customers', 'segment definition')
cust_seg_4

cust_seg_4[order(cust_seg_4$`segment definition`),]

aggregate(x = customers[, 2:5], by = list(customers$segment), mean)

head(customers)

# Complete segment solution using which, and exploiting previous test as input

customers$segment = "NA"
customers$segment[which(customers$recency > 365*3)] = "inactive"
customers$segment[which(customers$recency <= 365*3 & 
                          customers$recency > 365*2)] = "cold"
customers$segment[which(customers$recency <= 365*2 & 
                          customers$recency > 365*1)] = "warm"
customers$segment[which(customers$recency <= 365)] = "active" 
customers$segment[which(customers$segment == "warm" & 
                          customers$first_purchase <= 365*2)] = "new warm"
customers$segment[which(customers$segment == "warm" & 
                          customers$amount < 100)] = "warm low value"
customers$segment[which(customers$segment == "warm" & 
                          customers$amount >= 100)] = "warm high value"
customers$segment[which(customers$segment == "active" & 
                          customers$first_purchase <= 365)] = "new active"
customers$segment[which(customers$segment == "active" & 
                          customers$amount < 100)] = "active low value"
customers$segment[which(customers$segment == "active" & 
                          customers$amount >= 100)] = "active high value"

table(customers$segment)

aggregate(x = customers[, 2:5], by = list(customers$segment), mean)

# Re-order factor in a way that makes sense
customers$segment = factor(x = customers$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))

aggregate(x = customers[, 2:5], by = list(customers$segment), mean)
# head(customers[customers$segment == 'new warm',])

table(customers$segment)

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

table(customers$segment)

head(customers)

aggregate(x = customers[, 2:5], by = list(customers$segment), mean)

Customer.Active.High.Value <- customers[customers$segment=="active high value", ]

Customer.Active.High.Value$customer_id[1:5]

Customer.New.Warm <- customers[customers$segment=="new warm", ]

View(Customer.New.Warm)
