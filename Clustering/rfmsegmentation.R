rm(list=ls())

# --- Computing recency , frequency , monetary value

#load file into local variable called 'data



data = read.csv('Purchase_Segment.txt', header = FALSE , sep = '\t' , dec = '.')
head(data)

#View(data)
#add headers and interpret the last column as a date 

colnames(data) = c('customer_id' , 'purchase_amount' , 'date_of_purchase')

data$date_of_purchase = as.Date(data$date_of_purchase , "%Y-%m-%d")


summary(data)


data$days_since = as.numeric(difftime(time1 = "2016-01-01",
                                      time2 = data$date_of_purchase,
                                      units = "days"))


#display data  after tranformation



head(data)
summary(data)
nrow(data)


#compute key marketing indicators using sql language

library(sqldf)


#compute recency and freq and avg puchase amt

customers = sqldf("SELECT customer_id ,
                  MIN(days_since) AS 'recency',
                  COUNT(*) AS 'frequency',
                  AVG(purchase_amount) AS 'amount'
                  FROM data GROUP BY 1")

#explore the data


head(customers)
summary(customers)

nrow(customers)


hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)

#####################preparing and transformation data

### copy customer data into new data frame 

new_data = customers


head(new_data)

row.names(new_data) = new_data$customer_id
new_data$customer_id =  NULL
head(new_data)


# take log transformation of the amount , and plot


hist(new_data$amount ,  breaks = 10)
hist(new_data$amount ,  breaks = 50)
hist(new_data$amount ,  breaks = 100)

summary(new_data)

new_data$amount = log(new_data$amount)
hist(new_data$amount)

#standardize variables




new_data = scale(new_data)


head(new_data)
summary(new_data)


## Running a herarchical segmentation 

##Compute distance metrics on stan

#d = dist(new_data)
#d

sample = seq(1 , 18)
















































