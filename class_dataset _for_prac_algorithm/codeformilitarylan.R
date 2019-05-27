########
rm(list=ls())


###Military LAN dataset##############


train <- read.csv(file.choose())  ##("Trian(1).csv")
head(train)
summary(train)
View(train)


###getting str of data in right format    
train$duration <- as.numeric(train$duration) #duration is continous 
train$protocol_type <- as.factor(train$protocol_type) #protocol_type is categorical
train$service <- as.factor(train$service) #service is categorical
train$flag <- as.factor(train$flag) # flag is categorical
train$src_bytes <- as.numeric(train$src_bytes) # src_bytes is continuos
train$dst_bytes <- as.numeric(train$dst_bytes) #dst_bytes is continuos
train$land <- as.factor(train$land) # land is categorical

###wrong_fragment is telling number of frags but it is consitent like it has 0 or 3 frags so 
## I am interested in it if there are frags are not so we will bin it 

train$wrong_fragment <- ifelse(train$wrong_fragment == 0 ,0,1)
head(train$wrong_fragment)

##continuing with structuring of data





test <- read.csv(file.choose()) ##("Test(1).csv")

head(test)




complete_data <- rbind(train,test)

summary(complete_data)

colSums(is.na(complete_data))


library(corrplot)


str(complete_data)

colnames(complete_data)


library(randomForest)


set.seed(1)

dim(complete_data)


rf_data <- randomForest(class ~ . - duration  , data = train,
                        mtry = 41, importance = TRUE)



range(complete_data$duration)

summary(train)
















