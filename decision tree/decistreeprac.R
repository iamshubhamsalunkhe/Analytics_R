##fitting classification tree 

library(tree)

datatree <- read.csv("decisiontree.csv")
head(datatree)
str(datatree)
View(datatree)
dim(datatree)

train <- datatree[1:1450,]
test <- datatree[1451:1728,]

head(train)
head(test)
colSums(is.na(datatree))


tree_data <- tree(train$Buying ~ ., data = train)

summary(tree_data)

plot(tree_data)
text(tree_data)


tree_pred <- predict(tree_data , data = test , type = "class")
table(tree_pred)







