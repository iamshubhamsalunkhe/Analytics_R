rm(list=ls())
library(arules)
library(arulesViz)

?Groceries
View(Groceries)
data <- Groceries
str(Groceries)
?str
inspect(Groceries)
adv <- read.transactions(file.choose())
?apriori
rules <- apriori(adv, parameter = list(supp = 0, conf = 0.7, target = "rules"))
summary(rules)

?is.redundant
