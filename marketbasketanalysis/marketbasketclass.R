rm(list = ls())


library(arules)

library(arulesViz)
 
library(datasets)



data(Groceries)

?Groceries



itemFrequency(Groceries , type = "absolute")
itemFrequencyPlot(Groceries)
itemFrequencyPlot(Groceries , topN = 20)
itemFrequencyPlot(Groceries , topN = 20 , type = "absolute")




rules <- apriori(Groceries , parameter = list(supp =0.001 , conf = 0.8) , control = list(verbose = F))

options(digits = 2)
inspect(rules[1:5])


summary(rules)


rules <- sort(rules , by = "lift" , decreasing = TRUE)

inspect(rules[1:5])


rules <- sort(rules , by = "confidence" , decreasing =  TRUE)
inspect(rules[1:5])
 




