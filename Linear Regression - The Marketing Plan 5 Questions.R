rm(list = ls())

# Advertising data displays sales (in thousands of units) for a particular product as a 
# function of advertising budgets (in thousands of dollars) for TV, radio, and newspaper 
# media. 
# 
# Suppose that in our role as a Data Scientist we are asked to suggest, on the basis of 
# this data, a marketing plan for next year that will result in high product sales. 
# Here are a few important questions that we might seek to address: 

#   1.	Is there a relationship between advertising budget and sales? 
#   2.	How strong is the relationship between advertising budget and sales?
#   3.	Which media contribute to sales? Do all three media - TV, radio, and newspaper - 
#       contribute to sales, or do just one or two of the media contribute?
#   4.	How accurately can we estimate the effect of each medium on sales? 
#       For every dollar spent on advertising in a particular medium, by what 
#       amount will sales increase? 
#   5.	Is there synergy among the advertising media?


advertisment <- read.csv("Advertising.csv" , header = T)
head(advertisment)
summary(advertisment)

#   1.	Is there a relationship between advertising budget and sales?


modeladv <- lm(Sales ~ TV + Radio + Newspaper , data = advertisment)
summary(modeladv)

modeltvN <- lm(Sales ~ TV + Newspaper , data = advertisment)
summary(modeltvN)


modeltvR <- lm(Sales ~ TV + Radio + TV * Radio, data = advertisment)
summary(modeltvR)

modelRN <- lm(Sales ~ Radio + Newspaper , data = advertisment)
summary(modelRN)

modelTVRTVR


#2.How strong is the relationship between advertising budget and sales?


corr_matrix_adv <- cor(advertisment)
corrplot(corr_matrix_adv)

corrplot(corr_matrix_adv , order = "hclust")
    
































