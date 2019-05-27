#1. Create Dataframe (df) combining the following vectors

#numvec = c(2,5,8,9,0,6,7,8,4,5,7,11)

#charvec = c("David","James","Sara","Tim","Pierre","Janice","Sara","Priya","Keith", "Mark", "Apple", "Sara")

#gender = c("M","M","F","M","M","M","F","F","F","M","M","F")

#state = c("CO","KS","CA","IA","MO","FL","CA","CO","FL","CA","WY","AZ")

#Subset dataframe using the following operations



#b. charvec = "Sara"

#c. Display (names, state) for numvec = 5

#d. charvec != "Sara" & gender == "F" & numvec > 5



numvec <- c(2,5,8,9,0,6,7,8,4,5,7,11)
numvec

charvec <- c("David","James","Sara","Tim","Pierre","Janice","Sara","Priya","Keith", "Mark", "Apple", "Sara")
charvec

gender <- c("M","M","F","M","M","M","F","F","F","M","M","F")
gender

state <- c("CO","KS","CA","IA","MO","FL","CA","CO","FL","CA","WY","AZ")
state

person.data <- data.frame(numvec,charvec,gender,state)
person.data


df <- data.frame(numvec,charvec,gender,state)
df



df$numvec<5
#a. numvec<5
df$numvec[df$numvec<5]

df[df$charvec =="Sara",]


df[df$numvec == 5 , c (3,4)]


#d. charvec != "Sara" & gender == "F" & numvec > 5

df[df$charvec != "Sara" & gender == "F" & df$numvec > 5 , ]



### Q2 for coffee

gender_coffee <- c("M","F","M","F","M","F","M","F","M","F")

age_coffee <- c(25,56,63,45,15,23,28,39,56,21)

coffee <-  c(3, 1, 2, 5, 0, 2, 0, 1, 3, 2) 


dfcoffee <- data.frame(gender_coffee,age_coffee,coffee)
dfcoffee



#a. Add the coffee vector into the dataframe to form a single combined dataframe.

#b. Subset only 2nd column from the dataframe.

#c. Subset rows (1, 3, 8) from the dataframe.

dfcoffee[,2]


















