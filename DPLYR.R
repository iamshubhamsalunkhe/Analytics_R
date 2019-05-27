rm(list=ls())
#install.packages('dplyr')
library(dplyr)
library(hflights)

hflights
hflights <- tbl_df(hflights)
hflights

#View(hflights)

# The Five Verbs of dplyr 1. The dplyr package contains ???ve key data manipulation functions, also called verbs:
#   . select(), which returns a subset of the columns,
#   . filter(), that is able to return a subset of the rows, 
#   . arrange(), that reorders the rows according to single or multiple variables, 
#   . mutate(), used to add columns from existing data, 
#   . summarise(), which reduces each group to a single row by calculating aggregate measures.
# What order of operations should we use to to ???nd the average value of the ArrDelay (arrival delay) 
# variable for all American Airline ???ights in the h???ights tbl?

## Manipulate variables

# 2. Return a copy of h???ights that contains the four columns related to delay
# (ActualElapsedTime, AirTime, ArrDelay, DepDelay).

# hflights <- tbl_df(hflights)
# hflights

head(hflights)

g <- select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)

g[1,]

# 3.  Return a copy of h???ights containing the columns Origin up to Cancelled 

select(hflights, Origin:Cancelled)

# 4. Find the most concise way to select: columns Year up to and including DayOfWeek,
# columns ArrDelay up to and including Diverted.

a <- colnames(hflights)
a

select(hflights, Year:DayOfWeek, ArrDelay:Diverted)
select(hflights, c(Year:DayOfWeek, ArrDelay:Diverted))
select(hflights, c(1:4, 12:21))
select(hflights, -c(5:11))

# 6. Use a combination of helper functions and variable names to return the UniqueCarrier, FlightNum, TailNum, 
# Cancelled, and CancellationCode columns of h???ights

select(hflights, UniqueCarrier, 
       ends_with("Num"),
       starts_with("Cancel"))

# 7.  Which variables in h???ight do you think count as a plane's "ground time"? Use mutate() to add these 
#variables together and save them as GroundTime. Save your results as g.

f <- mutate(hflights, GroundTimes = TaxiIn + TaxiOut)

# 8 Return a copy of all ???ights that traveled 3000 miles or more. Save it in f1. 

"a" %in% c("b", "a", "c")

f1 <- filter(hflights, Distance > 3000)

# 9. Return a copy of all ???ights ???ights where taxiing took longer than ???ying. Save it in f3. 

filter(hflights, TaxiIn + TaxiOut > AirTime)

## OR

filter(f, GroundTimes > AirTime)

# 10. Return a copy of all cancelled weekend ???ights 

filter(hflights, DayOfWeek > 5 & Cancelled == 1)

filter(hflights, DayOfWeek %in% c(6,7), Cancelled == 1)

sum(is.na(select(hflights,DepTime)))

# 11. Arrange according to carrier and decreasing departure delays

arrange(hflights, desc(UniqueCarrier), desc(DepDelay))

# 12. Arrange ???ights by total delay (normal order).

arrange(hflights, DepDelay + ArrDelay)

# 13. Filter out ???ights leaving to DFW before 8am and arrange according to decreasing AirTime

colnames(hflights)
g <- filter(hflights, Dest == "DFW", DepTime < 800)
arrange(g, desc(AirTime))

## OR

arrange(filter(hflights, Dest == "DFW", DepTime < 800), desc(AirTime))

## OR

hflights %>%
  filter(Dest == "DFW", DepTime < 800) %>%
  arrange(desc(AirTime))

# Manipulating Groups of Observation (summarize and group_by) 
# 14. Determine the shortest and longest distance ???own and save statistics to min_dist and max_dist resp.

summarise(hflights, min_dist = min(Distance), max_dist = max(Distance))

## OR

hflights %>%
  summarize(min_dist = min(Distance), max_dist = max(Distance))

# 15. Determine the longest distance for diverted ???ights, save statistic to max_div. Use a one-liner!
  
summarize(filter(hflights, Diverted == "1"), max_dist = max(Distance))

## OR

hflights %>%
  filter(Diverted == "1") %>%
  summarize(max_dist = max(Distance))

# dplyr provides several helpful aggregate functions of its own, in addition to the ones 
# that are already de???ned in R. These include:
# . first(x) - The ???rst element of vector x. 
# . last(x) - The last element of vector x. 
# . nth(x, n) - The nth element of vector x. 
# . n() - The number of rows in the data.frame or group of observations that summarise() describes. 
# . n_distinct(x) - The number of unique values in vector x.

# 16. Create a table with the following variables (and variable names): the total number of observations in 
# h???ights (n_obs), the total number of carriers that appear in h???ights (n_carrier), the total number of 
# destinations that appear in h???ights (n_dest), and the destination of the ???ight that appears in the 100th 
# row of h???ights (dest100).

hflights %>%
  summarise(n_obs = n(),
            n_carriers = n_distinct(UniqueCarrier),
            n_dest = n_distinct(Dest),
            dest100 = nth(Dest,100))

#Use Piping: (1) Take the hflights data set and then, (2) Add a variable named diff that is the result of 
# subtracting TaxiIn from TaxiOut, and then (3) pick all of the rows whose diff value does not equal NA, and 
# then (4) summarise the data set with a value named avg that is the mean diff value. Store the result in the 
# variable p. 

p <- hflights %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(diff != "NA") %>%
  summarise(avg = mean(diff))

#18. Use Piping: De???ne a data set named d that contains just the Dest, UniqueCarrier, Distance, and ActualElapsedTime 
#columns of hflights as well an additional variable: RealTime which is equal the actual elapsed time plus 100 minute.

hflights %>%
  select(Dest, UniqueCarrier, Distance, ActualElapsedTime) %>%
  mutate(RealTime = ActualElapsedTime + 100)

# 19. Use Piping to compare the individual carriers. For each carrier, count the total number of ???ights ???own 
# by the carrier (n_???ights), the total number of cancelled ???ights (n_canc), and the average arrival delay of the 
# ???ights whose delay does not equal NA (avg_delay). Once you've calculated these results, arrange() the carriers 
# from low to high by their average arrival delay. Use number of ???ights cancelled to break any ties. Which airline
# scores best based on these statistics?

hflights %>%
  group_by(UniqueCarrier) %>%
  summarize(n_flights = n(),
            n_canc = sum(Cancelled == 1),
            avg_delay = mean(ArrDelay, na.rm =T)) %>%
  arrange(avg_delay, n_canc)
