rm(list=ls())
library(dplyr)
library(hflights)

# The Five Verbs of dplyr 1. The dplyr package contains five key data manipulation functions, also called verbs:
#   . select(), which returns a subset of the columns,
#   . filter(), that is able to return a subset of the rows, 
#   . arrange(), that reorders the rows according to single or multiple variables, 
#   . mutate(), used to add columns from existing data, 
#   . summarise(), which reduces each group to a single row by calculating aggregate measures.
# What order of operations should we use to to ???nd the average value of the ArrDelay (arrival delay) 
# variable for all American Airline ???ights in the hflights tbl?

## Manipulate variables

# 2. Return a copy of hflights that contains the four columns related to delay(ActualElapsedTime, AirTime, ArrDelay, DepDelay).



# 3.  Return a copy of hflights containing the columns Origin up to Cancelled 



# 4. Find the most concise way to select: columns Year up to and including DayOfWeek, columns ArrDelay up to and 
# including Diverted.



# 6. Use a combination of helper functions and variable names to return the UniqueCarrier, FlightNum, TailNum, 
# Cancelled, and CancellationCode columns of hflights



# 7.  Which variables in hflights do you think count as a plane's "ground time"? Use mutate() to add these 
#variables together and save them as GroundTime. Save your results as g.



# 8 Return a copy of all hflights that traveled 3000 miles or more. Save it in f1. 



# 9. Return a copy of all hflights flights where taxiing took longer than flying. Save it in f3. 



# 10. Return a copy of all cancelled weekend flights 



# 11. Arrange according to carrier and decreasing departure delays



# 12. Arrange flights by total delay (normal order).



# 13. Filter out flights leaving to DFW before 8am and arrange according to decreasing AirTime



# Manipulating Groups of Observation (summarize and group_by) 
# 14. Determine the shortest and longest distance and save statistics to min_dist and max_dist resp.



# 15. Determine the longest distance for diverted flights, save statistic to max_div. Use a one-liner!



# dplyr provides several helpful aggregate functions of its own, in addition to the ones 
# that are already de???ned in R. These include:
# . first(x) - The ???rst element of vector x. 
# . last(x) - The last element of vector x. 
# . nth(x, n) - The nth element of vector x. 
# . n() - The number of rows in the data.frame or group of observations that summarise() describes. 
# . n_distinct(x) - The number of unique values in vector x.

# 16. Create a table with the following variables (and variable names): the total number of observations in 
# hflights (n_obs), the total number of carriers that appear in hflights (n_carrier), the total number of 
# destinations that appear in hflights (n_dest), and the destination of the ???ight that appears in the 100th 
# row of hflights (dest100).

hflights %>%
  summarise(n_obs = n(),
            n_carriers = n_distinct(UniqueCarrier),
            n_dest = n_distinct(Dest),
            dest100 = nth(Dest,100))


# 17. Use Piping: (1) Take the hflights data set and then, (2) Add a variable named diff that is the result of 
# subtracting TaxiIn from TaxiOut, and then (3) pick all of the rows whose diff value does not equal NA, and 
# then (4) summarise the data set with a value named avg that is the mean diff value. Store the result in the 
# variable p. 

p <- hflights %>%
  mutate(diff = TaxiOut - TaxiIn) %>%
  filter(diff != "NA") %>%
  summarise(avg = mean(diff))

# 18. Use Piping: Define a data set named d that contains just the Dest, UniqueCarrier, Distance, and ActualElapsedTime 
# columns of hflights as well an additional variable: RealTime which is equal the actual elapsed time plus 100 minute.



# 19. Use Piping to compare the individual carriers. For each carrier, count the total number of flights ???own 
# by the carrier (n_hflights), the total number of cancelled flights (n_canc), and the average arrival delay of the 
# flights whose delay does not equal NA (avg_delay). Once you've calculated these results, arrange() the carriers 
# from low to high by their average arrival delay. Use number of flights cancelled to break any ties. Which airline
# scores best based on these statistics?

