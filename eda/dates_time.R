# Working with dates and time in R

# 0. Basics 
# 1. Date formats
# 2. Converting into dates using as.Date
# 3. Extracting days, months, quarters, years from date objects
# 4. Working with time






# 0. Basics: standard format in R is yyyy-mm-dd
Sys.Date()
Sys.Date() + 10
typeof(Sys.Date())



# Dates are stored as days since 1st January 1970 
as.integer(Sys.Date())
unclass(Sys.Date())



# 1.Date formats


# 2. Converting into dates using as.Date
# 1.1 %d-%m-%Y

typeof("25-12-2017")
d1 = as.Date("25-12-2017", format="%d-%m-%Y")
d1 # returns in format %Y-%m-%d
typeof(d1)

d2 = as.Date("12-25-2017", format="%m-%d-%Y")
d2

d2 + 1000

# 1.2 %d-%m-%y
d3 = as.Date("25-12-17", format = "%d-%m-%y")
d3

# dates with slashes
as.Date("25/12/2017", format = "%d/%m/%Y")

# 1.3 Month formats: %b and %B
d4 = as.Date("25-Dec-2017", format = "%d-%b-%Y")
d4

d5 = as.Date("25-December-17", format = "%d-%B-%y")
d5

# Example 2.1: investment case study rounds file
rounds <- read.csv("rounds2.csv", nrows = 100)
str(rounds)
View(rounds)

fund_date <- rounds$funded_at
typeof(fund_date)
fund_date + 2


fund_date <- as.Date(fund_date, format="%d-%m-%Y")
typeof(fund_date) # converts to Y-m-d format
fund_date + 2

# Example 2.2: Dates in a non-standard format
prices <- read.csv("gold_silver.csv")
View(prices)
str(prices)

price_date <- as.Date(prices$Month, format = "%b-%y") # returns NA
price_date

# solution - add a dummy day using paste()
prices$Month <- paste("01-", prices$Month, sep="")
prices$Month <- as.Date(prices$Month, format = "%d-%b-%y")
View(prices)

# 3. Extracting days, months, quarters, years from date objects
# method 1
prices$day <- weekdays(prices$Month)
prices$month <- months(prices$Month)
prices$quarters <- quarters(prices$Month)
prices$years <- years(prices$Month) # does not work

# method 2: using format()
# extracting years 
prices$years <- format(prices$Month, "%Y")
prices$months2 <- format(prices$Month, "%m") # similarly for days 

# 4. Time in R

# Default format: "%Y-%m-%d %H:%M:%S"
Sys.time()
typeof(Sys.time())

# number of seconds since midnight of January 1, 1970
unclass(Sys.time())

# define a new datetime object 
date_time = "2017-12-25 10:17:07"
typeof(date_time)

# Two POSIX classes - POSIXct and POSIXlt

# POSIXct
?as.POSIXct()
date_time = as.POSIXct(date_time)
unclass(date_time) 

# POSIXlt
date_time = "2017-04-27 10:17:07"
date_time = as.POSIXlt(date_time, format = "%Y-%m-%d %H:%M:%S")
d = unclass(date_time)
d
View(d)

hour = format(date_time, "%H")
mins = format(date_time, "%M")
hour
mins

# strptime
date_time = "2017-12-25 10:17:07"
s = strptime(date_time, format = "%Y-%m-%d %H:%M:%S")
unclass(s)
View(unclass(s))









# Example
grades <- read.csv("grades.csv")
grades <- data.frame(grades$submitted_at, grades$user.id, grades$marks)
colnames(grades) <- c("submit_time", "user_id", "marks")
View(grades)
str(grades)

# convert to datetime
grades$t1 <- as.POSIXlt(grades$submit_time, format = "%m/%d/%y-%H:%M:%S")

# alternative
grades$t2 <- strptime(grades$submit_time, format = "%m/%d/%y-%H:%M:%S")


# Extract hour, minute, second
grades$sec <- format(grades$t1, "%S")
grades$min <- format(grades$t1, "%M")
grades$hour <- format(grades$t1, "%H")
grades$date <- as.Date(grades$t1)
grades$day <- format(grades$date, "%d")
grades$month <- format(grades$date, "%m")

# some analysis!
library(ggplot2)
ggplot(grades, aes(month)) + geom_histogram(stat = "count")
ggplot(grades, aes(day)) + geom_histogram(stat = "count")
ggplot(grades, aes(hour)) + geom_histogram(stat = "count")
ggplot(grades, aes(min)) + geom_histogram(stat = "count")

# grouping by months
library(dplyr)
grades$month <- as.factor(grades$month)

table(grades$month)
aggregate(marks~month, grades, mean)

#only January submissions
grades <- subset(grades, month=="01")

# day
ggplot(grades, aes(day)) + geom_histogram(stat = "count")

day_marks <- data.frame(aggregate(marks~day, grades, median))
day_marks

str(day_marks)
ggplot(day_marks, aes(day, marks))+geom_bar(stat="identity")

# hour
ggplot(grades, aes(hour)) + geom_histogram(stat = "count")
hour_marks <- data.frame(aggregate(marks~hour, grades, median))
hour_marks
ggplot(hour_marks, aes(hour, marks))+geom_bar(stat="identity")
