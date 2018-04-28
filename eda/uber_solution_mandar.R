############# UBER CASE STUDY #################
# Submitted by: Mandar Jamble [Applicant ID = APFE1785873]

################# ASSUMPTIONS #####################
# 1. This code is developed & run on Windows 10 platform
# 2. Working directory is correctly set
# 3. All files are present in the working directory

################# Business Problem #####################
# 1. Identify the root cause of Uber cancellation and non-availability of cars
# 2. Identify demand-supply gap
# 3. Identify possible hypothesis for the problem
# 4. Recommend ways to improve

library(tidyr)
library(dplyr)

###### Load & Clean Dataset######
uberTrips <- read.csv("Uber Request Data.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
uberTrips[2:4] <- lapply(uberTrips[2:4], factor) # Coerce Pickup.point, Driver.id & Status to factors  
str(uberTrips) # No need to further format text columns  

# Cursory check for duplicates on trips
sum(duplicated(uberTrips$Request.id)) # No duplicates

# NA check
sum(is.na(uberTrips)) # 6564 entries are NA
sapply(uberTrips, function(x) length(which(is.na(x)==T))) # Driver.id & Drop.timestamp have NA

# Blank check
sapply(uberTrips, function(x) length(which(x == ""))) # no blanks

# Update Date-Time columns
install.packages("lubridate")
library(lubridate)
uberTrips[5:6] <- lapply(uberTrips[5:6], function(x) parse_date_time(x, orders = c("%d/%m/%Y %H:%M", "%d-%m-%Y %H:%M:%S")))

#uberTrips <- group_by(uberTrips, Driver.id, Status, Request.timestamp)


# Calculate trip duration in minutes
uberTrips$Duration <- round(difftime(uberTrips$Drop.timestamp,uberTrips$Request.timestamp,units="mins"))

# Determine time intervals
#   Midnight: 00:00 hrs to 06:00 hrs 
#   Early-Day: 06:01 hrs to 11:00 hrs
#   Mid-Day: 11:01 hrs to 18:00 hrs
#   Evening: 18:01 hrs to 23:59 hrs
install.packages("chron") # For times function
library(chron)

uberTrips$Interval <- "NA"

midnight_start <- times(strftime(as.POSIXct(paste(as.Date(uberTrips[1,5]),"00:00:00")),"%H:%M:%S"))
midnight_end <- times(strftime(as.POSIXct(paste(as.Date(uberTrips[1,5]),"06:00:00")),"%H:%M:%S"))
uberTrips$Interval[which(between(times(strftime(as.POSIXlt(uberTrips$Request.timestamp),"%H:%M:%S")),midnight_start, midnight_end))] <- "Midnight"

early_start <- times(strftime(as.POSIXct(paste(as.Date(uberTrips[1,5]),"06:00:01")),"%H:%M:%S"))
early_end <- times(strftime(as.POSIXct(paste(as.Date(uberTrips[1,5]),"11:00:00")),"%H:%M:%S"))
uberTrips$Interval[which(between(times(strftime(as.POSIXlt(uberTrips$Request.timestamp),"%H:%M:%S")),early_start, early_end))] <- "Early-Day"

mid_day_start <- times(strftime(as.POSIXct(paste(as.Date(uberTrips[1,5]),"11:00:01")),"%H:%M:%S"))
mid_day_end <- times(strftime(as.POSIXct(paste(as.Date(uberTrips[1,5]),"18:00:00")),"%H:%M:%S"))
uberTrips$Interval[which(between(times(strftime(as.POSIXlt(uberTrips$Request.timestamp),"%H:%M:%S")),mid_day_start, mid_day_end))] <- "Mid-Day"

evening_start <- times(strftime(as.POSIXct(paste(as.Date(uberTrips[1,5]),"18:00:01")),"%H:%M:%S"))
evening_end <- times(strftime(as.POSIXct(paste(as.Date(uberTrips[1,5]),"23:59:59")),"%H:%M:%S"))
uberTrips$Interval[which(between(times(strftime(as.POSIXlt(uberTrips$Request.timestamp),"%H:%M:%S")),evening_start, evening_end))] <- "Evening"

uberTrips$req_day <- format(uberTrips$Request.timestamp, "%d")
uberTrips$req_hour <- format(uberTrips$Request.timestamp, "%H")
uberTrips$req_day0fWeek <- weekdays(uberTrips$Request.timestamp)

uberTrips$drop_day <- format(uberTrips$Drop.timestamp, "%d")
uberTrips$drop_hour <- format(uberTrips$Drop.timestamp, "%H")

library(reshape2)
library(ggplot2)
library(scales)

day_plot <- ggplot(uberTrips, aes(req_day, y = (..count..), fill = Status)) + 
  geom_histogram(stat = "count", binwidth=1) +
  geom_text(aes(y =(..count..),
                label = (..count..)), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 4) + 
  labs(title = "Day Analysis", x = "Day", y = "Trip Count")

day_plot

# trip_status_plot <- ggplot(data = uberTrips, aes(x = Status, fill = Status)) +
#  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
#  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
#                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
#            stat = 'count', 
#            position = position_dodge(0.3), 
#            size = 4) + 
#  labs(title = "Trip Status Analysis", x = "Trip status", y = "Percent Total Trips")


install.packages("plotly")
library(plotly)

trip_status_plot <- plot_ly(as.data.frame(summary(uberTrips$Status)), 
        labels = names(as.list(summary(uberTrips$Status))), 
        values = as.vector(summary(uberTrips$Status)), 
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        showlegend = TRUE) %>%
  layout(title = 'Trip Status Analysis',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

trip_req_plot <- ggplot(uberTrips, aes(Pickup.point, y = prop.table(..count..)* 100, fill = Status)) + 
  geom_histogram(stat = "count", binwidth=1) +
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5,
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 4) + 
  labs(title = "Trip Requests Analysis", x = "Trip Pickup", y = "Percent Total Trips")

par(mfrow=c(1,2))
trip_status_plot
trip_req_plot

time_slot_plot <- ggplot(data = uberTrips, aes(x = Interval, fill = Pickup.point)) +
  geom_bar(aes(y = prop.table(..count..) * 100), position = "dodge") + 
  geom_text(aes(y = prop.table(..count..) * 100 + 0.5, 
                label = paste0(round(prop.table(..count..) * 100, digits=1), '%')), 
            stat = 'count', 
            position = position_dodge(1), 
            size = 4) + 
  labs(title = "Time-slot Analysis", x = "Time slot", y = "Percent Total Trips") +
  scale_x_discrete(limits=c("Midnight","Early-Day","Mid-Day","Evening"))

time_slot_plot

trips_by_hour_plot <- ggplot(uberTrips, aes(req_hour, y = (..count..), fill = Status)) + 
  geom_histogram(stat = "count", bandwidth=1) + 
  geom_text(aes(y = (..count..), label = (..count..)), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 3) +
  labs(title = "Requests By Hour Analysis", x = "Hour", y = "Trip Count")

#ggplot(uberTrips, aes(x=req_hour, y = ((..count..)* 100/sum(..count..)), fill = Status)) + 
#  geom_bar(stat = "count", position = "fill") +
#  geom_text(aes(x=req_hour, y = (prop.table(..count..) * 100/sum(prop.table(..count..))) + 0.5,
#                label = paste0(round((prop.table(..count..) * 100/sum(prop.table(..count..))), digits=1), '%')), 
#            stat = 'count',
#            position=position_fill(vjust=0.5),
#            size = 3) +
#  scale_y_continuous(labels = percent_format()) +
#  labs(title = "Trip By Hour Analysis", x = "Hour", y = "Percent")

trips_by_hour_plot

from_city_plot <- ggplot(subset(uberTrips, uberTrips$Pickup.point=="City"), aes(req_hour, y = (..count..), fill = Status)) + 
  geom_histogram(stat = "count", binwidth = 1) +
  geom_text(aes(y = (..count..), label = (..count..)), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 3) +
  labs(title = "City-to-Airport Trips", x = "Hour", y = "Trip Count") 

from_city_plot

from_airport_plot <- ggplot(subset(uberTrips, uberTrips$Pickup.point=="Airport"), aes(req_hour, y = (..count..), fill = Status)) + 
  geom_histogram(stat = "count", binwidth = 1) +
  geom_text(aes(y = (..count..), label = (..count..)), 
            stat = 'count',
            position=position_stack(vjust=0.5),
            size = 3) +
  labs(title = "Airport-to-City Trips", x = "Hour", y = "Trip Count") 

from_airport_plot

trips_to_city <- subset(uberTrips, uberTrips$Pickup.point=="Airport")
trips_to_airport <- subset(uberTrips, uberTrips$Pickup.point=="City")

avg_duration_by_hour_toCity <- aggregate(trips_to_city$Duration, by=list(Category=trips_to_city$req_hour), FUN=mean, na.rm=T)
avg_duration_by_hour_toCity$x <- round(avg_duration_by_hour_toCity$x, digits=1)

avg_duration_by_hour_toAirport <- aggregate(trips_to_airport$Duration, by=list(Category=trips_to_airport$req_hour), FUN=mean, na.rm=T)
avg_duration_by_hour_toAirport$x <- round(avg_duration_by_hour_toAirport$x, digits=1)

colnames(avg_duration_by_hour_toCity) <- c("Hour","Avg_Duration_ToCity")
colnames(avg_duration_by_hour_toAirport) <- c("Hour","Avg_Duration_ToAirport")
avg_duration_by_hour <- merge(avg_duration_by_hour_toCity, avg_duration_by_hour_toAirport, by.x = "Hour", by.y = "Hour", all.x = TRUE)

avg_duration_by_hour$Avg_Duration_ToCity <- as.numeric(avg_duration_by_hour$Avg_Duration_ToCity)
avg_duration_by_hour$Avg_Duration_ToAirport <- as.numeric(avg_duration_by_hour$Avg_Duration_ToAirport)

ggplot(data = avg_duration_by_hour, aes(x = Hour, y=Avg_Duration_ToCity)) + geom_point() + geom_smooth()
ggplot(data = avg_duration_by_hour, aes(x = Hour, y=Avg_Duration_ToAirport)) + geom_point() + geom_smooth()

#ggplot(uberTrips[which(!is.na(uberTrips$drop_hour)),], aes(drop_hour, fill = Pickup.point)) + geom_histogram(stat = "count")

trip_drop_plot <- ggplot(uberTrips[which(!is.na(uberTrips$drop_hour)),], aes(drop_hour, y = (..count..))) + 
  geom_histogram(stat = "count", bandwidth=1) + 
  geom_text(aes(y = (..count..), label = (..count..)), 
            stat = 'count',
            vjust = -0.5,
            size = 4) +
  labs(title = "Drops By Hour Analysis", x = "Hour", y = "Trip Count")

trip_drop_plot

library(plyr)

gap_to_airport <- count(trips_to_airport, c("req_hour", "Status"))
gap_to_airport_wide <- dcast(gap_to_airport, req_hour ~ Status)
gap_to_airport_wide[is.na(gap_to_airport_wide)] <- 0
gap_to_airport_wide$gap <- 100*(gap_to_airport_wide$Cancelled + gap_to_airport_wide$`No Cars Available`) / (gap_to_city_wide$Cancelled+gap_to_city_wide$`No Cars Available`+gap_to_city_wide$`Trip Completed`)
gaps_to_airport_plot <- ggplot(gap_to_airport_wide, aes(x = req_hour, y = gap)) +   
  geom_bar(stat = "identity", fill = "#F1948A") +   
  geom_text(aes(y = gap, label = paste0(round(gap, digits=0),'%')), 
            stat = 'identity',
            vjust = -0.5,
            size = 4) +
  labs(title="Gaps by Hour - To Airport", x = "Hour", y = "Gap Percent") 

gaps_to_airport_plot

gap_to_city <- count(trips_to_city, c("req_hour", "Status"))
gap_to_city_wide <- dcast(gap_to_city, req_hour ~ Status)
gap_to_city_wide[is.na(gap_to_city_wide)] <- 0
gap_to_city_wide$gap <- 100*(gap_to_city_wide$Cancelled + gap_to_city_wide$`No Cars Available`) / (gap_to_city_wide$Cancelled+gap_to_city_wide$`No Cars Available`+gap_to_city_wide$`Trip Completed`)
gaps_to_city_plot <- ggplot(gap_to_city_wide, aes(x = req_hour, y = gap)) +   
  geom_bar(stat = "identity", fill = "#73C6B6") +   
  geom_text(aes(y = gap, label = paste0(round(gap, digits=0),'%')), 
            stat = 'identity',
            vjust = -0.5,
            size = 4) +
  labs(title="Gaps by Hour - To City", x = "Hour", y = "Gap Percent") 

gaps_to_city_plot


gaps_by_drops <- count(uberTrips[which(uberTrips$Status!="Trip Completed"),], "req_hour")
gaps_by_drops$drops <- count(uberTrips[which(!is.na(uberTrips$drop_hour)),], "drop_hour") [,2]
colnames(gaps_by_drops) <- c("req_hour","incomplete_reqs","drops")
gaps_by_drops$gap <- round(100*(gaps_by_drops$incomplete_reqs-gaps_by_drops$drops)/gaps_by_drops$incomplete_reqs, digits=1)

gaps_by_drops_plot <- ggplot(gaps_by_drops, aes(x = req_hour, y = gap)) +   
  geom_bar(stat = "identity", fill = "#73C6B6") +   
  geom_text(aes(y = gap, label = paste0(round(gap, digits=0),'%')), 
            stat = 'identity',
            vjust = -0.5,
            size = 4) +
  labs(title="Gaps by Hour [Un-serviced - Drops]", x = "Hour", y = "Gap Percent") 

gaps_by_drops_plot

gap_analysis <- count(trips_to_city, "req_hour")
colnames(gap_analysis) <- c("Hour","City_requests")

gap_analysis$City_drops <- count(trips_to_airport[which(!is.na(trips_to_airport$drop_hour)),], "drop_hour") [,2]
gap_analysis$Airport_requests <- count(trips_to_airport, "req_hour")[,2]
gap_analysis$Airport_drops <- count(trips_to_city[which(!is.na(trips_to_city$drop_hour)),], "drop_hour")[,2]
gap_analysis$To_city_gap <- (gap_analysis$City_requests - gap_analysis$Airport_drops)*100/gap_analysis$City_requests
gap_analysis$To_airport_gap <- (gap_analysis$Airport_requests - gap_analysis$City_drops)*100/gap_analysis$Airport_requests

gap_analysis <- gap_analysis[, c("Hour","To_city_gap", "To_airport_gap")]
gap_analysis.m <- melt(gap_analysis, id.vars='Hour')

gaps_by_hour_plot <- ggplot(gap_analysis.m, aes(x = Hour, y = value, fill = variable)) +   # Fill column
  geom_bar(stat = "identity", position = "stack") +   # draw the bars
  geom_text(aes(y = value, label = paste0(round(value, digits=1),'%')), 
            stat = 'identity',
            position=position_stack(vjust=0.5),
            size = 3) +
  labs(title="Gaps by Hour", x = "Hour", y = "Gap Count") 

gaps_by_hour_plot
