library(tidyverse)
library(plotly)
library(nycflights13)
library(data.table)
library(lubridate)
library(trelliscopejs)
flights
nycflights13::planes
flights_planes <- flights %>% left_join(planes, by = "tailnum") # Join with planes data to get manufacturer, model, some specifications 
flights_planes<- flights_planes %>%rename(model_year = "year.y")


str(flights)
Flights <- as.data.table(flights_planes)
# keep only conducted flights 
Flights <- Flights[air_time>0,]
# Check unique Paths for each carrier 
# Add Path 
Flights[ , path:= paste(origin, dest, sep = "_")]
Flights[ , uniqueN(path), by = .(carrier)][ order(-V1)]
# Add Date
Flights[ ,date:= date(time_hour)]
# Categorization for departure delay  if delay >15 % of airtime 
Flights[ , dep_type := if_else(dep_delay <=.15* air_time , "on_time", "delayed" )]
# Number of  trips for each carrier and path 
Flights[ , .N, by = .(carrier, path)][order(-N)]


# Most frequent flights 
Flights[ , uniqueN(c(path, date)), by = .(path ,carrier,flight)][ order(-V1)]

# Most delayed flights , paths, carrier, date 
Flights[dep_type =="delayed" ,.N, by = .(path, carrier) ][order(-N)]
Flights[dep_type =="delayed" ,.N, by = .(date) ][order(-N)] # Most day for delay 

# Check for maximum Departure delay 
Flights[ , .SD[which.max(dep_delay)], by = .(path), .SDcols = c("flight",  "carrier", "date","dep_delay", "manufacturer", "model_year")]
# Flights showing departure delay but arrived early or ontime 
Flights[dep_type =="delayed" & arr_delay <0,.N , by = .(path, carrier)]

Flights[ , .SD[which.max(model_year)], by = .(flight), .SDcols = c("path", "manufacturer", "model_year")] # Last model year per flight

# Some plots for more exploration of data 
ggplot(Flights, aes(carrier, fill = dep_type))+ geom_bar( position = "fill")
ggplot(Flights, aes(dep_delay, arr_delay, color = origin))+geom_jitter()+facet_trelliscope(~carrier)

# bar chart for delay status per manufacturer
Flights[ , .N, by = .( carrier, dep_type)] %>% ggplot(aes(carrier, N, fill = dep_type))+ geom_col(position = "fill") +labs(title = "Delay status per  carrier")+ theme_test()

ggplot(Flights, aes(factor(month), fill = dep_type))+ geom_bar(position = "dodge")+ labs(title = "Departure delay status per month", x="Month")+ theme_test()

