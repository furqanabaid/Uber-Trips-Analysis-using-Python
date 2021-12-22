#importing Libraries

library(data.table)
library(plotly)
library(ggplot2)
library(ggthemes)
library(lambda.r)
library(lubridate)
library(reticulate)
library(dplyr)

#reading data
data <- read.csv("uber-raw-data-sep14.csv")
data$Date.Time <- mdy_hms(data$Date.Time)
var<-head(data,10)
var
#Uber trips according to days and hours
data$Day <- format(as.Date(data$Date.Time,format="%Y-%m-%d"), format = "%d")
ExtractedWeekdays <-weekdays(as.Date(data$Date.Time))
ExtractedWeekdays
ExtractedWeekdays[ExtractedWeekdays == 'Monday'] <- 1
ExtractedWeekdays[ExtractedWeekdays == 'Tuesday'] <- 2
ExtractedWeekdays[ExtractedWeekdays == 'Wednesday'] <- 3
ExtractedWeekdays[ExtractedWeekdays == 'Thursday'] <- 4
ExtractedWeekdays[ExtractedWeekdays == 'Friday'] <- 5
ExtractedWeekdays[ExtractedWeekdays == 'Saturday'] <- 6
ExtractedWeekdays[ExtractedWeekdays == 'Sunday'] <- 7
data$Weekday <- ExtractedWeekdays
data$Hour <- format(as.POSIXct(data$Date.Time,format="%H:%M:%S"),"%H")
data
var1<-head(data)
var1
#"Data according to the days and Density"
ggplot(data,aes(x = as.integer(Day), color = 'Density')) + geom_histogram(aes(y = ..density.. ),bins = 40,  fill = '#ba0054', alpha = 0.5) +  
  geom_density(color = '#123456') + ylab("Density") + xlab("Day")  + scale_color_manual(values = c('density' = '#000000'))
#"Data according to the hours and Density"
ggplot(data,aes(x = as.integer(Hour), color = 'density')) + geom_histogram(aes(y = ..density.. ),bins = 45,  fill = '#ba0054', alpha = 0.5) +  
  geom_density(color = '#123456') + ylab("Density") + xlab("Hour")  + theme(legend.title=element_blank()) +  scale_color_manual(values = c('density' = '#ffffff'))
#"Data according to the weekday and Density"
ggplot(data,aes(x = as.integer(Weekday), color = 'density')) + geom_histogram(aes(y = ..density.. ),bins = 30,  fill = '#ba0054', alpha = 0.5) +  
  geom_density(color = '#123456') + ylab("Density") + xlab("Weekday")  + theme(legend.title=element_blank()) +
  scale_color_manual(values = c('density' = '#ffffff'))
#correlation of hours and weekdays on the Uber trips
df<- data %>% group_by(Weekday, Hour) %>% dplyr:: summarise(Total = n())
ggplot(df, aes(Hour, Weekday, fill = Total)) + geom_tile() + scale_fill_distiller(palette = "RdPu") +
  coord_trans(y = "reverse") 
#density of Uber trips according to the regions of the New Your city
ggplot(data, aes(x=Lon, y=Lat)) + geom_point(size=1, color = "#123456") +
  scale_x_continuous(limits=c(min(data$Lon), max(data$Lon))) +
  scale_y_continuous(limits=c(min(data$Lat), max(data$Lat))) + xlab("Lat")+ ylab("Lon")+ ggtitle("Uber Trip Analysis")