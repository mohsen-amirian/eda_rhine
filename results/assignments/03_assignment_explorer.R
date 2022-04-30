#QUESTION1
#area km^2 and runoff m^3/s

#QUESTION2
library(data.table)
average_runoff <- mean(runoff_day$value)
average_runoff 
#1372.793 m^3/s

average_area <- mean(catchment_area$area)
average_area 
#68769 km^2

#QUESTION3
library('ggplot2')
runoff_avg <- runoff_day[, mean(value), by = sname]
runoff_avg
ggplot(data = runoff_avg, aes(x = sname, y = V1)) +
  geom_bar(stat = 'Identity') +
  ylab('Mean Runoff')

#QUESTION4
cor(runoff_stations$altitude, runoff_stations$area)
#-0.8611829 the negative correlation between when one increases
#the other one decreases. 