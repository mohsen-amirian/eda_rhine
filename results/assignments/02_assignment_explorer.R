#1
#185,000 km^2

#Q2
catchment_area <- 185000*1000*1000 #m^2
precipitation_hour <- 0.5 #mm/h
precipitation_day <- precipitation_hour * 24 / 1000 #m/day
catchment_water <- precipitation_day * catchment_area #m^3/day
average_runoff <- catchment_water / (24 * 60 * 60)
average_runoff
#25694.44 m^3/s

#Q4
#a) 
#increase in greenhouse gases --> increase in global average temperature
#--> affect the hydrological cycle --> changes in precipitation and water cycle
