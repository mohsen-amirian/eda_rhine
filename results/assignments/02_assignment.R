#clean code

tempratures = c(3,6,10,14)
weights = c(1,0.8,1.2,1)

library(data.table)

multiply = function(first_number, second_number){
  first_number * second_number
}
results <- multiply(tempratures, weights)


#/////////////////////////////////////////
#/////////////////////////////////////////


runoff_ts <- data.frame(time = as.Date(1:90, origin = '2020/12/31'), 
                        value = sample(c(130, 135, 140), 
                                       size = 90, replace = T))
head(runoff_ts)

library(data.table)
runoff_dt <- data.table(runoff_ts)
runoff_dt[, mon := month(time)]
runoff_dt[, mon_mean := mean(value), by = mon]
runoff_month <- runoff_dt[, .(mon, mon_mean)] 
runoff_month
unique_runoff <- unique(runoff_month)


runoff_percentage_change = function(first_month_avg, second_month_avg){
  raw_difference <- second_month_avg - first_month_avg
  final_percentage <- (raw_difference/10)*100
}



first_month_percentage <- runoff_percentage_change(unique_runoff[1, mon_mean], unique_runoff[2, mon_mean])


second_month_percentage <- runoff_percentage_change(unique_runoff[2, mon_mean], unique_runoff[3, mon_mean])

unique_runoff[, differential_percentage := c(0, first_month_percentage, second_month_percentage)]
unique_runoff
