#QUESTION1
#the 50th percentile is the median, they are the same

#QUESTION2
# the data is not normally distributed when the median and the mean are not the same 
#and it is skewed to the left or to the right


#QUESTION4
library(data.table)
library(ggplot2)
readRDS('./data/runoff_summary.rds')
runoff_years <- readRDS('./data/runoff_year.rds')
runoff_stats <- readRDS('./data/runoff_stats.rds')
runoff_months <- readRDS('./data/runoff_month.rds')
runoff_day <- readRDS('./data/runoff_day.rds')

#months
runoff_months[, min := min(value), by = 'sname']
runoff_months[, max := max(value), by = 'sname']

runoff_months_max <- runoff_months[runoff_months$value == runoff_months$max] 
runoff_months_min <- runoff_months[runoff_months$value == runoff_months$min] 
#max
ggplot(runoff_months_max, aes(x = sname, y = max, label = month)) +
  geom_point() +
  geom_text(vjust = -0.3) 
#min
ggplot(runoff_months_min, aes(x = sname, y = min, label = month)) +
  geom_point() +
  geom_text(vjust = -0.3)

#years
runoff_years[, min := min(value), by = 'sname']
runoff_years[, max := max(value), by = 'sname']

runoff_years_max <- runoff_years[runoff_years$value == runoff_years$max] 
runoff_years_min <- runoff_years[runoff_years$value == runoff_years$min] 
#max
ggplot(runoff_years_max, aes(x = sname, y = max, label = year)) +
  geom_point() +
  geom_text(vjust = -0.3) 
#min
ggplot(runoff_years_min, aes(x = sname, y = min, label = year)) +
  geom_point() +
  geom_text(vjust = -0.3) 