library(data.table)
library(ggplot2)
runoff_day <- readRDS('data/runoff_day.rds')
runoff_summer <- readRDS('data/runoff_summer.rds')
runoff_year_key <- readRDS('data/runoff_year_key.rds')
runoff_month_key <- readRDS('data/runoff_month_key.rds')
runoff_summary <- readRDS('data/runoff_summary.rds')
runoff_winter <- readRDS('data/runoff_winter.rds')

colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

key_stations <- c('DOMA', 'BASR', 'KOEL')

#QUESTION1

####monthly data
runoff_month_key[year <= 2000, age_range := factor('beforeyear_2000')]
runoff_month_key[year > 2000, age_range := factor('afteryear_2000')]
runoff_month_key
ggplot(runoff_month_key, aes(factor(month), value, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Month") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
#there are more changes between the age range during the winter months which are 12, 1 and 2

####yearly data
runoff_year_key[year <= 2000, age_range := factor('beforeyear_2000')]
runoff_year_key[year > 2000, age_range := factor('afteryear_2000')]
ggplot(runoff_year_key, aes(age_range, value, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Age Range") +
  ylab(label = "Runoff (m3/s)") +
  theme_bw()
#more changes are noticed in the station DOMA between the age ranges, the other stations are different but not a lot

#QUESTION 2
runoff_day_key <- runoff_day[sname %in% key_stations]
year_thres <- 1990
runoff_day_key[year <= year_thres, age_range := factor('beforeyear_1990')]
runoff_day_key[year > year_thres, age_range := factor('afteryear_1990')]

runoff_day_key[, qu_01 := quantile(value, 0.1), by = .(sname, month)]
runoff_day_key[, qu_09 := quantile(value, 0.9), by = .(sname, month)]
#high runoff is the daily runoff above 0.9 quantile and low runoff below 0.1 quantile
runoff_day_key[, runoff_class := factor('medium')]
runoff_day_key[value <= qu_01, runoff_class := factor('low')]
runoff_day_key[value >= qu_09, runoff_class := factor('high')]
runoff_day_key[, days := .N, .(sname, year, runoff_class, season)]
runoff_day_key

runoff_day_key_class <- unique(
  runoff_day_key[, .(sname, days, year, age_range, season, runoff_class)])
#we use it to eliminate if there are duplicate values

ggplot(runoff_day_key_class[season == 'winter' | season == 'summer'], 
       aes(season, days, fill = age_range)) +
  geom_boxplot() +
  facet_wrap(runoff_class~sname, scales = 'free_y') +
  scale_fill_manual(values = colset_4[c(4, 1)]) +
  xlab(label = "Season") +
  ylab(label = "Days") +
  theme_bw()
#there are more outliers before the year 1990 in the summer season

#QUESTION 3
#1950-today regression plots and use data until 2010
#first we are going to use linear r
runoff_winter[, value_norm := scale(value), sname]
runoff_summer[, value_norm := scale(value), sname]
n_stations <- nrow(runoff_summary)
#winter
ggplot(runoff_winter[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (z-score)") +
  theme_bw()
#summer
ggplot(runoff_summer[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'lm', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (z-score)") +
  theme_bw()
#both graphs show a continuous increase in winter and decrease in summer, it looks like some information might be lost

#loess
#summer
ggplot(runoff_summer[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Summer runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (z-score)") +
  theme_bw()
#winter
ggplot(runoff_winter[year > 1950 & year <= 2010], aes(x = year, y = value_norm, col = sname)) +
  geom_smooth(method = 'loess', formula = y~x, se = 0) + 
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  ggtitle('Winter runoff') +
  xlab(label = "Year") +
  ylab(label = "Runoff (z-score)") +
  theme_bw()
#both graphs are more accurate than the ones using linear r. in the summer graph it kind of looks like the runoff stabilizes after 2010 and there isn't a big decrease except in 2 stations from the 1950
#in the winter it looks like the highest runoff was around 1990 and in the summer around 1950 and 1980