library(tidyverse)
library(lubridate)
library(rnoaa)
library(dataRetrieval)

# lower sacramento river at freeport 1961-10-01 	 2017-12-18
freeport_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11447650', parameterCd = '00010',
                                                 startDate = '1980-01-01', endDate = '1999-12-31',
                                                 statCd = c('00001', '00002', '00008'))
glimpse(freeport_water_temp)



freeport_water_temp %>%
  select(date = Date, max = X_.Right.Bank.Pump.Stand._00010_00001,
         min = X_.Right.Bank.Pump.Stand._00010_00002) %>%
  gather(stat, temp_c, -date) %>%
  ggplot(aes(x = date, y = temp_c, fill = stat)) +
  geom_col(position = 'dodge')

# 9 months missing
freeport_water_temp %>%
  select(date = Date, max_t = X_.Right.Bank.Pump.Stand._00010_00001,
         min_t = X_.Right.Bank.Pump.Stand._00010_00002) %>%
  mutate(mean_min_max = (max_t + min_t) / 2) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(meanish = mean(mean_min_max, na.rm = TRUE)) %>% glimpse()
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  ggplot(aes(x = date, y = meanish)) +
  geom_col()
