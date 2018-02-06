library(tidyverse)
library(lubridate)
library(rnoaa)
library(dataRetrieval)

# lower sacramento river at freeport 1961-10-01 	 2017-12-18
freeport_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11447650', parameterCd = '00010',
                                                 startDate = '1979-01-01', endDate = '1999-12-31',
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
  summarise(meanish = mean(mean_min_max, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  ggplot(aes(x = date, y = meanish)) +
  geom_col()

# imputation
library(forecast)
?na.interp

free <- freeport_water_temp %>%
  select(date = Date, max_t = X_.Right.Bank.Pump.Stand._00010_00001,
         min_t = X_.Right.Bank.Pump.Stand._00010_00002) %>%
  mutate(mean_min_max = (max_t + min_t) / 2) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(mean_water_temp_c = mean(mean_min_max, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  ungroup() %>%
  select(date, mean_water_temp_c) %>%
  bind_rows(
    tibble(date = seq.Date(ymd('1979-01-01'), ymd('1999-12-01'), by = 'month'),
           mean_water_temp_c = 0)
  ) %>%
  group_by(date) %>%
  summarise(mean_water_temp_c = max(mean_water_temp_c)) %>%
  ungroup() %>%
  mutate(mean_water_temp_c = ifelse(mean_water_temp_c == 0, NA, mean_water_temp_c))


ts_freeport <- ts(free$mean_water_temp_c, start = c(1979, 1), end = c(1999, 12), frequency = 12)

na.interp(ts_freeport) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_freeport, series = 'Original')

lower_sac_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('1999-12-01'), by = 'month'),
  watershed = 'Lower Sacramento River',
  monthly_mean_temp_c = as.numeric(na.interp(ts_freeport)))


lower_sac_water_temp_c %>%
  ggplot(aes(x = date, y = monthly_mean_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = free, aes(x = date, y = mean_water_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean water temperature (°C)')

write_rds(lower_sac_water_temp_c, 'data-raw/lower_sacramento/lower_sac_water_temp_c.rds')
