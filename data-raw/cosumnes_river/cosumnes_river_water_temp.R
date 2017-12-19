library(tidyverse)
library(lubridate)
library(rnoaa)
library(dataRetrieval)


# cosumnes river at michigan bar ca 1965-10-01  	2016-03-03
cosum_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11335000', parameterCd = '00010',
                                                 startDate = '1980-01-01', endDate = '1999-12-31',
                                                 statCd = c('00001', '00002', '00008'))
# *** There are no data available on the Waterdata system for the time period specified,
# although data may be available in the files of the local USGS office operating the station.

glimpse(cosum_water_temp)


cosum_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11335000', parameterCd = '00010',
                                              startDate = '2000-01-01', endDate = '2016-03-03',
                                              statCd = c('00001', '00002', '00008'))

cosum_water_temp %>%
  select(date = Date, max = X_00010_00001,
         min = X_00010_00002, med = X_00010_00008) %>%
  filter(!is.na(med)) %>%
  gather(stat, temp_c, -date) %>%
  group_by(year = year(date), month = month(date), stat) %>%
  summarise(mean_temp_c = mean(temp_c)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  select(date, stat, mean_temp_c) %>%
  spread(stat, mean_temp_c) %>%
  mutate(mean_max_min = (max + min)/2,
         dist_min = abs(min - med),
         dist_max = abs(max - med),
         dist_mean_max_min = abs(mean_max_min - med)) %>%
  select(date, dist_min:dist_mean_max_min) %>%
  gather(category, distance, -date) %>%
  ggplot(aes(x = distance, color = category)) +
  geom_density()

cosum_wt <- cosum_water_temp %>%
  select(date = Date, max = X_00010_00001,
         min = X_00010_00002, med = X_00010_00008) %>%
  gather(stat, temp_c, -date) %>%
  group_by(year = year(date), month = month(date), stat) %>%
  summarise(mean_temp_c = mean(temp_c)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  select(date, stat, mean_temp_c) %>%
  spread(stat, mean_temp_c) %>%
  mutate(mean_max_min = (max + min)/2) %>%
  select(date, mean_water_temp_c = mean_max_min)

cosum_wt %>% summarise(min(date), max(date))
# GHCND:USW00023232
# GHCND:USW00023271 probs better
# GHCND:USC00043038 best?
# https://www.ncdc.noaa.gov/cdo-web/results
