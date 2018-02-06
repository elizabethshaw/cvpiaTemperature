library(tidyverse)
library(lubridate)
library(rnoaa)
library(dataRetrieval)


# cosumnes river at michigan bar ca 1965-10-01  	2016-03-03
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

cosum_wt %>%
  ggplot(aes(x = date, y = mean_water_temp_c)) +
  geom_col()

cosum_wt %>% summarise(min(date), max(date))
sac1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023271', datatypeid = 'TAVG',
                    startdate = '2001-01-01', enddate = '2010-12-31', limit = 120, token = token)

sac2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023271', datatypeid = 'TAVG',
                    startdate = '2011-01-01', enddate = '2016-12-31', limit = 120, token = token)

cosum_at <- sac1$data %>%
  bind_rows(sac2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)


cosum <- cosum_wt %>%
  left_join(cosum_at) %>%
  filter(!is.na(mean_water_temp_c))

cosum %>%
  ggplot(aes(x = mean_air_temp_c, y = mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

cosum_temp_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = cosum)
summary(cosum_temp_model)

sac3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023271', datatypeid = 'TAVG',
                    startdate = '1979-01-01', enddate = '1979-12-31', limit = 12, token = token)

sac4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023271', datatypeid = 'TAVG',
                     startdate = '1980-01-01', enddate = '1989-12-31', limit = 120, token = token)

sac5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023271', datatypeid = 'TAVG',
                     startdate = '1990-01-01', enddate = '1999-12-31', limit = 120, token = token)


sac3$data %>%
  bind_rows(sac4$data) %>%
  bind_rows(sac5$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()

sac <- sac3$data %>%
  bind_rows(sac4$data) %>%
  bind_rows(sac5$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)

cosum_predicted_water_temp <- predict(cosum_temp_model, sac)

cosumnes_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('1999-12-01'), by = 'month'),
  watershed = 'Cosumnes River',
  monthly_mean_temp_c = cosum_predicted_water_temp)

write_rds(cosumnes_water_temp_c, 'data-raw/cosumnes_river/cosumnes_water_temp_c.rds')
