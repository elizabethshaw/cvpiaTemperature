library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(rnoaa)

# usgs water temp
deer_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11383500', parameterCd = '00010',
                                           startDate = '1998-10-05', endDate = '2017-11-10',
                                           statCd = c('00001', '00002', '00008'))

# lots of missing median values from gage, use mean of min and max water temp to approximate median water temp
glimpse(deer_water_temp)
deer_water_temp %>%
  select(date = Date, temp_c_max = X_00010_00001,
         temp_c_min = X_00010_00002, temp_c_med = X_00010_00008) %>%
  filter(!is.na(temp_c_med), temp_c_med > 18) %>%
  mutate(mean_min_max = (temp_c_min + temp_c_max)/2,
         dist_mean_med = abs(mean_min_max - temp_c_med),
         dist_max_med = abs(temp_c_max - temp_c_med),
         dist_min_med = abs(temp_c_min - temp_c_med)) %>%
  select(dist_mean_med, dist_min_med, dist_max_med) %>%
  gather(dist_type, dist) %>%
  ggplot(aes(x = dist, color = dist_type)) +
  geom_density()

dt <- deer_water_temp %>%
  select(date = Date, temp_c_max = X_00010_00001,
         temp_c_min = X_00010_00002) %>%
  mutate(water_temp_c = (temp_c_min + temp_c_max)/2) %>%
  select(date, water_temp_c) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(mean_water_temp_c = mean(water_temp_c, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  select(date, mean_water_temp_c)

# find appropriate air temp
# chico <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00041715', datatypeid = 'TAVG',
#                      startdate = '1999-01-01', enddate = '2008-12-31', token = token, limit = 130)
# corning <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USR0000CCRN', datatypeid = 'TAVG',
#                       startdate = '1999-01-01', enddate = '2008-12-31', token = token, limit = 130)
#
# c1 <- chico$data %>%
#   mutate(date = as_date(ymd_hms(date))) %>%
#   select(date, chico = value)
#
# c2 <- corning$data %>%
#   mutate(date = as_date(ymd_hms(date))) %>%
#   select(date, corning = value)
#
# c3 <- c1 %>%
#   left_join(c2)
# cor(c3$chico, c3$corning, use = 'complete.obs')
# corning is close to where we need, but incomplete date coverage, chico is good
chico1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00041715', datatypeid = 'TAVG',
                      startdate = '1979-01-01', enddate = '1979-12-31', token = token, limit = 12)
chico2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00041715', datatypeid = 'TAVG',
                      startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
chico3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00041715', datatypeid = 'TAVG',
                      startdate = '1990-01-01', enddate = '1999-12-31', token = token, limit = 130)

chico1$data %>%
  bind_rows(chico2$data) %>%
  bind_rows(chico3$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()

chico_at <- chico1$data %>%
  bind_rows(chico2$data) %>%
  bind_rows(chico3$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  bind_rows(
    tibble(date = seq.Date(ymd('1979-01-01'), ymd('1999-12-01'), by = 'month'),
           mean_air_temp_c = 0)
  ) %>%
  group_by(date) %>%
  summarise(mean_air_temp_c = max(mean_air_temp_c)) %>%
  ungroup() %>%
  mutate(mean_air_temp_c = ifelse(mean_air_temp_c == 0, NA, mean_air_temp_c))

ts_chico_at <- ts(chico_at$mean_air_temp_c, start = c(1979, 1), end = c(1999, 12), frequency = 12)

na.interp(ts_chico_at) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_chico_at, series = 'Original')

deer_air_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('1999-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_chico_at)))


deer_air_temp_c %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = chico_at, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

# get data for training model
# chico3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00041715', datatypeid = 'TAVG',
#                       startdate = '1998-01-01', enddate = '2007-12-31', token = token, limit = 130)
# chico4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00041715', datatypeid = 'TAVG',
#                       startdate = '2008-01-01', enddate = '2017-11-30', token = token, limit = 130)
#
# # too much missing data, look at other chico station
# chico3$data %>%
#   bind_rows(chico4$data) %>%
#   mutate(date = as_date(ymd_hms(date))) %>%
#   select(date, mean_air_temp_c = value) %>%
#   ggplot(aes(x = date, y = mean_air_temp_c)) +
#   geom_col()
#
# chico5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USR0000CCHC', datatypeid = 'TAVG',
#                       startdate = '1995-01-01', enddate = '1998-12-31', token = token, limit = 130)
# chico6 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00041715', datatypeid = 'TAVG',
#                       startdate = '1995-01-01', enddate = '1998-12-31', token = token, limit = 130)
#
# # substituting other chico station is reasonable
# chico5$data %>%
#   bind_rows(chico6$data) %>%
#   mutate(date = as_date(ymd_hms(date))) %>%
#   select(date, mean_air_temp_c = value, station) %>%
#   ggplot(aes(x = date, y = mean_air_temp_c, fill = station)) +
#   geom_col(position = 'dodge') +
#   geom_hline(yintercept = 18) +
#   geom_hline(yintercept = 20)
#
chico4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USR0000CCHC', datatypeid = 'TAVG',
                      startdate = '1998-01-01', enddate = '2007-12-31', token = token, limit = 130)
chico5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USR0000CCHC', datatypeid = 'TAVG',
                      startdate = '2008-01-01', enddate = '2017-11-30', token = token, limit = 130)

chico4$data %>%
  bind_rows(chico5$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()

chico_train_at <- chico4$data %>%
  bind_rows(chico5$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  bind_rows(
    tibble(date = seq.Date(ymd('1998-01-01'), ymd('2014-03-01'), by = 'month'),
           mean_air_temp_c = 0)
  ) %>%
  group_by(date) %>%
  summarise(mean_air_temp_c = max(mean_air_temp_c)) %>%
  ungroup() %>%
  mutate(mean_air_temp_c = ifelse(mean_air_temp_c == 0, NA, mean_air_temp_c))

ts_chico_tat <- ts(chico_train_at$mean_air_temp_c, start = c(1998, 1), end = c(2014, 3), frequency = 12)

na.interp(ts_chico_tat) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_chico_tat, series = 'Original')

deer_air_temp <- tibble(
  date = seq.Date(ymd('1998-01-01'), ymd('2014-03-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_chico_tat)))

deer <- dt %>%
  left_join(deer_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

deer_water_temp_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = deer)
summary(deer_water_temp_model)

deer_predicted_water_temp <- predict(deer_water_temp_model, deer_air_temp_c)

deer_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('1999-12-01'), by = 'month'),
  watershed = 'Deer Creek',
  monthly_mean_temp_c = deer_predicted_water_temp)

deer_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = monthly_mean_temp_c)) +
  geom_hline(yintercept = 18) +
  geom_hline(yintercept = 20) +
  theme_minimal()

write_rds(deer_water_temp_c, 'data-raw/deer_creek/deer_creek_water_temp_c.rds')
