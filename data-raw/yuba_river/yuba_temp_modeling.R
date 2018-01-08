library(tidyverse)
library(lubridate)
library(rnoaa)
library(dataRetrieval)
library(forecast)

# USGS water temperature yuba river near marysville ca  1964-10-01 	 2003-09-29---------------------
yuba_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11421000', parameterCd = '00010',
                                              startDate = '1980-01-01', endDate = '1999-12-31',
                                              statCd = c('00001', '00002'))
glimpse(yuba_water_temp) # incomplete record during period of interest

yuba_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11421000', parameterCd = '00010',
                                             startDate = '1964-10-01', endDate = '2003-09-29',
                                             statCd = c('00001', '00002'))

yuba_water_temp %>% summarise(min(Date), max(Date))

yuba_water <- yuba_water_temp %>%
  filter(year(Date) >= 1989) %>%
  select(date = Date, max = X_00010_00001, min = X_00010_00002) %>%
  mutate(mean_min_max = (max + min) / 2) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(mean_water_temp_c = mean(mean_min_max, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  ungroup() %>%
  select(date, mean_water_temp_c)

yuba_wt <- yuba_water_temp %>%
  select(date = Date, max = X_00010_00001, min = X_00010_00002) %>%
  mutate(mean_min_max = (max + min) / 2) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(mean_water_temp_c = mean(mean_min_max, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  ungroup() %>%
  select(date, mean_water_temp_c) %>%
  bind_rows(
    tibble(date = seq.Date(ymd('1964-10-01'), ymd('2003-09-01'), by = 'month'),
           mean_water_temp_c = 0)
  ) %>%
  group_by(date) %>%
  summarise(mean_water_temp_c = max(mean_water_temp_c)) %>%
  ungroup() %>%
  mutate(mean_water_temp_c = ifelse(mean_water_temp_c == 0, NA, mean_water_temp_c))

yuba_wt %>% summarise(min(date), max(date))
yuba_wt %>%
  ggplot(aes(x = date, y = mean_water_temp_c)) +
  geom_col()

ts_yuba <- ts(yuba_wt$mean_water_temp_c, start = c(1964, 10), end = c(2003, 9), frequency = 12)

na.interp(ts_yuba) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_yuba, series = 'Original')
# imputation not good enough use water_temp ~ air_temp for 1980-1990

# 1990 and onward most complete continuous record

# GHCND:USC00045385
yuba1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
                          startdate = '1989-01-01', enddate = '1998-12-31', token = token, limit = 130)
yuba2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
                     startdate = '1999-01-01', enddate = '2003-12-31', token = token, limit = 130)

yuba_air_temp <- yuba1$data %>%
  bind_rows(yuba2$data) %>%
  select(date, mean_air_temp_c = value) %>%
  mutate(date = as_date(ymd_hms(date)))

yuba_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()

yuba <- yuba_water %>%
  left_join(yuba_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

yuba %>%
  ggplot(aes(x = mean_air_temp_c, y = mean_water_temp_c)) +
  geom_point()

yuba_temp_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = yuba)
summary(yuba_temp_model)

yuba3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
                     startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
yuba4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
                     startdate = '1990-01-01', enddate = '1999-12-31', token = token, limit = 130)


yuba_at <- yuba3$data %>%
  bind_rows(yuba4$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  bind_rows(
    tibble(date = seq.Date(ymd('1980-01-01'), ymd('1999-12-01'), by = 'month'),
           mean_air_temp_c = 0)
  ) %>%
  group_by(date) %>%
  summarise(mean_air_temp_c = max(mean_air_temp_c)) %>%
  ungroup() %>%
  mutate(mean_air_temp_c = ifelse(mean_air_temp_c == 0, NA, mean_air_temp_c))


ts_yuba_at <- ts(yuba_at$mean_air_temp_c, start = c(1980, 1), end = c(1999, 12), frequency = 12)

na.interp(ts_yuba_at) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_yuba_at, series = 'Original')

yuba_air_temp_c <- tibble(
  date = seq.Date(ymd('1980-01-01'), ymd('1999-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_yuba_at)))


yuba_air_temp_c %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = yuba_at, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

yuba_pred_water_temp <- predict(yuba_temp_model, yuba_air_temp_c)

yuba_water_temp_c <- tibble(
  date = seq.Date(ymd('1980-01-01'), ymd('1999-12-01'), by = 'month'),
  `Yuba River` = yuba_pred_water_temp)

yuba_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = `Yuba River`), alpha = .4, fill = 'red') +
  geom_col(data = yuba_water, aes(y = mean_water_temp_c), alpha = .4, fill = 'blue') +
  geom_hline(yintercept = 18) +
  geom_hline(yintercept = 20) +
  theme_minimal()



write_rds(yuba_water_temp_c, 'data-raw/yuba_river/yuba_river_water_temp_c.rds')

