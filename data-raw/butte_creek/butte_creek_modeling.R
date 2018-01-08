library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(CDECRetrieve)
library(forcats)
library(rnoaa)
library(caret)

# hourly water temperature data on butte creek near durham (fahrenheit)
#07/29/1998 to present
bcd <- CDECRetrieve::cdec_query(stations = 'BCD', sensor_num = '25', dur_code = 'H',
                                           start_date = '1998-09-16', end_date = '2017-09-30')

# bcd = durham
butte_water_temp <- bcd %>%
  select(datetime, temp_f = parameter_value) %>%
  filter(between(temp_f, 10, 100)) %>%
  group_by(year = year(datetime), month = month(datetime)) %>%
  summarise(mean_water_temp_f = mean(temp_f, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
  select(date, mean_water_temp_c) %>%
  filter(!is.na(date))

# air temperature data near stream
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

# model training data 9/1998-9/2017
paradise1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', startdate = '1998-01-01',
                        datatypeid = 'TAVG', enddate = '2007-12-31', token = token, limit = 120)

paradise2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', startdate = '2008-01-01',
                         datatypeid = 'TAVG', enddate = '2017-10-31', token = token, limit = 120)


butte_air_temp <- paradise1$data %>%
  bind_rows(paradise2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)


butte <- butte_water_temp %>%
  left_join(butte_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

butte %>%
  ggplot(aes(x = mean_air_temp_c, mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

butte_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = butte)
summary(butte_model)

butte_model$coefficients
# air temp thresholds
y <- c(18, 20)
air_temp_thresholds <- (y - butte_model$coefficients[[1]]) / butte_model$coefficients[[2]]

pred <- broom::augment(butte_model) %>% pull(.fitted)
truth <- butte$mean_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)
confusionMatrix(xtab)

# chico univ farm, between chico and durham, GHCND:USC00041715, perfect but too much missing data
# de sabla, GHCND:USC00042402 too north east
# paradise GHCND:USC00046685, most appropriate for butte creek rearing extent
paradise3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', startdate = '1980-01-01',
                        datatypeid = 'TAVG', enddate = '1989-12-31', token = token, limit = 120)

paradise4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', startdate = '1990-01-01',
                        datatypeid = 'TAVG', enddate = '1999-12-31', token = token, limit = 120)
paradise3$data %>%
  bind_rows(paradise4$data) %>%
  mutate(date = ymd_hms(date), year = year(date),
         month = factor(month.abb[month(date)],
                        levels = c(month.abb[10:12], month.abb[1:9]), ordered = TRUE)) %>%
  select(date, month, mean_air_temp_c = value) %>%
  ggplot(aes(x = month, y = mean_air_temp_c)) +
  geom_boxplot() +
  geom_point(alpha = .5, pch = 1, size = 1) +
  labs(y = 'monthly average air temperature (°C)') +
  theme_minimal()

paradise_air_temp <- paradise3$data %>%
  bind_rows(paradise4$data) %>%
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


ts_paradise <- ts(paradise_air_temp$mean_air_temp_c, start = c(1980, 1), end = c(1999, 12), frequency = 12)

na.interp(ts_paradise) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_paradise, series = 'Original')

butte_air_temp_c <- tibble(
  date = seq.Date(ymd('1980-01-01'), ymd('1999-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_paradise)))


paradise_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = butte_air_temp_c, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

butte_pred_water_temp <- predict(butte_model, butte_air_temp_c)

butte_water_temp_c <- tibble(
  date = seq.Date(ymd('1980-01-01'), ymd('1999-12-01'), by = 'month'),
  `Butte Creek` = butte_pred_water_temp)

butte_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = `Butte Creek`))

write_rds(butte_water_temp_c, 'data-raw/butte_creek/butte_creek_water_temp_c.rds')
