library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(CDECRetrieve)
library(forcats)
library(rnoaa)
library(caret)

# hourly water temperature data on butte creek near durham (fahrenheit)
# butte_nr_durham <- CDECRetrieve::cdec_query(stations = 'BCD', sensor_num = '25', dur_code = 'H',
#                                            start_date = '1980-01-01', end_date = '1999-12-31')
# write_rds(butte_nr_durham, 'data-raw/butte_creek/butte_nr_durham.rds')
# data only available 1998-1999, weird website says more data available

# hourly water temperature data on butte creek near chico (fahrenheit)
# butte_nr_chico <- CDECRetrieve::cdec_query(stations = 'BCK', sensor_num = '25', dur_code = 'H',
                                           # start_date = '1998-09-16', end_date = '2017-09-30')

bcd <- CDECRetrieve::cdec_query(stations = 'BCD', sensor_num = '25', dur_code = 'H',
                                           start_date = '1998-09-16', end_date = '2017-09-30')

# write_rds(butte_nr_chico, 'data-raw/butte_creek/butte_nr_chico.rds')

# saved result from above query
butte_nr_durham <- read_rds('data-raw/butte_creek/butte_nr_durham.rds')
butte_nr_chico <- read_rds('data-raw/butte_creek/butte_nr_chico.rds')

# bck = chico, bcd = durham
butte_nr_chico %>%
  bind_rows(butte_nr_durham) %>%
  mutate(date = as_date(datetime)) %>%
  group_by(location_id, date) %>%
  summarise(mean_water_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
  filter(year(date) %in% c(1998, 1999)) %>%
  ggplot(aes(x = date, y = mean_water_temp_c, color = location_id)) +
  geom_line() +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

butte_nr_chico %>%
  bind_rows(bcd) %>%
  mutate(year = year(datetime), month = month(datetime)) %>%
  group_by(location_id, year, month) %>%
  summarise(mean_water_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
  ggplot(aes(x = date, y = mean_water_temp_c, fill = location_id)) +
  geom_col(position = 'dodge') +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

butte_nr_durham %>%
  select(datetime, temp_f = parameter_value) %>%
  group_by(date = as_date(datetime)) %>%
  summarise(daily_mean_temp_f = mean(temp_f, na.rm = TRUE))


# air temperature data near
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

# look at available stations near chico
chico <- rnoaa::ncdc(datasetid = 'GSOM', locationid = 'CITY:US060005', startdate = '1980-01-01',
            datatypeid = 'TAVG', enddate = '1989-12-31', token = token, limit = 1000)

chico$data %>%
  mutate(date = ymd_hms(date)) %>%
  ggplot(aes(x = date, y = value, color = station)) +
  geom_line()

# model training data 9/1998-9/2017
paradise1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', startdate = '1999-01-01',
                        datatypeid = 'TAVG', enddate = '2008-12-31', token = token, limit = 120)

paradise2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', startdate = '2009-10-01',
                         datatypeid = 'TAVG', enddate = '2016-12-31', token = token, limit = 120)

butte_air_temp <- paradise1$data %>%
  bind_rows(paradise2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, monthly_avg_air_temp_c = value)

butte_water_temp <- butte_nr_chico %>%
  mutate(year = year(datetime), month = month(datetime)) %>%
  group_by(year, month) %>%
  summarise(monthly_avg_water_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(year)) %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         monthly_avg_water_temp_c = (monthly_avg_water_temp_f - 32) * 5 / 9) %>%
  select(date, monthly_avg_water_temp_c)

butte <- butte_water_temp %>%
  left_join(butte_air_temp) %>%
  filter(!is.na(monthly_avg_air_temp_c),
         monthly_avg_water_temp_c > 0, monthly_avg_water_temp_c < 40)

butte %>%
  ggplot(aes(x = monthly_avg_air_temp_c, monthly_avg_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)


butte_model <- lm(monthly_avg_water_temp_c ~ monthly_avg_air_temp_c, data = butte)
summary(butte_model)

butte_model$coefficients
# air temp thresholds
y <- c(18, 20)
air_temp_thresholds <- (y - butte_model$coefficients[[1]]) / butte_model$coefficients[[2]]

butte %>%
  mutate(water_exceed18 = monthly_avg_air_temp_c >= air_temp_thresholds[[1]],
         water_exceed20 = monthly_avg_air_temp_c >= air_temp_thresholds[[2]]) %>%
  View()

pred <- broom::augment(butte_model) %>% pull(.fitted)
truth <- butte$monthly_avg_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)
confusionMatrix(xtab)

# chico univ farm, between chico and durham, GHCND:USC00041715, too much missing data
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
  select(date, month, monthly_avg_air_temp_c = value) %>%
  ggplot(aes(x = month, y = monthly_avg_air_temp_c)) +
  geom_boxplot() +
  geom_point(alpha = .5, pch = 1, size = 1) +
  labs(y = 'monthly average air temperature (°C)') +
  theme_minimal()

# data for setting modeled values
butte_air <- paradise3$data %>%
  bind_rows(paradise4$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, monthly_avg_air_temp_c = value)

predicted_water_temp_c <- predict(butte_model, butte_air)

butte_air$predicted_water_temp_c <- predicted_water_temp_c

years_with_missing_months <- butte_air %>%
  group_by(year = year(date)) %>%
  summarise(missing_months = 12 - n()) %>%
  filter(missing_months > 0) %>%
  pull(year)

fake_dates <- tibble(year = rep(years_with_missing_months, each = 12),
                     month = rep(1:12, times = length(years_with_missing_months)))

missing_air_temp_data <- butte_air %>%
  filter(year(date) %in% years_with_missing_months) %>%
  mutate(year = year(date), month = month(date)) %>%
  select(year, month) %>%
  bind_rows(fake_dates) %>%
  group_by(year, month) %>%
  summarise(count = n()) %>%
  filter(count < 2) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         monthly_avg_air_temp_c = NA,
         predicted_water_temp_c = NA) %>%
  select(date, monthly_avg_air_temp_c, predicted_water_temp_c)

modeled_butte_water_temp_c <- butte_air %>%
  bind_rows(missing_air_temp_data) %>%
  arrange(date)

modeled_butte_water_temp_c %>%
  ggplot(aes(x = date, y = predicted_water_temp_c)) +
  geom_col() +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

# imputation by rule of thumb: if sandwiched by exceedance, exceed
