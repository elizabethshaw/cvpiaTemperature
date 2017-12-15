library(tidyverse)
library(lubridate)
library(rnoaa)
library(CDECRetrieve)

# big chico BIC cdec query 12/14/2017
# bic <- cdec_query(stations = 'BIC', sensor_num = '25', dur_code = 'H',
#            start_date = '1998-09-10', end_date = '2014-12-01')
#
# write_rds(bic, 'data-raw/big_chico_creek/bic.rds')

bic <- read_rds('data-raw/big_chico_creek/bic.rds')

# really high temps 2002-12, 2003-01, value 708 messing with average
bic %>%
  # mutate(date = as_date(datetime)) %>%
  # filter(between(date, ymd('2002-12-01'), ymd('2003-01-31')),
  #        parameter_value < 110) %>%
  filter(parameter_value < 100) %>%
  mutate(year = year(datetime), month = month(datetime)) %>%
  group_by(year, month) %>%
  summarise(mean_water_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
  ggplot(aes(x = date, y = mean_water_temp_c)) +
  geom_col(position = 'dodge') +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

big_chico_water_temp <- bic %>%
  filter(parameter_value < 100) %>%
  mutate(year = year(datetime), month = month(datetime)) %>%
  group_by(year, month) %>%
  summarise(mean_water_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_water_temp_c = (mean_water_temp_f - 32) * 5 / 9) %>%
  filter(!is.na(date)) %>%
  select(date, mean_water_temp_c)

big_chico_water_temp %>%
  summarise(min = min(date), max = max(date), max-min)


# air temperature data near
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

chico <- rnoaa::ncdc(datasetid = 'GSOM', locationid = 'CITY:US060005', datatypeid = 'TAVG',
                     startdate = '1998-01-01', enddate = '2007-12-31', token = token, limit = 1000)

chico$data %>%
  group_by(station) %>%
  summarise(n())

# 2 GHCND:USC00042402   119
# 3 GHCND:USC00046685   117
# 4 GHCND:USR0000CCHC   119
# 5 GHCND:USR0000CCOH   120

# cohasset (USR0000CCOH) best location but doesn't have data 1980-1999, look at next best station paradise (USC00046685)
paradise1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', datatypeid = 'TAVG',
                         startdate = '1998-01-01', enddate = '2007-12-31', token = token, limit = 130)

paradise2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', datatypeid = 'TAVG',
                         startdate = '2008-01-01', enddate = '2014-12-31', token = token, limit = 130)

big_chico_air_temp <- paradise1$data %>%
  bind_rows(paradise2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)

big_chico <- big_chico_water_temp %>%
  left_join(big_chico_air_temp) %>%
  filter(!is.na(mean_air_temp_c))

big_chico %>%
  ggplot(aes(x = mean_air_temp_c, y = mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

big_chico_temp_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = big_chico)
summary(big_chico_temp_model)

big_chico_temp_model$coefficients
# air temp thresholds
y <- c(18, 20)
air_temp_thresholds <- (y - big_chico_temp_model$coefficients[[1]]) / big_chico_temp_model$coefficients[[2]]

big_chico %>%
  ggplot(aes(x = mean_air_temp_c, y = mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_vline(xintercept = air_temp_thresholds[1], alpha = .2) +
  geom_vline(xintercept = air_temp_thresholds[2], alpha = .2) +
  geom_hline(yintercept = 18) +
  geom_hline(yintercept = 20)

pred <- broom::augment(big_chico_temp_model) %>% pull(.fitted)
truth <- big_chico$mean_water_temp_c
xtab <- table(pred > 18, truth > 18)
xtab <- table(pred > 20, truth > 20)
confusionMatrix(xtab)

paradise3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', startdate = '1980-01-01',
                         datatypeid = 'TAVG', enddate = '1989-12-31', token = token, limit = 120)

paradise4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', startdate = '1990-01-01',
                         datatypeid = 'TAVG', enddate = '1999-12-31', token = token, limit = 120)

paradise_air_temp <- paradise3$data %>%
  bind_rows(paradise4$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)

predicted_water_temp_c <- predict(big_chico_temp_model, paradise_air_temp)

years_with_missing_months <- paradise_air_temp %>%
  group_by(year = year(date)) %>%
  summarise(missing_months = 12 - n()) %>%
  filter(missing_months > 0) %>%
  pull(year)

fake_dates <- tibble(year = rep(years_with_missing_months, each = 12),
                     month = rep(1:12, times = length(years_with_missing_months)))

missing_air_temp_data <- paradise_air_temp %>%
  filter(year(date) %in% years_with_missing_months) %>%
  mutate(year = year(date), month = month(date)) %>%
  select(year, month) %>%
  bind_rows(fake_dates) %>%
  group_by(year, month) %>%
  summarise(count = n()) %>%
  filter(count < 2) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-')),
         mean_air_temp_c = NA,
         predicted_water_temp_c = NA) %>%
  select(date, mean_air_temp_c, predicted_water_temp_c)

modeled_big_chico_water_temp_c <- paradise_air_temp %>%
  mutate(predicted_water_temp_c = predicted_water_temp_c) %>%
  bind_rows(missing_air_temp_data) %>%
  arrange(date)

modeled_big_chico_water_temp_c %>%
  mutate(exceed = case_when(
    predicted_water_temp_c > 20 ~ 'over 20',
    predicted_water_temp_c > 18 ~ 'over 18',
    TRUE ~ 'none'
  )) %>%
  filter(month(date) < 9, is.na(predicted_water_temp_c)) %>% glimpse()
  ggplot(aes(x = date, y = predicted_water_temp_c, fill = exceed)) +
  geom_col() +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3) +
  scale_fill_manual(values = c('#969696', '#feb24c', '#bd0026')) +
  theme_minimal()
