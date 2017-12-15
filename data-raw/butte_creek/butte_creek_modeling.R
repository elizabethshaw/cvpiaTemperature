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
# paradise1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', startdate = '1998-01-01',
#                         datatypeid = 'TAVG', enddate = '2007-12-31', token = token, limit = 120)
#
# paradise2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00046685', startdate = '2008-01-01',
#                          datatypeid = 'TAVG', enddate = '2017-10-31', token = token, limit = 120)
#
# write_rds(paradise1, 'data-raw/butte_creek/paradise1.rds')
# write_rds(paradise2, 'data-raw/butte_creek/paradise2.rds')

paradise1 <- read_rds('data-raw/butte_creek/paradise1.rds')
paradise2 <- read_rds('data-raw/butte_creek/paradise2.rds')

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

# data for setting modeled values
butte_air <- paradise3$data %>%
  bind_rows(paradise4$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)

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
         mean_air_temp_c = NA,
         predicted_water_temp_c = NA) %>%
  select(date, mean_air_temp_c, predicted_water_temp_c)

modeled_butte_water_temp_c <- butte_air %>%
  bind_rows(missing_air_temp_data) %>%
  arrange(date)

modeled_butte_water_temp_c %>%
  ggplot(aes(x = date, y = predicted_water_temp_c)) +
  geom_col() +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3)

View(modeled_butte_water_temp_c)
# imputation by eye: if sandwiched by exceedance, exceed
miss <- modeled_butte_water_temp_c %>%
  filter(is.na(predicted_water_temp_c)) %>%
  mutate(predicted_water_temp_c = c(9, 21, 19, 9, 7, 19, 21))

mod_butte_water_temp_c <- modeled_butte_water_temp_c %>%
  filter(!is.na(predicted_water_temp_c)) %>%
  bind_rows(miss) %>%
  arrange(date)

mod_butte_water_temp_c %>%
  mutate(exceed = case_when(
    predicted_water_temp_c > 20 ~ 'over 20',
    predicted_water_temp_c > 18 ~ 'over 18',
    TRUE ~ 'none'
  )) %>%
  ggplot(aes(x = date, y = predicted_water_temp_c, fill = exceed)) +
  geom_col() +
  geom_hline(yintercept = 18, alpha = .3) +
  geom_hline(yintercept = 20, alpha = .3) +
  scale_fill_manual(values = c('#969696', '#feb24c', '#bd0026')) +
  theme_minimal()
