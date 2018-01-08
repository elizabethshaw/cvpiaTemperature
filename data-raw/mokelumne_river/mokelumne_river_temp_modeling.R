library(rnoaa)
library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(broom)

token <- Sys.getenv("token")

moke_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11325500', parameterCd = '00010',
                                             statCd = c('00001', '00002'),
                                             startDate = '1966-01-01', endDate = '1975-12-31')

glimpse(moke_water_temp)
moke_water_temp %>%
  select(date = Date, max = X_00010_00001, min = X_00010_00002) %>%
  ggplot(aes(x = date, y = max)) +
  geom_col()

moke_wt <- moke_water_temp %>%
  select(date = Date, max = X_00010_00001, min = X_00010_00002) %>%
  mutate(water_temp_c = (max + min) / 2) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(mean_water_temp_c = mean(water_temp_c, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  select(date, mean_water_temp_c)

moke_wt %>% summarise(min(date), max(date))

# temp at lodi
lodi1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
                     startdate = '1966-01-01', enddate = '1975-12-31', limit = 120, token = token)

moke_at <- lodi1$data %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)

moke <- moke_wt %>%
  left_join(moke_at)

moke %>%
  ggplot(aes(x = mean_air_temp_c, y = mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

cor(x = moke$mean_air_temp_c, y = moke$mean_water_temp_c)

temp_model_moke <- lm(mean_air_temp_c ~ mean_water_temp_c, data = moke)
summary(temp_model_moke)

bad_water_temps <- c(18, 20)
b0 <- temp_model_moke$coefficients[[1]]
b1 <- temp_model_moke$coefficients[[2]]

air_temp_thersholds <- (bad_water_temps - b0) / b1


# temperature data from Mike Urkov---------------------
victor <- read_csv('data-raw/mokelumne_river/Victor15min.csv')

victor_mean_temps <- victor %>%
  mutate(date = as.Date(Time, '%H:%M:%S %m/%d/%Y')) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(mean_water_temp_c = mean(WaterTemperatureCelsius, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  select(date, mean_water_temp_c)

victor_mean_temps %>% summarise(min(date), max(date))

victor_mean_temps %>%
  mutate(month = factor(x = month(date), labels = month.name, ordered = TRUE)) %>%
  ggplot(aes(x = date, y = mean_water_temp_c, fill = month)) +
  geom_col()


#Lodi
lodi2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
                     startdate = '1994-01-01', enddate = '2003-12-31', limit = 120, token = token)

lodi3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
                     startdate = '2004-01-01', enddate = '2013-12-31', limit = 120, token = token)


moke_at <- lodi2$data %>%
  bind_rows(lodi3$data) %>%
  select(date, mean_air_temp_c = value) %>%
  mutate(date = as_date(ymd_hms(date)))

moke_at %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()

moke <- victor_mean_temps %>%
  left_join(moke_at) %>%
  filter(!is.na(mean_water_temp_c), !is.na(mean_air_temp_c)) %>%
  mutate(early = ifelse(month(date) > 8, TRUE, FALSE))

moke_temp_model <- lm(mean_water_temp_c ~ mean_air_temp_c + early, data = moke)
summary(moke_temp_model)
augment(moke_temp_model) %>% glimpse()

moke %>%
  mutate(month = factor(x = month(date), labels = month.name, ordered = TRUE)) %>%
  ggplot() +
  geom_point(aes(mean_air_temp_c, mean_water_temp_c, color = month, group = early)) +
  geom_line(data = augment(moke_temp_model), aes(x = mean_air_temp_c, y = .fitted, group = early))


lodi4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
                     startdate = '1980-01-01', enddate = '1989-12-31', limit = 120, token = token)

lodi5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
                     startdate = '1990-01-01', enddate = '1999-12-31', limit = 120, token = token)

lodi4$data %>%
  bind_rows(lodi5$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()

#imputation for cdo air temps
lodi_air_temp <- lodi4$data %>%
  bind_rows(lodi5$data) %>%
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


ts_lodi <- ts(lodi_air_temp$mean_air_temp_c, start = c(1980, 1), end = c(1999, 12), frequency = 12)

na.interp(ts_lodi) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_lodi, series = 'Original')

moke_air_temp_c <- tibble(
  date = seq.Date(ymd('1980-01-01'), ymd('1999-12-01'), by = 'month'),
  mean_air_temp_c = as.numeric(na.interp(ts_lodi))) %>%
  mutate(early = ifelse(month(date) > 8, TRUE, FALSE))

moke_air_temp_c %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col(fill = 'darkgoldenrod2') +
  geom_col(data = lodi_air_temp, aes(x = date, y = mean_air_temp_c)) +
  theme_minimal() +
  labs(y = 'monthly mean air temperature (°C)')

moke_predicted_water_temp <- predict(moke_temp_model, moke_air_temp_c)

moke_water_temp_c <- tibble(
  date = seq.Date(ymd('1980-01-01'), ymd('1999-12-01'), by = 'month'),
  `Mokelumne River` = moke_predicted_water_temp)

moke_water_temp_c %>%
  ggplot(aes(x = date, y = `Mokelumne River`)) +
  geom_col(alpha = .2) +
  geom_col(data = victor_mean_temps, aes(y = mean_water_temp_c), alpha = .4) +
  geom_hline(yintercept = 18)

write_rds(moke_water_temp_c, 'data-raw/mokelumne_river/mokelumne_river_water_temp_c.rds')
