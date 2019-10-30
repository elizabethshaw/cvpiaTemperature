library(tidyverse)
library(lubridate)
library(CDECRetrieve)
library(rnoaa)
library(forecast)

# CDEC water temperature near Emmaton---------------------
emmaton <- cdec_query(station = 'EMM', sensor_num = '25', dur_code = 'H',
                      start_date = '1999-02-23', end_date = '2017-12-31')

glimpse(emmaton)

# filter out unreliable values and convert to celsius, use water temperature with air temperture to fit model
north_delta <- emmaton %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(mean_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  filter(mean_temp_f < 100, mean_temp_f > 32) %>% #remove errors
  mutate(mean_temp_c = (mean_temp_f - 32) * 5/9)

annotate_years <- function(year) {
  annotate("rect", xmin = ymd(paste0(year,'-06-01')), xmax = ymd(paste0(year, '-09-01')),
           ymin = 0, ymax = Inf, alpha = 0.3)
}

ggplot(north_delta, aes(x = date, y = mean_temp_c)) +
  geom_line(color = 'orange') +
  annotate_years(1999:2017) +
  geom_hline(yintercept = 18, linetype = 2, size = .2) +
  geom_hline(yintercept = 20, linetype = 2, size = .2) +
  labs(title = 'emmaton', y = 'monthly mean (°C)') +
  theme_minimal()

# NOAA air temperature near Antioch, CA --------------------------

# Air temperature values for use with temperature model to predict water temperature
antioch1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                        startdate = '1979-01-01', enddate = '1979-12-31', token = token, limit = 12)
antioch2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                       startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
antioch3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                     startdate = '1990-01-01', enddate = '1999-12-31', token = token, limit = 130)
antioch4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                        startdate = '2000-01-01', enddate = '2000-12-31', token = token, limit = 12)

antioch1$data %>%
  bind_rows(antioch2$data) %>%
  bind_rows(antioch3$data) %>%
  bind_rows(antioch4$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  ggplot(aes(x = date, y = value)) +
  geom_col()

# Air temperature values for training temperature model
antioch4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                        startdate = '1999-01-01', enddate = '2008-12-31', token = token, limit = 130)
antioch5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00040232', datatypeid = 'TAVG',
                        startdate = '2009-01-01', enddate = '2017-12-31', token = token, limit = 130)

antioch4$data %>%
  bind_rows(antioch5$data) %>%
  mutate(date = ymd_hms(date)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_col()

air_temp_training <- antioch4$data %>%
  bind_rows(antioch5$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, air_temp_c = value)

water_temp_training <- north_delta %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(water_temp_c = mean(mean_temp_c, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  ungroup() %>%
  select(date, water_temp_c)

# strong linear relationship between water and air temperature
water_temp_training %>%
  left_join(air_temp_training) %>%
  filter(!is.na(air_temp_c)) %>%
  ggplot(aes(x = air_temp_c, y = water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

north_delta_training <- water_temp_training %>%
  left_join(air_temp_training) %>%
  filter(!is.na(air_temp_c))

# temperature model water temp as a function of air temp -----------------
north_delta_temp_model <- lm(water_temp_c ~ air_temp_c, north_delta_training)
summary(north_delta_temp_model)

north_delta_air_temp <- antioch1$data %>%
  bind_rows(antioch2$data) %>%
  bind_rows(antioch3$data) %>%
  bind_rows(antioch4$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, air_temp_c = value) %>%
  bind_rows(
    tibble(date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
           air_temp_c = 0)
  ) %>%
  group_by(date) %>%
  summarise(air_temp_c = max(air_temp_c)) %>%
  ungroup() %>%
  mutate(air_temp_c = ifelse(air_temp_c == 0, NA, air_temp_c))

# need to imupte values for missing air temperature values between 1980-1999 for predicting water temp----------
ts_north_delta_at <- ts(north_delta_air_temp$air_temp_c, start = c(1979, 1), end = c(2000, 12), frequency = 12)

na.interp(ts_north_delta_at) %>% autoplot(series = 'Interpolated') +
  forecast::autolayer(ts_north_delta_at, series = 'Original')

north_delta_air_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  air_temp_c = as.numeric(na.interp(ts_north_delta_at)))

# use air temp (with impute values) to predict water temp---------
north_delta_air_pred <- predict(north_delta_temp_model, north_delta_air_temp_c)

north_delta_water_temp_c <- tibble(
  date = seq.Date(ymd('1979-01-01'), ymd('2000-12-01'), by = 'month'),
  `North Delta` = north_delta_air_pred)

north_delta_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = `North Delta`)) +
  geom_hline(yintercept = 18, size = .2) +
  geom_hline(yintercept = 20, size = .2) +
  theme_minimal()

write_rds(north_delta_water_temp_c, 'data-raw/deltas/north_delta_water_temp_c.rds')
