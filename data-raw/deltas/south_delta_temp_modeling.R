library(tidyverse)
library(lubridate)
library(CDECRetrieve)
library(rnoaa)

# middle river at tracy blvd
middle_river <- cdec_query(stations = 'MTB', sensor_num = '25', dur_code = 'E',
                           start_date = '2002-10-30', end_date = '2017-12-31')

glimpse(middle_river)

south_delta <- middle_river %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(mean_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  filter(mean_temp_f < 100) %>% #remove errors
  mutate(mean_temp_c = (mean_temp_f - 32) * 5/9)

annotate_years <- function(year) {
  annotate("rect", xmin = ymd(paste0(year,'-06-01')), xmax = ymd(paste0(year, '-09-01')),
           ymin = 0, ymax = Inf, alpha = 0.3)
}

ggplot(south_delta, aes(x = date, y = mean_temp_c)) +
  geom_line(color = 'orange') +
  annotate_years(2002:2017) +
  geom_hline(yintercept = 18, linetype = 2, size = .2) +
  geom_hline(yintercept = 20, linetype = 2, size = .2) +
  labs(title = 'emmaton', y = 'monthly mean (°C)') +
  theme_minimal()

# air temperature from noa - try a few different sites
# tracy <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00048999', datatypeid = 'TAVG',
#                      startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
#
# stockton <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00048560', datatypeid = 'TAVG',
#                         startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
#
# stockton_airport1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023237', datatypeid = 'TAVG',
#                                 startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
#
# tracy$data %>%
#   mutate(date = ymd_hms(date)) %>%
#   ggplot(aes(x = date, y = value)) +
#   geom_col()
#
# stockton$data %>%
#   mutate(date = ymd_hms(date)) %>%
#   ggplot(aes(x = date, y = value)) +
#   geom_col()
#
# # the best, geographically and data coverage
# stockton_airport$data %>%
#   mutate(date = ymd_hms(date)) %>%
#   ggplot(aes(x = date, y = value)) +
#   geom_col()

stockton_airport1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023237', datatypeid = 'TAVG',
                                startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
stockton_airport2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023237', datatypeid = 'TAVG',
                                 startdate = '1990-01-01', enddate = '1999-12-31', token = token, limit = 130)

stockton_airport1$data %>%
  bind_rows(stockton_airport2$data) %>%
  mutate(date = ymd_hms(date)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_col()

stockton_airport3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023237', datatypeid = 'TAVG',
                                 startdate = '2002-01-01', enddate = '2011-12-31', token = token, limit = 130)
stockton_airport4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023237', datatypeid = 'TAVG',
                                 startdate = '2012-01-01', enddate = '2017-12-31', token = token, limit = 130)

stockton_airport3$data %>%
  bind_rows(stockton_airport4$data) %>%
  mutate(date = ymd_hms(date)) %>%
  ggplot(aes(x = date, y = value)) +
  geom_col()

air_temp_training <- stockton_airport3$data %>%
  bind_rows(stockton_airport4$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, air_temp_c = value)

water_temp_training <- south_delta %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(water_temp_c = mean(mean_temp_c, na.rm = TRUE)) %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  ungroup() %>%
  select(date, water_temp_c)

water_temp_training %>%
  left_join(air_temp_training) %>%
  ggplot(aes(x = air_temp_c, y = water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

south_delta_temp <- water_temp_training %>%
  left_join(air_temp_training)

temp_model <- lm(water_temp_c ~ air_temp_c, south_delta_temp)
summary(temp_model)

south_delta_air_temp_c <- stockton_airport1$data %>%
  bind_rows(stockton_airport2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, air_temp_c = value)

south_delta_water_temp_pred <- predict(temp_model, south_delta_air_temp_c)

south_delta_water_temp_c <- tibble(
  date = seq.Date(ymd('1980-01-01'), ymd('1999-12-01'), by = 'month'),
  `South Delta` = south_delta_water_temp_pred)

south_delta_water_temp_c %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = `South Delta`)) +
  geom_hline(yintercept = 18) +
  geom_hline(yintercept = 20) +
  theme_minimal()

write_rds(south_delta_water_temp_c, 'data-raw/deltas/south_delta_water_temp_c.rds')
