library(tidyverse)
library(CDECRetrieve)
library(dataRetrieval)
library(lubridate)
library(rnoaa)


# South Delta Tribs------------
# sdt <- cdec_query(stations = 'MOK', sensor_num = '25', dur_code = 'E', start_date = '2008-06-17')
#
# daily_mean_temp_sd <- sdt %>%
#   filter(between(parameter_value, 32, 100)) %>%
#   group_by(date = as_date(datetime)) %>%
#   summarise(temp_f = mean(parameter_value, na.rm = TRUE))
#
# write_rds(daily_mean_temp_sd, 'data-raw/sd_mc_temp.rds')

sdt <- read_rds('data-raw/migratory_corridor/sd_mc_temp.rds')

sdt %>% summarise(min(date, na.rm = TRUE), max(date, na.rm = TRUE))

# GHCND:USW00023237 Stockton airport https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00023237/detail
# air temperature data near stream
# token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file
#
# stockton1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023237', startdate = '2008-06-01',
#                          datatypeid = 'TAVG', enddate = '2018-01-31', token = token, limit = 240)
#
# training_years <- 2008:2018
#
# training_air_temp <- purrr::map(training_years, function(year) {
#   rnoaa::ncdc(datasetid = 'GHCND',
#               stationid = 'GHCND:USW00023237', startdate = paste0(year, '-01-01'),
#               datatypeid = c('TMIN', 'TMAX'), enddate = paste0(year, '-12-31'),
#               token = token, limit = 800)})
# ll <- length(training_air_temp)
# air_temp_training <- map_df(1:ll, ~training_air_temp[[.]]$data)
#
# write_rds(air_temp_training, 'data-raw/migratory_corridor/daily_stockton_air_temps.rds')

sd_at_trainx <- read_rds('data-raw/migratory_corridor/daily_stockton_air_temps.rds') %>%
  mutate(date = as_date(ymd_hms(date)),
         value = value/10) %>%
  select(date, value, datatype) %>%
  spread(datatype, value) %>%
  filter(!is.na(date), !is.na(TMAX), !is.na(TMIN)) %>%
  mutate(TMEAN = (TMAX + TMIN)/2)

train <- sdt %>%
  left_join(sd_at_trainx) %>%
  mutate(temp_c = (temp_f - 32) * 5/9) %>%
  filter(!is.na(date), !is.na(TMEAN))

train %>%
  filter(between(year(date), 2008, 2010)) %>%
  select(-temp_f) %>%
  gather(stat, temp, -date) %>%
  mutate(over_20 = temp > 20) %>%
  group_by(month(date), year(date), stat) %>%
  summarise(mc20_25 = sum(over_20)) %>% glimpse()
  ggplot(aes(x = date, y = temp, color = stat)) +
  geom_line() +
  geom_hline(yintercept = 20)

cor(train$temp_c, train$TMEAN, use = 'pairwise.complete.obs')

sd_model <- lm(temp_c ~ TMAX + TMIN + month, data = train)
summary(sd_model)

x <- broom::augment(sd_model) %>%
  select(temp_c, .fitted)

x$date <- train$date
x %>%
  gather(kind, temp, - date) %>%
  mutate(over20 = temp >= 20) %>%
  group_by(month = month(date), year = year(date), kind) %>%
  summarise(mc20_25 = sum(over20)) %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  ungroup() %>%
  select(date, kind, mc20_25) %>%
  spread(kind, mc20_25) %>%
  mutate(pred_v_act = .fitted - temp_c,
         month = factor(month(date), levels = 1:12, labels = month.abb)) %>%
  ggplot(aes(x = month, y = pred_v_act)) +
  geom_boxplot(aes(group = month)) +
  geom_point()
# tend to be underestimating


x %>%
  gather(kind, temp, - date) %>%
  mutate(over20 = temp >= 20) %>%
  group_by(month = month(date), year = year(date), kind) %>%
  summarise(mc20_25 = sum(over20)) %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  ungroup() %>%
  select(date, kind, mc20_25) %>%
  spread(kind, mc20_25) %>%
  mutate(pred_v_act = .fitted - temp_c,
         month = factor(month(date), levels = 1:12, labels = month.abb)) %>%
  filter(month(date) %in% 5:10) %>%
  ggplot(aes(x = pred_v_act)) +
  geom_density()


str(training_air_temp, max.level = 1)

(20 - sd_model$coefficients[[1]]) / sd_model$coefficients[[2]]


# San Joaquin Tribs
# https://help.waterdata.usgs.gov/stat_code
sjt <- dataRetrieval::readNWISdv(siteNumbers = '11303500', parameterCd = '00010',
                                 startDate = '1979-01-01', endDate = '1999-12-31', statCd = c('00001', '00002'))
sjt %>%
  select(date = Date, min_temp_c = X_00010_00001, max_temp_c = X_00010_00002) %>%
  mutate(mean_min_max = (min_temp_c + max_temp_c)/2) %>%
  gather(stat, temp, - date) %>%
  ggplot(aes(x = date, y = temp, color = stat)) +
  geom_line()

# Sacramento Tribs
sact <- cdec_query(stations = 'WLK', sensor_num = '25', dur_code = 'E', start_date = '2012-11-15')

daily_mean_temp_sac <- sact %>%
  filter(between(parameter_value, 32, 100)) %>%
  group_by(date = as_date(datetime)) %>%
  summarise(temp_f = mean(parameter_value, na.rm = TRUE))

write_rds(daily_mean_temp_sac, 'data-raw/sac_mc_temp.rds')
