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

sdt_median_p20 <- sdt %>%
  mutate(temp_c = (temp_f - 32) * 5/9,
         over20 = temp_c >= 20) %>%
  filter(!is.na(date)) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(p20 = sum(over20)/n()) %>%
  group_by(month) %>%
  summarise(median_p20 = median(p20)) %>%
  mutate(group = 'South Delta')


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
# sd_at_trainx <- read_rds('data-raw/migratory_corridor/daily_stockton_air_temps.rds') %>%
#   mutate(date = as_date(ymd_hms(date)),
#          value = value/10) %>%
#   select(date, value, datatype) %>%
#   spread(datatype, value) %>%
#   filter(!is.na(date), !is.na(TMAX), !is.na(TMIN)) %>%
#   mutate(TMEAN = (TMAX + TMIN)/2)


# San Joaquin Tribs---------------------------------
# https://help.waterdata.usgs.gov/stat_code
sjt <- dataRetrieval::readNWISdv(siteNumbers = '11303500', parameterCd = '00010',
                                 startDate = '1979-01-01', endDate = '1999-12-31', statCd = c('00001', '00002'))
sjt_median_p20 <- sjt %>%
  select(date = Date, min_temp_c = X_00010_00001, max_temp_c = X_00010_00002) %>%
  mutate(mean_min_max = (min_temp_c + max_temp_c)/2) %>%
  mutate(over20 = mean_min_max >= 20) %>%
  filter(!is.na(date), !is.na(mean_min_max)) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(p20 = sum(over20)/n()) %>%
  group_by(month) %>%
  summarise(median_p20 = median(p20)) %>%
  mutate(group = 'San Joaquin')

# training_years <- 1979:1999
# training_air_temp <- purrr::map_df(training_years, function(year) {
#     t <- rnoaa::ncdc(datasetid = 'GHCND',
#                 stationid = 'GHCND:USC00048999', startdate = paste0(year, '-01-01'),
#                 datatypeid = c('TMIN', 'TMAX'), enddate = paste0(year, '-12-31'),
#                 token = token, limit = 800)
#     t$data})
# write_rds(training_air_temp, 'data-raw/migratory_corridor/daily_tracy_air_temps.rds')
# sjt_air_temp <- read_rds('data-raw/migratory_corridor/daily_tracy_air_temps.rds') %>%
#   mutate(date = as_date(ymd_hms(date)),
#          value = value/10) %>%
#   select(date, value, datatype) %>%
#   spread(datatype, value) %>%
#   filter(!is.na(date), !is.na(TMAX), !is.na(TMIN)) %>%
#   mutate(TMEAN = (TMAX + TMIN) / 2)


# Sacramento Tribs------------------------------------------
# sact <- cdec_query(stations = 'WLK', sensor_num = '25', dur_code = 'E', start_date = '2012-11-15')
#
# daily_mean_temp_sac <- sact %>%
#   filter(between(parameter_value, 32, 100)) %>%
#   group_by(date = as_date(datetime)) %>%
#   summarise(temp_f = mean(parameter_value, na.rm = TRUE))
#
# write_rds(daily_mean_temp_sac, 'data-raw/migratory_corridor/sac_mc_temp.rds')

# training_years <- 2012:2017
# training_air_temp <- purrr::map_df(training_years, function(year) {
#     t <- rnoaa::ncdc(datasetid = 'GHCND',
#                 stationid = 'GHCND:USC00041948', startdate = paste0(year, '-01-01'),
#                 datatypeid = c('TMIN', 'TMAX'), enddate = paste0(year, '-12-31'),
#                 token = token, limit = 800)
#     t$data})
# write_rds(training_air_temp, 'data-raw/migratory_corridor/daily_colusa_air_temps.rds')
#
# sac_air_temp <- read_rds('data-raw/migratory_corridor/daily_colusa_air_temps.rds') %>%
#   mutate(date = as_date(ymd_hms(date)),
#          value = value/10) %>%
#   select(date, value, datatype) %>%
#   spread(datatype, value) %>%
#   filter(!is.na(date), !is.na(TMAX), !is.na(TMIN)) %>%
#   mutate(TMEAN = (TMAX + TMIN) / 2)

sact_median_p20 <- read_rds('data-raw/migratory_corridor/sac_mc_temp.rds') %>%
  mutate(temp_c = (temp_f - 32) * 5/9,
         over20 = temp_c >= 20) %>%
  filter(!is.na(date)) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(p20 = sum(over20)/n()) %>%
  group_by(month) %>%
  summarise(median_p20 = median(p20)) %>%
  mutate(group = 'Sacramento')


bind_rows(sdt_median_p20, sjt_median_p20, sact_median_p20) %>%
  ggplot(aes(x = month, y = median_p20, fill = group)) +
  geom_col(position = 'dodge')

median_p20 <- bind_rows(sdt_median_p20, sjt_median_p20, sact_median_p20)

watershed_groups <- cvpiaData::watershed_ordering %>%
  mutate(group = case_when(
    order %in% 28:30 ~ 'San Joaquin',
    order %in% 25:27 ~ 'South Delta',
    order %in% c(16, 17, 21, 22, 24, 31) ~ as.character(NA),
    TRUE ~ 'Sacramento'
  ))

missing <- cvpiaData::watershed_ordering[c(16, 17, 21, 22, 24, 31), ]
other_sheds <- tibble(order = rep(missing$order, 12),
       watershed = rep(missing$watershed, 12),
       month = rep(1:12, each = 6),
       median_p20 = NA)

prop_temp_over_20_migr_cor <- median_p20 %>%
  left_join(watershed_groups) %>%
  select(order, watershed, month, median_p20) %>%
  bind_rows(other_sheds) %>%
  arrange(month, order)

devtools::use_data(prop_temp_over_20_migr_cor, overwrite = TRUE)

