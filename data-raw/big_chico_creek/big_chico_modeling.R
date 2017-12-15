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

chico <- rnoaa::ncdc(datasetid = 'GSOM', locationid = 'CITY:US060005',datatypeid = 'TAVG',
                     startdate = '1998-01-01', enddate = '2007-12-31', token = token, limit = 1000)

chico$data %>%
  group_by(station) %>%
  summarise(n())

# 2 GHCND:USC00042402   119
# 3 GHCND:USC00046685   117
# 4 GHCND:USR0000CCHC   119
# 5 GHCND:USR0000CCOH   120
