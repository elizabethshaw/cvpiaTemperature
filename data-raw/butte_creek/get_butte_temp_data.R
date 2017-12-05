library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(CDECRetrieve)
library(forcats)
library(rnoaa)

# hourly water temperature data on butte creek near durham (fahrenheit)
# butte_nr_durham <- CDECRetrieve::cdec_query(stations = 'BCD', sensor_num = '25', dur_code = 'H',
#                                            start_date = '1980-01-01', end_date = '1999-12-31')
# write_rds(butte_nr_durham, 'data-raw/butte_creek/butte_nr_durham.rds')
# data only available 1998-1999, weird website says more data available

# hourly water temperature data on butte creek near chico (fahrenheit)
# butte_nr_chico <- CDECRetrieve::cdec_query(stations = 'BCK', sensor_num = '25', dur_code = 'H',
                                           # start_date = '1998-09-16', end_date = '2017-09-30')

# write_rds(butte_nr_chico, 'data-raw/butte_creek/butte_nr_chico.rds')

# saved result from above query
butte_nr_durham <- read_rds('data-raw/butte_creek/butte_nr_durham.rds')
butte_nr_chico <- read_rds('data-raw/butte_creek/butte_nr_chico.rds')

butte_nr_durham %>%
  select(datetime, temp_f = parameter_value) %>%
  group_by(date = as_date(datetime)) %>%
  summarise(daily_mean_temp_f = mean(temp_f, na.rm = TRUE))

# air temperature data near
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

chico_avg_temp <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00041715', startdate = '1980-01-01',
                     datatypeid = 'TAVG', enddate = '1989-12-31', token = token, limit = 120)

View(chico_avg_temp$data)

chico_avg <- rnoaa::ncdc(datasetid = 'GHCND', stationid = 'GHCND:USC00041715', startdate = '1980-01-01',
                        enddate = '1980-12-31', token = token, limit = 370)

View(chico_avg$data)
