library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(CDECRetrieve)
library(forcats)
library(rnoaa)

# hourly water temperature data on butte creek near durham (fahrenheit)
# butte_nr_durham <- CDECRetrieve::cdec_query(stations = 'BCD', sensor_num = '25', dur_code = 'H',
#                                            start_date = '1980-01-01', end_date = '1999-12-31')
# write_rds(butte_nr_durham, 'data-raw/butte_creek/butte_nr_durham_tempC.rds')

# data only available 1998-1999, weird website says more data available
glimpse(butte_nr_durham)
View(butte_nr_durham)

# saved result from above query
butte_nr_durham <- read_rds('data-raw/butte_creek/butte_nr_durham_tempC.rds')

butte_nr_durham %>%
  select(datetime, temp_f = parameter_value) %>%
  group_by(date = as_date(datetime)) %>%
  summarise(daily_mean_temp_f = mean(temp_f, na.rm = TRUE))

# air temperature data near
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

chico_avg_temp <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00041715', startdate = '1980-01-01',
                     datatypeid = 'TAVG', enddate = '1989-12-31', token = token)

