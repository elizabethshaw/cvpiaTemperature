library(tidyverse)
library(lubridate)
library(rnoaa)
library(dataRetrieval)

# yuba river near marysville ca  1964-10-01 	 2003-09-29
yuba_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11421000', parameterCd = '00010',
                                              startDate = '1980-01-01', endDate = '1999-12-31',
                                              statCd = c('00001', '00002'))
glimpse(yuba_water_temp) # incomplete record during period of interest

yuba_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11421000', parameterCd = '00010',
                                             startDate = '1964-10-01', endDate = '2003-09-29',
                                             statCd = c('00001', '00002'))

yuba_water_temp %>% summarise(min(Date), max(Date))

yuba_water_temp %>%
  select(date = Date, max = X_00010_00001, min = X_00010_00002) %>%
  ggplot(aes(x = date, y = max)) +
  geom_col()

# 1990 and onward most complete continuous record

# GHCND:USC00045385
# yuba1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
#                           startdate = '1989-01-01', enddate = '1998-12-31', token = token, limit = 130)
# yuba2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
#                      startdate = '1999-01-01', enddate = '2003-12-31', token = token, limit = 130)
# write_rds(yuba1, 'data-raw/yuba_river/yuba1.rds')
# write_rds(yuba2, 'data-raw/yuba_river/yuba2.rds')

yuba1 <- read_rds('data-raw/yuba_river/yuba1.rds')
yuba2 <- read_rds('data-raw/yuba_river/yuba2.rds')

yuba_air_temp <- yuba1$data %>%
  bind_rows(yuba2$data) %>%
  select(date, mean_air_temp_c = value) %>%
  mutate(date = as_date(ymd_hms(date)))

yuba_air_temp %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()

yuba3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
                     startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
yuba4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
                     startdate = '1990-01-01', enddate = '1999-12-31', token = token, limit = 130)

yuba3$data %>%
  bind_rows(yuba4$data) %>%
  select(date, mean_air_temp_c = value) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()
#data gaps
