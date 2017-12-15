library(tidyverse)
library(lubridate)
library(rnoaa)
library(dataRetrieval)

mill_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11381500', parameterCd = '00010',
                                             startDate = '1998-10-05', endDate = '2017-12-15',
                                             statCd = c('00001', '00002', '00008'))

mill_water_temp %>%
  select(date = Date, max = X_00010_00001,
         min = X_00010_00002, med = X_00010_00008) %>%
  filter(!is.na(med)) %>%
  gather(stat, temp_c, -date) %>%
  group_by(year = year(date), month = month(date), stat) %>%
  summarise(mean_temp_c = mean(temp_c)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  select(date, stat, mean_temp_c) %>%
  spread(stat, mean_temp_c) %>%
  mutate(mean_max_min = (max + min)/2,
         dist_min = abs(min - med),
         dist_max = abs(max - med),
         dist_mean_max_min = abs(mean_max_min - med)) %>%
  select(date, dist_min:dist_mean_max_min) %>%
  gather(category, distance, -date) %>%
  ggplot(aes(x = distance, color = category)) +
  geom_density()

# median temp value is sparcely populated, use mean of min and max temp to approximate median
mill_wt <- mill_water_temp %>%
  select(date = Date, max = X_00010_00001,
         min = X_00010_00002, med = X_00010_00008) %>%
  gather(stat, temp_c, -date) %>%
  group_by(year = year(date), month = month(date), stat) %>%
  summarise(mean_temp_c = mean(temp_c)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  select(date, stat, mean_temp_c) %>%
  spread(stat, mean_temp_c) %>%
  mutate(mean_max_min = (max + min)/2) %>%
  select(date, mean_water_temp_c = mean_max_min)

# air temperature data near stream
token <- Sys.getenv("token") #noaa cdo api token saved in .Renviron file

# RED BLUFF MUNICIPAL AIRPORT, CA US GHCND:USW00024216
# red_bluff1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
#                          startdate = '1998-01-01', enddate = '2007-12-31', token = token, limit = 130)
# red_bluff2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', startdate = '2008-01-01',
#                          datatypeid = 'TAVG', enddate = '2017-11-30', token = token, limit = 120)
# write_rds(red_bluff1, 'data-raw/mill_creek/red_bluff1.rds')
# write_rds(red_bluff2, 'data-raw/mill_creek/red_bluff2.rds')

red_bluff1 <- read_rds('data-raw/mill_creek/red_bluff1.rds')
red_bluff2 <- read_rds('data-raw/mill_creek/red_bluff2.rds')

mill_at <- red_bluff1$data %>%
  bind_rows(red_bluff2$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value)

mill_creek <- mill_wt %>%
  left_join(mill_at) %>%
  filter(!is.na(mean_air_temp_c))

mill_creek %>%
  ggplot(aes(x = mean_air_temp_c, y = mean_water_temp_c)) +
  geom_point()

mill_temp_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = mill_creek)
summary(mill_temp_model)

red_bluff3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
                         startdate = '1980-01-01', enddate = '1989-12-31', token = token, limit = 130)
red_bluff4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00024216', datatypeid = 'TAVG',
                          startdate = '1990-01-01', enddate = '1999-12-31', token = token, limit = 130)

red_bluff3$data %>%
  bind_rows(red_bluff4$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()
#data gaps
