library(rnoaa)
library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(caret)

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
write_rds(lodi1, 'data-raw/mokelumne_river/lodi1.rds')
lodi1 <- read_rds('data-raw/mokelumne_river/lodi1.rds')
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
frandy <- read_csv('data-raw/mokelumne_river/Frandy15min.csv')
frandy_mean_temps <- frandy %>%
  mutate(date = as.Date(date, '%H:%M:%S %m/%d/%Y')) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(mean_water_temp_c = mean(water_tempC, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  select(date, mean_water_temp_c)

frandy_mean_temps %>% summarise(min(date), max(date))


#Lodi
# lodi2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
#                      startdate = '1994-01-01', enddate = '2003-12-31', limit = 120, token = token)
#
# lodi3 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
#                      startdate = '2004-01-01', enddate = '2013-12-31', limit = 120, token = token)
# write_rds(lodi2, 'data-raw/mokelumne_river/lodi2.rds')
# write_rds(lodi3, 'data-raw/mokelumne_river/lodi3.rds')
lodi2 <- read_rds('data-raw/mokelumne_river/lodi2.rds')
lodi3 <- read_rds('data-raw/mokelumne_river/lodi3.rds')

moke_at <- lodi2$data %>%
  bind_rows(lodi3$data) %>%
  select(date, mean_air_temp_c = value) %>%
  mutate(date = as_date(ymd_hms(date)))

moke_at %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()

frandy_mean_temps %>%
  left_join(moke_at) %>%
  ggplot(aes(x = mean_air_temp_c, y = mean_water_temp_c)) +
  geom_point(aes(color = as.character(month(date)))) +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 20, alpha = .2) +
  geom_hline(yintercept = 18, alpha = .2)


# lodi4 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
#                      startdate = '1980-01-01', enddate = '1989-12-31', limit = 120, token = token)
#
# lodi5 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
#                      startdate = '1990-01-01', enddate = '1999-12-31', limit = 120, token = token)
# write_rds(lodi4, 'data-raw/mokelumne_river/lodi4.rds')
# write_rds(lodi5, 'data-raw/mokelumne_river/lodi5.rds')

lodi4 <- read_rds('data-raw/mokelumne_river/lodi4.rds')
lodi5 <- read_rds('data-raw/mokelumne_river/lodi5.rds')

lodi4$data %>%
  bind_rows(lodi5$data) %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_temp_c = value) %>%
  ggplot(aes(x = date, y = mean_air_temp_c)) +
  geom_col()

#imputation
