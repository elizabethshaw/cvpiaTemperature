library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(rnoaa)

# usgs water temp
deer_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11383500', parameterCd = '00010',
                                           startDate = '1998-10-05', endDate = '2017-11-10',
                                           statCd = c('00001', '00002', '00008'))

# air temp from corning, ca from noaa cdo
deer_air_temp <- read_csv('data-raw/deer_creek/corning_ca_air_temp.csv')

glimpse(deer_air_temp)

# lots of missing median values from gage, use mean of min and max water temp to approximate median water temp
glimpse(deer_water_temp)
deer_water_temp %>%
  select(date = Date, temp_c_max = X_00010_00001,
         temp_c_min = X_00010_00002, temp_c_med = X_00010_00008) %>%
  filter(!is.na(temp_c_med), temp_c_med > 18) %>%
  mutate(mean_min_max = (temp_c_min + temp_c_max)/2,
         dist_mean_med = abs(mean_min_max - temp_c_med),
         dist_max_med = abs(temp_c_max - temp_c_med),
         dist_min_med = abs(temp_c_min - temp_c_med)) %>%
  select(dist_mean_med, dist_min_med, dist_max_med) %>%
  gather(dist_type, dist) %>%
  ggplot(aes(x = dist, color = dist_type)) +
  geom_density()

dt <- deer_water_temp %>%
  select(date = Date, temp_c_max = X_00010_00001,
         temp_c_min = X_00010_00002) %>%
  mutate(water_temp_c = (temp_c_min + temp_c_max)/2) %>%
  select(date, water_temp_c)

da <- deer_air_temp_max %>%
  mutate(air_temp_c = (TAVG - 32) * 5 / 9) %>%
  select(date = DATE, air_temp_c)

deer_creek <- da %>%
  left_join(dt) %>%
  group_by(month = month(date), year = year(date)) %>%
  summarise(mean_air_temp_c = mean(air_temp_c, na.rm = TRUE),
            mean_water_temp_c = mean(water_temp_c, na.rm = TRUE)) %>%
  filter(!is.na(mean_water_temp_c))

glimpse(deer_creek)

deer_creek %>%
  ggplot(aes(x = mean_air_temp_c, y = mean_water_temp_c)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

deer_water_temp_model <- lm(mean_water_temp_c ~ mean_air_temp_c, data = deer_creek)
summary(deer_water_temp_model)

bad_water_temps <- c(18, 20)
b0 <- deer_water_temp_model$coefficients[[1]]
b1 <- deer_water_temp_model$coefficients[[2]]

air_temp_thersholds <- (bad_water_temps - b0) / b1

# no temp during 1980-1999 near by
# GHCND:USC00041715 closest or paradise again?
# TODO ask mark
# GHCND:USW00024216
