library(rnoaa)
library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(caret)

buoy_port_chicago <- rnoaa::buoy(dataset = 'stdmet', buoyid = 'PCOC1')

bpc <- buoy_port_chicago$data %>%
  select(time, air_temperature, sea_surface_temperature) %>%
  mutate(date = as_date(ymd_hms(time)), sea_tempF = sea_surface_temperature * 9/5 + 32,
         air_tempF = air_temperature * 9/5 + 32) %>%
  group_by(date) %>%
  summarise(sea_tempF = mean(sea_tempF, na.rm = TRUE),
            air_tempF = mean(air_tempF, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = year(date), month = month(date), day_of_year = yday(date))

ggplot(bpc, aes(x = air_tempF, y = sea_tempF, color = day_of_year)) +
  geom_point() +
  # geom_smooth(method = 'lm') +
  geom_hline(yintercept = 64.4)

bpc_model <- lm(sea_tempF ~ air_tempF + day_of_year, bpc)
summary(bpc_model)
