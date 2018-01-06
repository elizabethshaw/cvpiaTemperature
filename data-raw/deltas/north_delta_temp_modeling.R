library(tidyverse)
library(lubridate)
library(CDECRetrieve)
library(rnoaa)

emmaton <- cdec_query(stations = 'EMM', sensor_num = '25', dur_code = 'H',
                      start_date = '1999-02-23', end_date = '2017-12-31')

glimpse(emmaton)

north_delta <- emmaton %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(mean_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  filter(mean_temp_f < 100, mean_temp_f > 32) %>% #remove errors
  mutate(mean_temp_c = (mean_temp_f - 32) * 5/9)

annotate_years <- function(year) {
  annotate("rect", xmin = ymd(paste0(year,'-06-01')), xmax = ymd(paste0(year, '-09-01')),
           ymin = 0, ymax = Inf, alpha = 0.3)
}

ggplot(north_delta, aes(x = date, y = mean_temp_c)) +
  geom_line(color = 'orange') +
  annotate_years(1999:2017) +
  geom_hline(yintercept = 18, linetype = 2, size = .2) +
  geom_hline(yintercept = 20, linetype = 2, size = .2) +
  labs(title = 'emmaton', y = 'monthly mean (°C)') +
  theme_minimal()

# air temp
travis <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USW00023202', datatypeid = 'TAVG',
                     startdate = '1990-01-01', enddate = '1999-12-31', token = token, limit = 130)
