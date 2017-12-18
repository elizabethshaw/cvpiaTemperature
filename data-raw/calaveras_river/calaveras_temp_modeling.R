library(tidyverse)
library(lubridate)
library(CDECRetrieve)
library(rnoaa)

# water temp calaveras below new hogan
calaveras <- cdec_query(stations = 'NGQ', sensor_num = '25', dur_code = 'E',
                        start_date = '2010-01-20', end_date = '2017-12-01')

calaveras %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(daily_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  ggplot(aes(date, daily_temp_f)) +
  geom_col()

# GHCND:USW00023237
