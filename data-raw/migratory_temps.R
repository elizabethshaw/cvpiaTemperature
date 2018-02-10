library(tidyverse)
library(CDECRetrieve)
library(dataRetrieval)
library(lubridate)


# South Delta Tribs
sdt <- cdec_query(stations = 'MOK', sensor_num = '25', dur_code = 'E', start_date = '2008-06-17')

daily_mean_temp_sd <- sdt %>%
  filter(between(parameter_value, 32, 100)) %>%
  group_by(date = as_date(datetime)) %>%
  summarise(temp_f = mean(parameter_value, na.rm = TRUE))

write_rds(daily_mean_temp_sd, 'data-raw/sd_mc_temp.rds')

ggplot(aes(x = date, y = temp_f)) +
  geom_line()

# San Joaquin Tribs
# https://help.waterdata.usgs.gov/stat_code
sjt <- dataRetrieval::readNWISdv(siteNumbers = '11303500', parameterCd = '00010',
                                 startDate = '1979-01-01', endDate = '1999-12-31', statCd = c('00001', '00002'))
sjt %>%
  select(date = Date, min_temp_c = X_00010_00001, max_temp_c = X_00010_00002) %>%
  mutate(mean_min_max = (min_temp_c + max_temp_c)/2) %>%
  gather(stat, temp, - date) %>%
  ggplot(aes(x = date, y = temp, color = stat)) +
  geom_line()

# Sacramento Tribs
sact <- cdec_query(stations = 'WLK', sensor_num = '25', dur_code = 'E', start_date = '2012-11-15')

daily_mean_temp_sac <- sact %>%
  filter(between(parameter_value, 32, 100)) %>%
  group_by(date = as_date(datetime)) %>%
  summarise(temp_f = mean(parameter_value, na.rm = TRUE))

write_rds(daily_mean_temp_sac, 'data-raw/sac_mc_temp.rds')
