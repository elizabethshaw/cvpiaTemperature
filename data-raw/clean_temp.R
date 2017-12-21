library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)

# mike wright's notes on temperature

# Here 5q_date is the 6-hourly time stamps on the 5Q output; note that it's on
# the CalLite-CV calendar i.e. starting in 2014 (except that the SJR run starts
# in 2011, but then stops in 2011 and picks back up in 2014 so I deleted the
# 2011 data, and the American River run ends 9 months early... I don't know
# why). I've named each field after its DSM stream; tempoverview.csv contains
# the original 5Q names of the records whose values are copied here. ALL UNITS
# IN FAHRENHEIT!

cl_dates <- read_csv('data-raw/calLite_calSim_date_mapping.csv')
cvpia_watershed <- read_csv('data-raw/cvpia_trib_names.csv') %>%
  pull(watershed)

temperatures <- read_csv('data-raw/tempmaster.csv', skip = 1) %>%
  mutate(day_month = str_sub(`5q_date`, 1, 6),
         year = str_sub(`5q_date`, 8, 9),
         year = str_c('20', year),
         date = dmy(paste(day_month, year))) %>%
  select(-day_month, -year, -`5q_date`) %>%
  gather(watershed, temp_F, -date) %>%
  group_by(date, watershed) %>%
  summarise(mean_daily_temp_F = mean(temp_F, na.rm = TRUE),
            mean_daily_temp_C = (mean_daily_temp_F - 32) * (5/9)) %>%
  ungroup()

monthly_mean_temperature <- temperatures %>%
  group_by(year = year(date), month = month(date), watershed) %>%
  summarise(monthly_mean_temp_c = mean(mean_daily_temp_C)) %>%
  ungroup() %>%
  mutate(cl_date = ymd(paste(year, month, 1, sep = '-'))) %>%
  left_join(cl_dates) %>%
  filter(between(year(cs_date), 1980, 1999)) %>%
  select(date = cs_date, watershed, monthly_mean_temp_c) %>%
  spread(watershed, monthly_mean_temp_c) %>%
  bind_cols(select(read_rds('data-raw/big_chico_creek/big_chico_creek_water_temp_c.rds'), `Big Chico Creek`)) %>%
  bind_cols(select(read_rds('data-raw/butte_creek/butte_creek_water_temp_c.rds'), `Butte Creek`)) %>%
  bind_cols(select(read_rds('data-raw/cosumnes_river/cosumnes_water_temp_c.rds'), `Cosumnes River`)) %>%
  bind_cols(select(read_rds('data-raw/deer_creek/deer_creek_water_temp_c.rds'), `Deer Creek`)) %>%
  bind_cols(select(read_rds('data-raw/lower_sacramento/lower_sac_water_temp_c.rds'), `Lower Sacramento River`)) %>%
  bind_cols(select(read_rds('data-raw/mill_creek/mill_creek_water_temp_c.rds'), `Mill Creek`)) %>%
  bind_cols(select(read_rds('data-raw/mokelumne_river/mokelumne_river_water_temp_c.rds'), `Mokelumne River`)) %>%
  bind_cols(select(read_rds('data-raw/yuba_river/yuba_river_water_temp_c.rds'), `Yuba River`)) %>%
  mutate(`Antelope Creek` = `Cow Creek`,
         `Bear Creek` = `Cow Creek`,
         `Elder Creek` = `Thomes Creek`,
         `Paynes Creek` = `Cow Creek`,
         `Bear River` = NA,
         `Feather River` = `American River`,
         `Calaveras River` = `Mokelumne River`,
         `Yolo Bypass` = NA,
         `Sutter Bypass` = NA) %>%
  select(date, cvpia_watershed)

View(monthly_mean_temperature)

#degday
#sum daily mean tmep over oct and nov
#sum * 14/60 upper sac river
#sum * 7/60 everything else
# temperature %>%
#   dplyr::select(-`5Q date`, -`N.Delta`, -`SC.Delta`) %>%
#   tidyr::gather(watershed, temperature, -`DSM date`) %>%
#   dplyr::mutate(date = as.Date(`DSM date`)) %>%
#   dplyr::filter(lubridate::month(date) %in% c(10, 11)) %>%
#   dplyr::group_by(date, watershed) %>%
#   dplyr::summarise(avg_temp = mean(temperature, na.rm = TRUE)) %>%
#   dplyr::mutate(year = lubridate::year(date)) %>%
#   dplyr::group_by(year, watershed) %>%
#   dplyr::summarise(sum = sum(avg_temp)) %>%
#   dplyr::mutate(degday = ifelse(watershed == 'Upper Sacramento River', sum * 14/60, sum * 7/60)) %>%
#   dplyr::left_join(watershed_ordering) %>%
#   dplyr::arrange(year, order) %>%   dplyr::ungroup() %>% View()
# #zeros 16,17, 21, 22, 24, 31


# delta temps ----------------------------------
# North Delta use USGS 11455420 SACRAMENTO R A RIO VISTA CA


# South Delta use FMWT midwater trawl data
sd_temp <- read_excel('data-raw/FMWT 1967-2016 Catch Matrix_updated.xlsx', sheet = 'FlatFile')

glimpse(sd_temp)
sd_temp %>%
  select(date = Date, station = Station, temp_C = `Top Temperature (⁰C)`) %>%
  filter(date >= '1980-01-01') %>%
  group_by(month = month(date)) %>% summarise(count = n()) %>% View()
  group_by(year = year(date), month = month(date), station) %>%
  summarise(monthly_mean_tempC = mean(temp_C, na.rm = TRUE)) %>%
  arrange(year, month, station) %>% View()
  ungroup() %>%
  group_by(station) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% View()
