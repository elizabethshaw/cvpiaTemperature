library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(ggsci)

# mike wright's notes on temperature

# Here 5q_date is the 6-hourly time stamps on the 5Q output; note that it's on
# the CalLite-CV calendar i.e. starting in 2014 (except that the SJR run starts
# in 2011, but then stops in 2011 and picks back up in 2014 so I deleted the
# 2011 data, and the American River run ends 9 months early... I don't know
# why). I've named each field after its DSM stream; tempoverview.csv contains
# the original 5Q names of the records whose values are copied here. ALL UNITS
# IN FAHRENHEIT!

# read in date mapping calLite -> calsim
cl_dates <- read_csv('data-raw/calLite_calSim_date_mapping.csv')

# read in watershed ids and names for ordering output
cvpia_watershed_ids <- read_csv('data-raw/cvpia_trib_names.csv')
cvpia_watershed <- read_csv('data-raw/cvpia_trib_names.csv') %>%
  pull(watershed)

# clean mike wright's temperature modeling output
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

# add additional modeled temperature data from sadie
monthly_mean_temperature <- temperatures %>%
  group_by(year = year(date), month = month(date), watershed) %>%
  summarise(monthly_mean_temp_c = mean(mean_daily_temp_C)) %>%
  ungroup() %>%
  mutate(cl_date = ymd(paste(year, month, 1, sep = '-'))) %>%
  left_join(cl_dates) %>%
  filter(between(year(cs_date), 1979, 1999)) %>%
  mutate(date = ymd(paste(year(cs_date), month(cs_date), 1, sep = '-'))) %>%
  select(date, watershed, monthly_mean_temp_c) %>%
  bind_rows(read_rds('data-raw/big_chico_creek/big_chico_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/butte_creek/butte_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/cosumnes_river/cosumnes_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/deer_creek/deer_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/lower_sacramento/lower_sac_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/mill_creek/mill_creek_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/mokelumne_river/mokelumne_river_water_temp_c.rds')) %>%
  bind_rows(read_rds('data-raw/yuba_river/yuba_river_water_temp_c.rds')) %>%
  spread(watershed, monthly_mean_temp_c) %>%
  mutate(`Antelope Creek` = `Cow Creek`,
         `Bear Creek` = `Cow Creek`,
         `Elder Creek` = `Thomes Creek`,
         `Paynes Creek` = `Cow Creek`,
         `Bear River` = NA,
         `Feather River` = `American River`,
         `Calaveras River` = `Mokelumne River`,
         `Yolo Bypass` = NA,
         `Sutter Bypass` = NA) %>%
  gather(watershed, monthly_mean_temp_c, -date)

juv_temp <- monthly_mean_temperature %>%
  filter(year(date) > 1979)

devtools::use_data(juv_temp, overwrite = TRUE)

# degree days
cl_years <- cl_dates %>%
  mutate(cl_year = year(cl_date),
         cs_year = year(cs_date)) %>%
  select(cl_year, cs_year) %>%
  unique()

# watershed id zeros: 16*, 17, 21, 22, 24, 31 (no spawning)
# *upper mid sac (16) spawning area is represented within upper sac in model
zero_watersheds <- cvpia_watershed_ids %>%
  filter(order %in% c(16, 17, 21, 22, 24, 31)) %>%
  pull(watershed)

hec5q_degday <- temperatures %>%
  filter(!(watershed %in% zero_watersheds)) %>% #no spawning
  group_by(cl_year = year(date), month = month(date), watershed) %>%
  summarise(degdays = sum(mean_daily_temp_C, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(cl_years) %>%
  filter(between(cs_year, 1979, 1999)) %>%
  mutate(date = ymd(paste(cs_year, month, 1, sep = '-'))) %>%
  select(date, watershed, degdays)

# take modeled mean monthly flow and multiple by number of days to estimate degree days
estimate_watersheds <- cvpia_watershed[!cvpia_watershed %in% c(unique(hec5q_degday$watershed), zero_watersheds)]

estimate_degday <- monthly_mean_temperature %>%
  gather(watershed, temp, - date) %>%
  mutate(num_days = days_in_month(date),
         degdays = temp * num_days,
         date = ymd(paste(year(date), month(date), 1, sep = '-'))) %>%
  filter(watershed %in% estimate_watersheds) %>%
  select(date, watershed, degdays)

zero_degday <- tibble(
  date = rep(seq(as.Date('1979-01-01'), as.Date('1999-12-01'), by = 'month'), each = 6),
  watershed = rep(zero_watersheds, times = 252),
  degdays = as.numeric(NA)
)

deg_days <- zero_degday %>%
  bind_rows(hec5q_degday) %>%
  bind_rows(estimate_degday)

use_data(deg_days)

deg_days %>%
  spread(date, degdays) %>%
  left_join(cvpia_watershed_ids) %>%
  arrange(order) %>%
  select(-order)

modelss <- tibble(watershed = unique(hec5q_degday$watershed), model = 'hec5q') %>%
  bind_rows(tibble(watershed = unique(estimate_degday$watershed), model = 'sadie'))

deg_days %>%
  left_join(modelss) %>%
  filter(!is.na(degdays)) %>%
  ggplot(aes(x = date, y = degdays, color = model)) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~watershed)

# delta temps ----------------------------------
dn <- read_rds('data-raw/deltas/north_delta_water_temp_c.rds')
ds <- read_rds('data-raw/deltas/south_delta_water_temp_c.rds')
delta_temps <- dn %>%
  left_join(ds) %>%
  gather(watershed, monthly_mean_temp_c, -date)

use_data(delta_temps)
