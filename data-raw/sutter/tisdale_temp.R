library(tidyverse)
library(readxl)
library(lubridate)

# clean water temp data from RST at Sutter Tisdale from diane.coulon@wildlife.ca.gov---------------------
# t11 <- readxl::read_excel('data-raw/sutter/2011-12 Tisdale  Catch Summary.xls', skip = 5, col_names = FALSE) %>%
#   select(date = X__3, water_temp_f = X__11) %>%
#   group_by(date = as_date(date)) %>%
#   summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE))
#
# t12 <- readxl::read_excel('data-raw/sutter/2012-13 Tisdale Catch Summary.xlsx', skip = 7, col_names = FALSE) %>%
#   select(date = X__1, water_temp_f = X__9) %>%
#   group_by(date = as_date(date)) %>%
#   summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE)) #
#
# t13 <- readxl::read_excel('data-raw/sutter/2013-14 Tisdale Catch Summary AC.xlsx', skip = 7, col_names = FALSE) %>%
#   select(date = X__1, water_temp_f = X__12) %>% filter(is.na(date))
#   group_by(date = as_date(date)) %>%
#   summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE)) #
#
# t14 <- readxl::read_excel('data-raw/sutter/2014-15 Tisdale Catch Summary.xlsx', skip = 7, col_names = FALSE) %>%
#   select(date = X__1, water_temp_f = X__12) %>%
#   group_by(date = as_date(date)) %>%
#   summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE))
#
# t15 <- readxl::read_excel('data-raw/sutter/2015-16 Tisdale Catch Summary.xlsx', skip = 7, col_names = FALSE) %>%
#   select(date = X__1, water_temp_f = X__12) %>%
#   group_by(date = as_date(date)) %>%
#   summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE))
#
#
# t16 <- readxl::read_excel('data-raw/sutter/Tisdale_RST_Catch_Data_2016-2017.xlsx', skip = 8, col_names = FALSE, na = 'n/a') %>%
#   select(date = X__1, water_temp_f = X__12) %>%
#   group_by(date = as_date(date)) %>%
#   summarise(water_temp_f = mean(water_temp_f)) %>%
#   filter(!is.na(water_temp_f))
#
# t17 <- readxl::read_excel('data-raw/sutter/Tisdale_RST_Catch_Data_2017-2018.xlsx', skip = 8, col_names = FALSE) %>%
#   select(date = X__1, water_temp_f = X__13) %>%
#   group_by(date = as_date(date)) %>%
#   summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE))
#
# which(LETTERS == 'M')
#
# bind_rows(t11, t12, t13, t14, t15, t16, t17) %>%
# ggplot(aes(x = date, y = water_temp_f)) +
#   geom_col()
#
# bind_rows(t11, t12, t13, t14, t15, t16, t17) %>%
#   filter(!is.na(date)) %>%
#   write_csv('data-raw/sutter/cleaned_sutter_water_temp11_16.csv')

# read in cleaned water temp data
sutter_water_temp <- read_csv('data-raw/sutter/cleaned_sutter_water_temp11_16.csv') %>%
  mutate(water_temp_c = (water_temp_f - 32) * 5/9) %>%
  group_by(month = month(date)) %>%
  summarise(mean_temp_c = mean(water_temp_c, na.rm = TRUE))

tibble(
  date = seq(as.Date('1980-01-01'), as.Date('2000-12-31'), by = 'month'),
  watershed = 'Sutter Bypass',
  monthly_mean_temp_c = rep(sutter_water_temp$mean_temp_c, times = 21)
) %>% write_rds('data-raw/sutter/sutter_bypass_water_temp_c.rds')
