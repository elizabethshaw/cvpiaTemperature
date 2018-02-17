library(tidyverse)
library(readxl)
library(lubridate)

# clean water temp data from RST at Knights Landing calfish.org---------------------
which(letters == 'm')

y16 <- readxl::read_excel('data-raw/yolo/Knights_Landing_RST_Catch_Data_2016-2017.xlsx', skip = 7, col_names = FALSE, na = 'NA') %>%
  select(date = X__1, water_temp_f = X__12) %>%
  group_by(date) %>%
  summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE)) %>%
  filter(!is.na(water_temp_f))

  ggplot(aes(date, water_temp_f)) +
  geom_col()

y17 <- readxl::read_excel('data-raw/yolo/Knights_Landing_RST_Catch_Data_2017-2018.xlsx', skip = 7, col_names = FALSE, na = 'N/A') %>%
    select(date = X__1, water_temp_f = X__13) %>%
    group_by(date) %>%
    summarise(water_temp_f = mean(water_temp_f, na.rm = TRUE)) %>%
    filter(!is.na(water_temp_f))

bind_rows(y16, y17) %>%
  mutate(water_temp_c = (water_temp_f - 32) * 5/9) %>%
  group_by(month = month(date)) %>%
  summarise(mean_temp_c = mean(water_temp_c, na.rm = TRUE))

ggplot(aes(date, water_temp_f)) +
  geom_col()
