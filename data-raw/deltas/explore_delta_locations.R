library(tidyverse)
library(lubridate)
library(CDECRetrieve)
library(rnoaa)


# compare different south delta temps to see which is most appropriate representation

# middle river at tracy blvd
middle_river <- cdec_query(stations = 'MTB', sensor_num = '25', dur_code = 'E',
                           start_date = '2002-10-30', end_date = '2017-12-31')

glimpse(middle_river)

mr <- middle_river %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(mean_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  filter(mean_temp_f < 100) %>% #remove errors
  mutate(location = 'middle river')

ggplot(mr, aes(x = date, y = mean_temp_f)) +
  geom_col() +
  labs(title = 'middle river')


# old river at bacon island
old_river <- cdec_query(stations = 'OBI', sensor_num = '25', dur_code = 'E',
                           start_date = '2008-02-27', end_date = '2017-12-31')

glimpse(old_river)

or <- old_river %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(mean_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  filter(mean_temp_f < 100) %>%  #remove errors
  mutate(location = 'old river')

ggplot(or, aes(x = date, y = mean_temp_f)) +
  geom_col() +
  labs(title = 'old river')

# prisoners point san joaquin river
prisoners_point <- cdec_query(stations = 'PPT', sensor_num = '25', dur_code = 'H',
                        start_date = '2006-03-02', end_date = '2017-12-31')

glimpse(prisoners_point)

pp <- prisoners_point %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(mean_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  filter(mean_temp_f < 100) %>% #remove errors
  mutate(location = 'san joaquin river')

ggplot(pp, aes(x = date, y = mean_temp_f)) +
  geom_col() +
  labs(title = 'san joaquin river')

# north delta
emmaton <- cdec_query(stations = 'EMM', sensor_num = '25', dur_code = 'H',
                              start_date = '1999-02-23', end_date = '2017-12-31')

glimpse(emmaton)

ee <- emmaton %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(mean_temp_f = mean(parameter_value, na.rm = TRUE)) %>%
  filter(mean_temp_f < 100) %>% #remove errors
  mutate(location = 'emmaton')

annotate_years <- function(year) {
  annotate("rect", xmin = ymd(paste0(year,'-06-01')), xmax = ymd(paste0(year, '-10-01')),
           ymin = -Inf, ymax = Inf, alpha = 0.2)
}

filter(mr, year(date) >= 2010) %>%
  bind_rows(filter(or, year(date) >= 2010)) %>%
  bind_rows(filter(pp, year(date) >= 2010)) %>%
  bind_rows(filter(ee, year(date) >= 2010)) %>%
  mutate(mean_temp_c = (mean_temp_f - 32) * 5/9) %>%
  ggplot() +
  geom_line(aes(x = date, y = mean_temp_c, color = location)) +
  theme_minimal() +
  annotate_years(2010:2017) +
  geom_hline(yintercept = 20, linetype = 2) +
  geom_hline(yintercept = 18, linetype = 2)



