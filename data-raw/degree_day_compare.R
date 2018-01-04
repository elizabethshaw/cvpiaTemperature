library(tidyverse)


# wrote these files from sit-model import
# write_rds(DD.Dry, "/home/sadie/Github/cvpiaTemperature/data-raw/jim_degday_dry.rds")
# write_rds(DD.Wet, "/home/sadie/Github/cvpiaTemperature/data-raw/jim_degday_wet.rds")

DD.Dry <- read_rds("/home/sadie/Github/cvpiaTemperature/data-raw/jim_degday_dry.rds")
DD.Wet <- read_rds("/home/sadie/Github/cvpiaTemperature/data-raw/jim_degday_wet.rds")

DD.Wet %>% 
# DD.Dry %>% 
  select(Watershed, yr1:yr20) %>% 
  gather(year, degday, -Watershed) %>% 
  mutate(year = as.numeric(sub('yr', '', year))) %>%
  filter(degday != 0) %>% 
  ggplot(aes(x = year, y = degday, color = Watershed)) +
  geom_line() +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1300))
