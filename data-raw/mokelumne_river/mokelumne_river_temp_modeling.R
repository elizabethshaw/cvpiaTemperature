library(rnoaa)
library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(caret)

token <- Sys.getenv("token")

moke_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11325500', parameterCd = '00010', statCd = c('00001', '00002'),
                          startDate = '1966-01-01', endDate = '1975-12-31')

glimpse(moke_water_temp)

water_temp <- moke_water_temp %>%
  select(date = Date, max_tempC = X_00010_00001, min_tempC = X_00010_00002) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(max_tempC = mean(max_tempC, na.rm = TRUE), min_tempC = mean(min_tempC, na.rm = TRUE),
            mean_water_tempC = mean(c(max_tempC, min_tempC))) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  mutate(mean_water_tempF = mean_water_tempC * 9/5 + 32) %>%
  select(date, mean_water_tempF)

# temp at lodi
test2 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
                     startdate = '1994-08-01', enddate = '2003-12-31', limit = 120, token = token)

test1 <- rnoaa::ncdc(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
                     startdate = '2004-01-01', enddate = '2013-12-31', limit = 120, token = token)



lodi_air_temp <- test2$data %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, avg_tempC = value)

lodi_air_temp %>%
  group_by(year = year(date)) %>%
  summarise(n())

View(lodi_air_temp)
training <- water_temp %>%
  left_join(s_air_temp)

cor(training$mean_water_tempF, training$mean_air_tempF)

temp_model_moke <- lm(mean_water_tempF ~ mean_air_tempF, data = training)
summary(temp_model_moke)

bad_water_temps <- c(64.4, 68)
b0 <- temp_model_moke$coefficients[[1]]
b1 <- temp_model_moke$coefficients[[2]]

air_temp_thersholds <- (bad_water_temps - b0) / b1

training %>%
  ggplot(aes(x = mean_air_tempF, y = mean_water_tempF)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_point(pch = 1) +
  theme_minimal() +
  labs(x = 'monthly mean air temperature (°F)',
       y = 'monthly mean water temperature (°F)') +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 64.4, linetype = 2, color = 'red') +
  geom_hline(yintercept = 68, linetype = 2, color = 'red') +
  annotate('text', x = 50, y = 64.4 + 1, size = 6,
           label = 'Above 18°C') +
  annotate('text', x = 50, y = 68 + 1, size = 6,
           label = 'Above 20°C')


# temperature data from Mike Urkov---------------------
frandy <- read_csv('data-raw/mokelumne_river/Frandy15min.csv')

frandy_water_temp <- freddy %>%
  mutate(date = as.Date(date, '%H:%M:%S %m/%d/%Y'),
         water_tempF = water_tempC * 9/5 + 32) %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(mean_water_tempF = mean(water_tempF, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year, month, '01', sep = '-'))) %>%
  select(date, mean_water_tempF)

#Lodi
lodi_air_temp <- climate_data(datasetid = 'GSOM', stationid = 'GHCND:USC00045032', datatypeid = 'TAVG',
                                  startdate = '1994-08-01', enddate = '2003-12-31', limit = 120)

air_temp <- lodi_air_temp$data %>%
  mutate(date = as_date(ymd_hms(date))) %>%
  select(date, mean_air_tempF = value)

validate <- left_join(air_temp, freddy_water_temp)

pred_values <- predict(temp_model_moke, validate)

pred_above_18 <-  pred_values > bad_water_temps[[1]]
above_18 <- validate$mean_water_tempF > bad_water_temps[[1]]

xtab18 <- table(pred_above_18, above_18)

confusionMatrix(xtab18)

pred_above_20 <-  pred_values > bad_water_temps[[2]]
above_20 <- validate$mean_water_tempF > bad_water_temps[[2]]

xtab20 <- table(pred_above_20, above_20)

confusionMatrix(xtab20)

temp_model_moke %>% broom::augment() %>% View()
