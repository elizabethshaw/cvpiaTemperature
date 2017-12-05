library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(rnoaa)

deer_water_temp <- dataRetrieval::readNWISdv(siteNumbers = '11383500', parameterCd = '00010',
                                           startDate = '1998-10-05', endDate = '2017-11-10', statCd = c('00001', '00002', '00008'))

deer_flow <- dataRetrieval::readNWISdv(siteNumbers = '11383500', parameterCd = '00060',
                                       startDate = '1998-10-05', endDate = '2017-11-10')


# air temp from corning, ca
deer_air_temp_max <- read_csv('data-raw/deer_creek/corning_ca_air_temp.csv')


glimpse(deer_temp_max)

# lots of missing median values from gage, use mean of min and max water temp to approximate median water temp
glimpse(deer_water_temp)
deer_water_temp %>%
  select(date = Date, temp_c_max = X_00010_00001,
         temp_c_min = X_00010_00002, temp_c_med = X_00010_00008) %>%
  filter(!is.na(temp_c_med)) %>%
  mutate(mean_min_max = (temp_c_min + temp_c_max)/2,
         dist_mean_med = abs(mean_min_max - temp_c_med),
         dist_max_med = abs(temp_c_max - temp_c_med),
         dist_min_med = abs(temp_c_min - temp_c_med)) %>%
  select(dist_mean_med, dist_min_med, dist_max_med) %>%
  gather(dist_type, dist) %>%
  ggplot(aes(x = dist, color = dist_type)) +
  geom_density()

dt <- deer_water_temp %>%
  select(date = Date, temp_c_max = X_00010_00001,
         temp_c_min = X_00010_00002) %>%
  mutate(temp_c_approx_med = (temp_c_min + temp_c_max)/2,
         water_temp_f = temp_c_approx_med * 9/5 + 32) %>%
  select(date, water_temp_f)

da <- deer_air_temp_max %>%
  select(date = DATE, air_temp_f_max = TMAX,
         air_temp_f_min = TMIN, air_temp_f_mean = TAVG)

df <- deer_flow %>%
  select(date = Date, flow_cfs = 'X_00060_00003')


deer_creek <- da %>%
  left_join(dt) %>%
  left_join(df) %>%
  mutate(month = month(date))


glimpse(deer_creek)
dcc <-  deer_creek %>%
  mutate(year = year(date)) %>%
  group_by(year, month) %>%
  summarise(flow_cfs = mean(flow_cfs, na.rm = TRUE),
            air_temp_f_mean = mean(air_temp_f_mean, na.rm = TRUE),
            water_temp_f = mean(water_temp_f, na.rm = TRUE)) %>%
  ungroup()

water_temp_model <- lm(water_temp_f ~ flow_cfs + air_temp_f_mean + month, data = deer_creek)

deer_cr <- dcc %>%
  filter(!is.na(water_temp_f))

set.seed(655)
index <- sample(1:216)
m1 <- lm(water_temp_f ~ air_temp_f_mean, data = deer_cr[index[1:190], ])
summary(m1)
pred <- predict(m1, deer_cr[index[191:216], ])
truth <- deer_cr[index[191:216], 5]$water_temp_f
xtab <- table(pred > 64.4, truth > 64.4)

confusionMatrix(xtab)

water_temp_model2 <- lm(water_temp_f ~ air_temp_f_mean, data = dcc)
summary(water_temp_model2)

bad_water_temps <- c(64.4, 68)
b0 <- water_temp_model2$coefficients[[1]]
b1 <- water_temp_model2$coefficients[[2]]

air_temp_thersholds <- (bad_water_temps - b0) / b1

tt <- broom::augment(water_temp_model2)
glimpse(tt)
library(caret)
ttt <- tt %>%
  select(water_temp_f, .fitted) %>%
  mutate(truth18c = factor(water_temp_f >= 64.4),
         truth20c = factor(water_temp_f >= 68),
         pred18c = factor(.fitted >= 64.4),
         pred20c = factor(.fitted >= 68))
truth <- pull(ttt, truth20c)
pred <- pull(ttt, pred20c)
xtab <- table(truth,pred)
confusionMatrix(xtab)

dcc %>%
  mutate(score = ca)
  filter(air_temp_f_mean >= air_temp_thersholds, water_temp_f >= bad_water_temps)


dcc %>%
  ggplot(aes(x = air_temp_f_mean, y = water_temp_f)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 68) +
  geom_hline(yintercept = 64.4)

dcc %>%
  filter(water_temp_f >= 64.4) %>%
  ggplot(aes(x = air_temp_f_mean, y = water_temp_f)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  geom_hline(yintercept = 68) +
  geom_hline(yintercept = 64.4)


subdc <- dcc %>%
  filter(water_temp_f >= 64.4)

m1 <- lm(water_temp_f ~ air_temp_f_mean, data = subdc)
summary(m1)

library(forecast)

tsdc <- deer_creek %>%
  mutate(year = year(date)) %>%
  group_by(year, month) %>%
  summarise(flow_cfs = mean(flow_cfs, na.rm = TRUE),
            air_temp_f_mean = mean(air_temp_f_mean, na.rm = TRUE),
            water_temp_f = mean(water_temp_f, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(year > 1998) %>%
  select(flow_cfs, air_temp_f_mean, water_temp_f) %>%
  ts(start = c(1999, 1), frequency = 12)

water_temp_model3 <- tslm(water_temp_f ~ air_temp_f_mean, data = tsdc)

water_temp_model3 %>%
  forecast(tibble(air_temp_f_mean = 54)) %>%
  autoplot()


dcc %>%
  ggplot(aes(x = air_temp_f_mean, y = water_temp_f)) +
  geom_point(pch = 1) +
  theme_minimal() +
  labs(x = 'monthly mean air temperature (°F)',
       y = 'monthly mean water temperature (°F)') +
  geom_smooth(method = 'lm', se = FALSE) +
  theme(text = element_text(size = 18)) +
  geom_hline(yintercept = 64.4, linetype = 2, color = 'red') +
  geom_hline(yintercept = 68, linetype = 2, color = 'red') +
  annotate('text', x = 50, y = 64.4 + 1, size = 6,
           label = 'Above 18°C') +
  annotate('text', x = 50, y = 68 + 1, size = 6,
           label = 'Above 20°C')

deer_creek %>%
  # filter(flow_cfs<2000) %>%
  ggplot(aes(x = flow_cfs, y = water_temp_f, alpha = .1, color = as.factor(month))) +
  geom_point() +
  scale_x_log10()

deer_creek %>%
  # filter(flow_cfs<2000) %>%
  ggplot(aes(x = flow_cfs, y = as.factor(month))) +
  geom_point() +
  scale_x_log10()

cor(deer_creek$water_temp_f, deer_creek$air_temp_f_mean, use = 'complete.obs')
