

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
  mutate(num_days = days_in_month(date),
         degdays = monthly_mean_temp_c * num_days,
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

devtools::use_data(deg_days, overwrite = TRUE)

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


