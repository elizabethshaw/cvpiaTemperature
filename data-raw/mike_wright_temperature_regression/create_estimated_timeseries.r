
### use outputs of regression estimating water temperature in a stream using equilibrium temperature and river mile to generate time series of temperatures for each affected stream

# load needed libraries

require(plyr)
require(dplyr)
require(lubridate)
require(tidyr)
require(purrr)
require(ggplot2)


# inputs: regression parameters and reach lengths (/2 for river mile, assuming reach begins at confluence) for each river, monthly equilibrium air temps. .csv format


# create tbl_df of air temperatures, to be converted to water temperatures using the parameters in regression_parameters.csv, as assigned in regression_assignments.csv
	# eq_temp was generated in GerberEQTEMP-61to17.xlsx from a 6-hourly time series in F to a monthly (average) time series, still in F since regression was calibrated using F
		# in this run, the entirety of years 1979-2001 are provided

eq_temp <- read_csv('data-raw/mike_wright_temperature_regression/eq_temp.csv') %>%
  mutate(date = mdy(date)) %>%
  filter(year(date) <= 2000)


# create tbl_df of rearing lengths. rearing lifestage will be used and the miles for the creek will be divided by two to obtain the river mile used in the calculations

cvpia_rearing_lengths <- read_csv('data-raw/mike_wright_temperature_regression/cvpia_rearing_lengths.csv')


# create tbl_df of regression assignments. this will allow us to identify which parameters each river should use
	# these assignments were taken from an email dated 04/26/19, which was sent to Mike Urkov among others

regression_assignments <- read_csv('data-raw/mike_wright_temperature_regression/regression_assignments.csv')


# create tbl_df of regression parameters. leaving in existing format
	# parameters were generated by Kirk Nelson; Final_21streamResults.xlsx contains the parameters and analysis of stream-by-stream comparisons

regression_parameters <- read_csv('data-raw/mike_wright_temperature_regression/regression_parameters.csv')

# add parameter value columns to regression_assignments, automatically using match as the key; this identifies the parameters used for each watershed to which this analysis is applied
watershed_parameters <- left_join(regression_assignments,regression_parameters)

# add river mile information  to watershed_parameters, automatically using watershed as the key; this puts river miles (which must be divided by two) in the same tbl_df as parameters
watershed_parameters_river_miles <- left_join(watershed_parameters,cvpia_rearing_lengths %>% filter(lifestage=='rearing',species=='fr')) %>% mutate(miles=miles/2)


# function for creating a juv_temp formatted long df of monthly temperatures from study period (Oct79-Dec99) for a given watershed

water_temps <- function(watershed_) {

	# filter watershed_parameters_river_miles to keep only the single row which has this watershed's information
	params <- filter(watershed_parameters_river_miles,watershed==watershed_)

	# for each date in eq_temp, generate a water temperature estimate in C using the monthly_mean_temp_f_air values in F from eq_temp and mm, bm, mb, bb, and miles from params
		#### NOTE that currently, the method takes input equilibrium temperatures in F and outputs water temperature estimates in C!
	water_temp_estimates <- mutate(eq_temp,monthly_mean_temp_c=(params$mm*monthly_mean_temp_f_air + params$bm)*params$miles + (params$mb*monthly_mean_temp_f_air + params$bb))

	# add column indicating which watershed the temperatures are from and arrange in order of juv_temp
	water_temp_estimates <- mutate(water_temp_estimates,watershed=watershed_) %>% select(date,watershed,monthly_mean_temp_c)


}


# map function across all of the watersheds in watershed_parameters_river_miles and combine in tbl_df

juv_temp_regression <-
  map(unique(watershed_parameters_river_miles$watershed),water_temps) %>% bind_rows

write_rds(juv_temp_regression, "data-raw/mike_wright_temperature_regression/juv_temp_regression.rds")

# create plot of all temperatures. reveals some likely problems with high or low temperatures
tiff(paste0('temps.tiff'), units="in", width=45, height=15, res=100)
	ggplot(juv_temp_regression,aes(x=date,y=monthly_mean_temp_c)) +
	geom_line(aes(col=watershed))
dev.off()

# save image and history
save.image()
savehistory()


# LOAD AND SAVE ----------------------

# load and save the juv_temp_regression binary and save it locally
# Mike Wright has run all of the code above to produce the juv_temp_regression
# rds within the .Rdata. I unpack this here and save it locally as an RDS
# mike_env <- new.env()
# load(file = "data-raw/mike_wright_temperature_regression/.RData", envir = mike_env)
#
# juv_temp_regression <- mike_env$juv_temp_regression %>%
#   filter(year(date) <= 2000)
#
# write_rds(juv_temp_regression, "data-raw/mike_wright_temperature_regression/juv_temp_regression.rds")
#
#
#
#




