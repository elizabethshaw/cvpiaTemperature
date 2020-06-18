#' Monthly Mean Water Temperature
#' @description A dataset containing the monthly mean water temperature in
#' celsius for each CVPIA watershed 1979-1999.
#'
#' @format dataframe with 7,440 rows and 3 variables
#' \describe{
#'  \item{date}{calendar date, monthly value 1979-1999}
#'  \item{watershed}{CVPIA watershed name}
#'  \item{monthly_mean_temp_c}{modeled monthly mean temperature in °C}
#'  }
#'
#' @details The following four methods were used to estimate the monthly mean water
#' temperature for the watersheds during 1979-1999:
#'
#'
#' \strong{HEC-5Q} \cr
#' Temperature assignments were determined by Mike Wright using the input files
#' for the three HEC-5Q runs, i.e. AR_5QCS.dat and AR_5Q-CL.OUT.
#'
#'
#' \strong{Empirical Data} \cr
#' Only the Lower Sacramento had sufficient measured water temperature during
#' the period of the CVPIA salmon life cycle model. The few missing values were
#' imputed using \href{https://www.rdocumentation.org/packages/forecast/versions/8.1/topics/na.interp}{\code{forecast::na.interp}}.
#'
#'  \strong{Double Regression Modeling} \cr
#'  Data from many temperature gauges was collected from several CVPIA watersheds.
#'  In these watersheds, a double regression technique was employed to predict water
#'  temperature based on equilibrium temperature and the river mile of interest.
#'  Equilibrium temperature is the temperature a pool of water would approach if
#'  left in contact with the air for an infinite time, generally equivalent to the
#'  air temperature. The results were applied to both watersheds of origin and as
#'  surrogate data for ungauged watersheds.
#'
#'  \href{https://cvpiatemperature-r-package.s3-us-west-2.amazonaws.com/Temperature_Regression_Model_for_CVPIA_Streams.pdf}{Modeling Details}
#'
#' \strong{Additional Temperature Modeling} \cr
#' For streams without major dams, water temperature is highly correlated with air temperature.
#' For each watershed not included in the HEC-5Q model that had partial water temperature data,
#' a linear model was fitted to estimate water temperature as a function of air temperature.
#'
#' Generally, the air temperature record spans both the period of the CVPIA salmon life cycle model
#' and the complete period of record of available water temperature data. In the
#' cases where there were missing air temperature values between 1979-1999, we imputed the missing values using
#' \href{https://www.rdocumentation.org/packages/forecast/versions/8.1/topics/na.interp}{\code{forecast::na.interp}}
#' in order to have a complete air temperature dataset for water temperature estimation.
#'
#'  Air Temperature Data Source:
#'  \itemize{
#'    \item NOAA Climate Data Online \href{https://www.ncdc.noaa.gov/cdo-web/}{(CDO)} accessed using the \href{https://github.com/ropensci/rnoaa}{\code{rnoaa}} R package developed by \href{https://ropensci.org/}{rOpenSci}.
#'    }
#'
#'  Stream Water Temperature Data Sources:
#'  \itemize{
#'    \item DWR California Data Exchange Center \href{http://cdec.water.ca.gov/index.html}{(CDEC)} accessed using the \href{https://github.com/flowwest/cdecretrieve}{\code{CDECRetrieve}} R packaged developed by \href{http://www.flowwest.com/}{FlowWest}.
#'    \item USGS National Water Information System \href{https://waterdata.usgs.gov/nwis}{(NWIS)} accessed using the \href{https://github.com/USGS-R/dataRetrieval}{\code{dataRetrieval}} R package developed by \href{https://www.usgs.gov/}{USGS}.
#'  }
#'
#' @section Watershed Modeling Details:
#' \itemize{
#'    \item \strong{Upper Sacramento River} HEC-5Q model output at COTTONWOOD CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Antelope Creek} Regression method was fitted to data from in-river temperature gauges
#'    \item \strong{Battle Creek} HEC-5Q model output at BATTLE CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Bear Creek} Regression method was fitted to data from in-river temperature gauges
#'    \item \strong{Big Chico Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage ID: \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=BIC}{BIC} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00046685/detail}{USC00046685}
#'    \item \strong{Butte Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage ID: \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=BCD}{BCD} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00046685/detail}{USC00046685}
#'    \item \strong{Clear Creek} HEC-5Q model  output at IGO in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Cottonwood Creek} HEC-5Q model output at COTTONWOOD CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Cow Creek} HEC-5Q model  output at COW CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Deer Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11383500}{11383500} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00041715/detail}{USC00041715}
#'    \item \strong{Elder Creek} Regression method was fitted to data from Antelope River temperature gauges
#'    \item \strong{Mill Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11381500}{11381500} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00024216/detail}{USW00024216}
#'    \item \strong{Paynes Creek} Regression method was fitted to data from Antelope River temperature gauges
#'    \item \strong{Stony Creek} HEC-5Q model  output at STONY CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Thomes Creek} HEC-5Q model output at THOMES CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Upper-mid Sacramento River} HEC-5Q model output at STONY CREEK in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Sutter Bypass} Tisdale Rotary Screw Trap montly mean water temperature 2011-2017
#'    \item \strong{Bear River} Regression method was fitted to data from Deer Creek temperature gauges
#'    \item \strong{Feather River} Regression method was fitted to data from Deer Creek temperature gauges
#'    \item \strong{Yuba River} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11421000}{11421000} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00024216/detail}{USW00024216}
#'    \item \strong{Lower-mid Sacramento River} HEC-5Q model output at KNIGHTS LDG in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Yolo Bypass} Knights Landing Rotary Screw Trap montly mean water temperature 2004-2018
#'    \item \strong{American River} HEC-5Q model output at WILLIAM POND PARK in SSJB_AR_Reference_063015/AR/AR_CL_Temp.dss
#'    \item \strong{Lower Sacramento River} Measured mean monthly water temperature from  USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11447650}{11447650} with imputed missing values using \href{https://www.rdocumentation.org/packages/forecast/versions/8.1/topics/na.interp}{\code{forecast::na.interp}}
#'    \item \strong{Calaveras River} Regression method was fitted to data from Deer Creek temperature gauges
#'    \item \strong{Cosumnes River} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11335000}{11335000} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00023271/detail}{USW00023271}
#'    \item \strong{Mokelumne River} Estimated mean monthly water temperature from a linear model fitted with water temperature data provided by EBMUD measured near Victor, CA and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00045032/detail}{USC00045032}
#'    \item \strong{Merced River} HEC-5Q model output at SANTA FE BR in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS
#'    \item \strong{Stanislaus River} HEC-5Q model output at BLW MCHENRY BR in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS
#'    \item \strong{Tuolumne River} HEC-5Q model output at GEER ROAD BR in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS
#'    \item \strong{San Joaquin River} HEC-5Q model output at ABV TUOL in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS
#' }
#'
#'
#' @source
#' \itemize{
#'   \item HEC-5Q Model Output and Double Regression Temperature Modeling: Michael Wright \email{mwright@@usbr.gov}
#'   \item Data Wrangling and Additional Temperature Modeling: Sadie Gill  \email{sgill@@flowwest.com}
#' }
#'
"monthly_mean_temperature"

#' Rearing Stream Temperature
#' @description The 1979-1999 rearing temperature in °C
#' @format a 3 dimensional array [31 watersheds, 12 months, 21 years]
#' @details
#' Calculated using \code{\link{monthly_mean_temperature}}
#'
#'
"stream_temperature"

#' Degree Days
#'
#' @description The monthly accumulated degree days (celsius)
#'
#' @format dataframe with 7,812 rows and 3 variables
#' \describe{
#'   \item{date}{calendar date, monthly value 1979-1999}
#'   \item{watershed}{CVPIA watershed name}
#'   \item{degdays}{monthly ATU in °C}
#' }
#'
#' @details
#' For watersheds with HEC-5Q modeled results, the calculation for degree days
#' is the sum of the daily mean values for each month.
#'
#' For the other regions, the calculation for degree days is the monthly mean
#' temperature multiplied by the number of days in the month.
#'
#' For more details about the temperature modeling see \code{\link{juv_temp}}
#'
#' @source
#' \itemize{
#'   \item HEC-5Q Model Output and Double Regression Temperature Modeling: Michael Wright \email{mwright@@usbr.gov}
#'   \item Data Wrangling and Additional Temperature Modeling: Sadie Gill  \email{sgill@@flowwest.com}
#' }
#'
"deg_days"

#' Delta Temperature
#'
#' @description Rearing temperature in the North and South Delta
#'
#' @format dataframe with 504 rows and 3 variables
#' \describe{
#'   \item{date}{calendar date, monthly value 1980-1999}
#'   \item{watershed}{CVPIA watershed name}
#'   \item{monthly_mean_temp_c}{modeled monthly mean temperature in °C}
#' }
#'
#' @details
#' Water temperature in the Delta was modeled as a function of air temperature.
#'
#' North Delta was modeled using a sample of recent measured water temperature at
#' CDEC station \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=EMM}{EMM}
#' and NOAA CDO air temperature data from \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00040232/detail}{Antioch}.
#'
#' South Delta was modeled using a sample of recent measured water temperature at
#' CDEC station \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=MTB}{MTB}
#' and NOAA CDO air temperature data \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00023237/detail}{Stockton Airport}.
#'
#' Air Temperature Data Source:
#'  \itemize{
#'    \item NOAA Climate Data Online \href{https://www.ncdc.noaa.gov/cdo-web/}{(CDO)} accessed using the \href{https://github.com/ropensci/rnoaa}{\code{rnoaa}} R package developed by \href{https://ropensci.org/}{rOpenSci}.
#'    }
#'
#'  Stream Water Temperature Data Sources:
#'  \itemize{
#'    \item DWR California Data Exchange Center \href{http://cdec.water.ca.gov/index.html}{(CDEC)} accessed using the \href{https://github.com/flowwest/cdecretrieve}{\code{CDECRetrieve}} R packaged developed by \href{http://www.flowwest.com/}{FlowWest}.
#'  }
#'
#' @source
#' Data Wrangling and Additional Temperature Modeling: Sadie Gill  \email{sgill@@flowwest.com}
#'
"delta_temps"

#' Migratory Corridor Water Temperature
#'
#' @description The median proportion of days over 20°C per month
#'
#' @format dataframe with 300 rows and 4 variables
#' \describe{
#'   \item{order}{ordering of watersheds within salmon life cycle model}
#'   \item{watershed}{CVPIA watershed}
#'   \item{month}{integer representation of calendar month}
#'   \item{median_p20}{median proportion of temperature over 20°C}
#' }
#'
#' @details
#' Watersheds were assigned membership to one of the following three locations to
#' represent the migratory corridor temperature conditions. The data was summarised
#' by calculating the median proportion of days over 20°C for each month during the available period of record.
#'
#' Sacramento River, \href{http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=WLK}{Sacramento River Below Wilkins Slough}:
#' Upper Sacramento River, Antelope Creek, Battle Creek, Bear Creek, Big Chico Creek Butte Creek, Clear Creek, Cottonwood Creek,
#' Cow Creek, Deer Creek, Elder Creek, Mill Creek, Paynes Creek, Stony Creek, Thomes Creek, Bear River, Feather River, Yuba River
#' American River
#'
#' South Delta, \href{http://cdec.water.ca.gov/dynamicapp/staMeta?station_id=MOK}{Mokelumne River At San Joaquin River}:
#' Calaveras River, Cosumnes River, Mokelumne River
#'
#' San Joaquin River, \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11303500}{San Joaquin River Near Vernalis}:
#' Merced River, Stanislaus River, Tuolumne River
#'
#' NA (no spawning in these regions):
#' Upper-mid Sacramento River, Sutter Bypass, Lower-mid Sacramento River, Yolo Bypass, Lower Sacramento River, San Joaquin River
#'
#' @source Sadie Gill \email{sgill@@flowwest.com}
"prop_temp_over_20_migr_cor"
