#' Monthly Mean Water Temperature
#' @description A dataset containing the monthly mean water temperature in
#' celsius for each CVPIA watershed 1980-1999.
#'
#' @format dataframe with 7,440 rows and 3 variables
#' \describe{
#'  \item{date}{calendar date, monthly value 1980-1999}
#'  \item{watershed}{CVPIA watershed name}
#'  \item{monthly_mean_temp_c}{modeled monthly mean temperature in °C}
#'  }
#'
#' @details The following four methods were used to estimate the monthly mean water
#' temperature for the watersheds during 1980-1999:
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
#'
#' \strong{Additional Temperature Modeling} \cr
#' For streams without major dams, water temperature is highly correlated with air temperature.
#' For each watershed not included in the HEC-5Q model that had partial water temperature data,
#' a linear model was fitted to estimate water temperature as a function of air temperature.
#'
#' Generally, the air temperature record spans both the period of the CVPIA salmon life cycle model
#' and the complete period of record of available water temperature data. In the
#' cases where there were missing air temperature values between 1980-1999, we imputed the missing values using
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
#'  \strong{Temperature Surrogates} \cr
#'  For watersheds without representation within the HEC-5Q model and insufficient
#'  empirical data to generate a model, a nearby modeled stream's temperature estimates
#'  were selected to represent the watershed's monthly mean temperature.
#'
#' @section Watershed Modeling Details:
#' \itemize{
#'    \item \strong{Upper Sacramento River} HEC-5Q model output at COTTONWOOD CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Antelope Creek} Cow Creek temperatue used as surrogate
#'    \item \strong{Battle Creek} HEC-5Q model output at BATTLE CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Bear Creek} Cow Creek temperatue used as surrogate
#'    \item \strong{Big Chico Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage ID: \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=BIC}{BIC} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00046685/detail}{USC00046685}
#'    \item \strong{Butte Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from CDEC Gage ID: \href{http://cdec.water.ca.gov/cgi-progs/staMeta?station_id=BCD}{BCD} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00046685/detail}{USC00046685}
#'    \item \strong{Clear Creek} HEC-5Q model  output at IGO in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Cottonwood Creek} HEC-5Q model output at COTTONWOOD CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Cow Creek} HEC-5Q model  output at COW CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Deer Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11383500}{11383500} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00041715/detail}{USC00041715}
#'    \item \strong{Elder Creek} Thomes Creek temperatue used as surrogate
#'    \item \strong{Mill Creek} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11381500}{11381500} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00024216/detail}{USW00024216}
#'    \item \strong{Paynes Creek} Cow Creek temperatue used as surrogate
#'    \item \strong{Stony Creek} HEC-5Q model  output at STONY CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Thomes Creek} HEC-5Q model output at THOMES CR in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Upper-mid Sacramento River} HEC-5Q model output at STONY CREEK in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Sutter Bypass} TBD
#'    \item \strong{Bear River} TBD
#'    \item \strong{Feather River} American River temperatue used as surrogate
#'    \item \strong{Yuba River} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11421000}{11421000} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00024216/detail}{USW00024216}
#'    \item \strong{Lower-mid Sacramento River} HEC-5Q model output at KNIGHTS LDG in SSJB_SAC_Reference_062315/SAC/SAC_CL_TEMP.DSS
#'    \item \strong{Yolo Bypass} TBD
#'    \item \strong{American River} HEC-5Q model output at WILLIAM POND PARK in SSJB_AR_Reference_063015/AR/AR_CL_Temp.dss
#'    \item \strong{Lower Sacramento River} Measured mean monthly water temperature from  USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11447650}{11447650} with imputed missing values using \href{https://www.rdocumentation.org/packages/forecast/versions/8.1/topics/na.interp}{\code{forecast::na.interp}}
#'    \item \strong{Calaveras River} Mokelumne River temperatue used as surrogate
#'    \item \strong{Cosumnes River} Estimated mean monthly water temperature from a linear model fitted with water temperature data from USGS Gage ID: \href{https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=11335000}{11335000} and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USW00023271/detail}{USW00023271}
#'    \item \strong{Mokelumne River} Estimated mean monthly water temperature from a linear model fitted with water temperature data provided by EBMUD measured near Victor, CA and air temperature data from NOAA CDO Station Id: \href{https://www.ncdc.noaa.gov/cdo-web/datasets/GSOM/stations/GHCND:USC00045032/detail}{USC00045032}
#'    \item \strong{Merced River} HEC-5Q model output at SANTA FE BR in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS
#'    \item \strong{Stanislaus River} HEC-5Q model output at BLW MCHENRY BR in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS
#'    \item \strong{Tuolumne River} HEC-5Q model output at GEER ROAD BR in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS
#'    \item \strong{San Joaquin River} HEC-5Q model output at ABV TUOL in SSJB_SJR_Reference_062915/SJR/SJR_CL_TEMP.DSS
#' }
#'
#'
#'
#' @source
#' \itemize{
#'   \item HEC-5Q Model Output: Michael Wright \email{mwright@@usbr.gov}
#'   \item Data Wrangling and Additional Temperature Modeling: Sadie Gill  \email{sgill@@flowwest.com}
#'   \item Temperature Surrogate Selection: Mark Tompkins \email{mtompkins@@flowwest.com} and Mike Urkov \email{mike.urkov@@gmail.com}
#' }
#'
"juv_temp"

#' Monthly Accumulated Degree Days
#'
#' @description dio
#'
#' @format
#' \describe{
#'   \item{date}{calendar date, monthly value 1980-1999}
#'   \item{watershed}{CVPIA watershed name}
#'   \item{degdays}{monthly ATU in °C}
#' }
#'
#' @details
#' For watersheds with HEC-5Q modeled results, degree days was calculated by
#' summing the daily mean values of the month.
#'
#' For the other regions, degree days was caluclated by multiplying the monthly mean
#' temperature with the number of days in the month.
#'
#' For more details about the temperature modeling see \code{\link{juv_temp}}
#'
#' @source
#' \itemize{
#'   \item HEC-5Q Model Output: Michael Wright \email{mwright@@usbr.gov}
#'   \item Data Wrangling and Additional Temperature Modeling: Sadie Gill  \email{sgill@@flowwest.com}
#'   \item Temperature Surrogate Selection: Mark Tompkins \email{mtompkins@@flowwest.com} and Mike Urkov \email{mike.urkov@@gmail.com}
#' }
#'
"deg_days"
