-----

Hi its me Lizzy!

<img src="cvpia_logo.jpg" align="right" width="40%"/>

### Modeled Temperature Data for the CVPIA SIT Model

*This data package contains modeled temperature data for each of the watersheds within the CVPIA salmon life cycle model.*

#### Installation

``` r
# install.packages("devtools")
devtools::install_github("FlowWest/cvpiaTemperature")
```

#### Usage
This package provides temperature related datasets to the [`cvpiaData`](https://flowwest.github.io/cvpiaData/) package.

``` r
# datasets within the package
data(package = 'cvpiaTemperature')

# explore temperature modeling metadata
?cvpiaTemperature::juv_temp
```

#### About the Models
Temperature inputs to the CVPIA Decision Support Model (DSM) were developed using one of the following methods:    
1. HEC5Q water temperature model
2. Measured water temperatures
3. Correlation between measured air and water temperatures
4. Water temperature from the closest, most hydrologically and geomorphically similar watershed with available data. 

The HEC5Q model accepts flow inputs from the CALSIM II water resources system operations planning model. Watershed specific methods are detailed on the Reference tab. 

   
<style>.logo{margin-top: 40px;}</style>
<div class = 'logo'>Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="TransLogoTreb.png" width="150px"/></div>

