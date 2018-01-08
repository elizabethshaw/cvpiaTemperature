-----
<img src="cvpia_logo.png" align="right" width="40%"/>

### Modeled Temperature Data for the CVPIA SIT Model

*This data package contains modeled temperature data for each of the watersheds within the CVPIA salmon life cycle model.*

#### Installation

``` r
# install.packages("devtools")
devtools::install_github("FlowWest/cvpiaTemperature")
```

#### Usage
This package provides flow related datasets to the [`cvpiaData`](https://flowwest.github.io/cvpiaData/) package.

``` r
# datasets within the package
data(package = 'cvpiaTemperature')

# explore temperature modeling metadata
?cvpiaTemperature::mean_monthly_temperature
```

#### About the Models
Heq5Q
Measured Values USGS CDEC
Linear Modeling
