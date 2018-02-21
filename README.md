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
This package provides temperature related datasets to the [`cvpiaData`](https://flowwest.github.io/cvpiaData/) package.

``` r
# datasets within the package
data(package = 'cvpiaTemperature')

# explore temperature modeling metadata
?cvpiaTemperature::juv_temp
```

   
<style>.logo{margin-top: 40px;}</style>
<div class = 'logo'>Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="TransLogoTreb.png" width="150px"/></div>
