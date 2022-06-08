
# PRAWNS


This package allows pollution data in a raster format for England to be linked
with LSOAs, geographcal areas with associated national statistics such as Index
of Multiple Deprivation (IMD) score. It currently accepts data in the format
provided by the National Atmospheric Emissions Inventory (NAEI) for emissions,
and specific publicly available data files for the national statistics.
<!-- badges: start -->
<!-- badges: end -->



## Installation

You can install the development version of PRAWNS from [GitHub](https://github.com/) with:



``` r
# install.packages("devtools")
devtools::install_github("Nathan-303/PRAWNS")
```

##Data sources

County lookup 2019:
https://geoportal.statistics.gov.uk/datasets/ons::local-authority-district-to-county-april-2019-lookup-in-england/about

2019_LSOA_stats:
https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019 (File 7)

LSOA_Lookup (2011)
https://geoportal.statistics.gov.uk/datasets/dc0b24da0880417abc979c705bce3fde/explore

Shapefiles
https://data.cambridgeshireinsight.org.uk/dataset/output-areas/resource/3bc4faef-38c7-417d-88a9-f302ad845ebe

2019 pollution data
https://naei.beis.gov.uk/data/map-uk-das

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PRAWNS)
## basic example code
```
#Create the standardised PRAWNS object used for all the graphing and analysis 
#functions
create_prawns(raster_path= "Data/NOx_rasters_2019",
              shapefile_path = "Data/2011_LSOA_shapefile_20m_generalised",
              data_path= list.files("Data/LSOA_statistics"),
              key_variable = "LSOA19CD",
              key_variable_aliases =c("LSOA.code..2011.","?..LSOA11CD"),
              output_path="Outputs/create_prawns_1_0_0_test.csv",
              pollutant_data_name = "nox",
              year=2019,
              pollutant="NOx")
}

#Get a graphical summary of the distribution of a pollutant in a city
city_summary(prawn_path = "Outputs/create_prawns_1_0_0_test.csv",
                   shape_path = "Data/2011_LSOA_shapefile_20m_generalised",
                   targets = "London",
                   output_path= "Outputs/geographic_summary1_0_0_test",
                    pollutant="NOx"
                  )

#Calculate the difference between the most and least deprived deciles
stat_wrangler(prawn_path = prawn_path,deciles=c(1,10))
