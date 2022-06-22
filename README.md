
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
## Input data
This package takes inputs with a particular data structure, and is designed 
to work with specified data sources. A full explanation of what to use is included in the
setting_up_data_sources vignette

## Outputs
This package produces csv files linking LSOAs to average emissions, as well as a variety of
graphs using this data. Should further data processing be required, or additional data be used,
the PRAWNS.csv object from ``create_prawns()`` or as a side effect of ``bulk_processor()`` can
be further processed as it links all the data back to LSOA code.

## Example

Create the standardised PRAWNS object used for all the graphing and analysis 
functions
``` r
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
```

Get a graphical summary of the distribution of a pollutant in a city
``` r
city_summary(prawn_path = "Outputs/create_prawns_1_0_0_test.csv",
                   shape_path = "Data/2011_LSOA_shapefile_20m_generalised",
                   targets = "London",
                   output_path= "Outputs/geographic_summary1_0_0_test",
                    pollutant="NOx"
                  )
```

Calculate the difference between the most and least deprived deciles
``` r
stat_wrangler(prawn_path = prawn_path,deciles=c(1,10))
```
