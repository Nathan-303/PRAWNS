
# PRAWNS

<!-- badges: start -->
<!-- badges: end -->


Roadmap:

I won't extend the roadmap more than 1 minor version beyond the WIP branch, features planned beyond that will go in planned features

The current target build will appear at the top of this file, 

Patches (0.0.x) will be tagged on github and recorded in here


	  
0.0.1 (uploaded 6/6/22)

  create_prawns works
  
  data source for city_summary works
  
0.0.2 (uploaded 6/6/22)  
	
	got the following graphs working in city_summary
	  Decile_distribution
	  Pollutant_distribution
	  City_histogram
	  City_freq

0.0.3 (uploaded 7/6/22)

  stat_wrangler
    calculates the flat and % differences between specified deciles
    complete and fully functional
	
	Renamed the long IMD decile column to IMD within create_prawns

0.0.4 (uploaded 8/6/22)

	Ported decile_vs_emission_by_variable into the package
    Corrected the issue arising from IMD as a factor
    
  A new and exciting error in the NOx emissions with trend lines
    
0.1.0 (Uploaded 8/6/22)

	create_prawns (complete)
		Reads the shapefile and creates the PRAWNS object as a csv file
	 	Porting from an old project of mine where it's a collection of scripts
		Fully self contained
		-The input files for national statistics can't be changed in the function call

	city_summary
		Creates a single page spread of graphs providing information on a geographic area identified by variables in the function input
		Porting from an old project where it's a collection of scripts
		Requires a PRAWNS csv file for easy operation as this lets me standerdise the input
		
	stat_wrangler (complete)
	  Calculates the flat and % difference between the mean and median points for deciles
	  Calculates the flat and % difference between the intercept of the regrassion lines plotted for all data and the median for each        decile

1.0.0

  All functionality from pollutant processing hub ported as functions
  
  
		
Planned features:

	Input files can be changed within the function call

	Take pollutants other than NOx
	
	Accepts data from TIF format (relevant for modelled data from some sources)
	
	Source summary ported in
	
	Averages with and without london tracked against the city
	
	Each graph function within summary can be called separately
	
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

2019 polution data
https://naei.beis.gov.uk/data/map-uk-das

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(PRAWNS)
## basic example code
```

