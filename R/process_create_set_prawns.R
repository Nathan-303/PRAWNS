#' A function for creating the data source for PRAWNS functions``
#'
#' This function takes a raster and a shapefile, works out the average value for
#' each polygon in the shapefile then uses an identifier present in the
#' shapefile to link with other data sources. This the pollution for areas to be
#' worked out then linked to statistics associated with those areas. Refer to +
#' the vignette setting_up_data sources to obtain the correct input files and
#' structure them for compatibility
#'
#' @param raster_path The filepath for the raster that is to be used. This is
#' compatible with folders containing multiple raster layers, mutually exclusive with tif path
#'
#' @param tif_path The filepath for the tif file used to input modelled data, mutually sxclusive with raster_path
#'
#' @param shapefile_path The filepath for the shapefile to be used
#'
#' @param output_path The filepath to output to. Defaults to FALSE
#'
#' @param pollutant the pollutant being investigated, this should be in quotes and correspond with its name in the data
#'
#' @param pollutant_data_name what the pollutant is referred to as in the tables
#'
#' @param year the year the data is from, used to standardise column names, currently doesnt work for years before 2000
#'
#' @param is_raw whether or not to output a csv only containing the LSOAs mapped to the emissions
#'
#' @keywords data
#'
#' @export
#'
#' @examples
#' process_create_prawns(
#'   raster_path="The folder containing the rasters",
#'   shapefile_path="Where the shapefiles are",
#'   pollutant_data_name="nox",
#'   year="2019",
#'   pollutant="NOx")

process_create_set_prawns <- function(raster_path="undefined",
                          tif_path="undefined",
                          csv_coordinates_path="undefined",
                          shapefile_path,
                          output_path="undefined",
                          pollutant_data_name,
                          year,
                          pollutant){

# Calculate the average pollution for each area ----------------------------
  #names that the LSOA column can have
  potentials <- c("LSOA01CD","LSOA11CD","LSOA21CD")

#Three chained if statements, which trigger if there is a path to the appropriate file in the function call
  #If its'a raster
  #Create a list of all the raster files present in the folder specified by raster_path
  filelist <- grep('\\.asc$', unzip((raster_path), list=TRUE)$Name,
                   ignore.case=TRUE, value=TRUE)


print("vect call")
#Read the shapefile
LSOA_shapefile <- vect(shapefile_path)
LSOA_field <- intersect(potentials,as.character(names(LSOA_shapefile)))
#potentials%in%names(LSOA_shapefile)
LSOA_shapefile <- LSOA_shapefile %>% tidyterra::rename("LSOA"=LSOA_field)

output <- tibble(LSOA=LSOA_shapefile$LSOA)

print("still alive")

index <- c(1:length(LSOA_shapefile))

transient <- sf::st_as_sf(LSOA_shapefile[index])
for(source_number in 1:length(filelist)){

  #unzip only the layer being extracted to minimise memory use, skip offshore
  transient_raster <- unzip(zipfile = raster_path,
                            files=filelist[source_number],
                            exdir=here::here(tempdir())) %>%
    rast()%>%
    terra::subst(NA,0)


  pollution_mean <- exact_extract(transient_raster,transient,'mean') %>%
    tibble() %>% signif(digits=4)

  unlist(here::here("tempdir"))
  #rename the column for smoother binding, dplyr rename not used because it was being awkwa
  colnames(pollution_mean)[1]=filelist[source_number]

  output <- pollution_mean %>% bind_cols(output)
}


    renamer <- function(data,last_two_digits_year,pollutant_data_name){

      NamedList <- c("Agricultural","Domestic combustion","Energy production",
                     "Industrial combustion","Industrial production","Natural",
                     "Offshore","Other transport and mobile machinery","Road transport","Solvents","Tot_area","Total",
                     "Waste treatment and disposal")

      Nmdlst <- paste0(c("agric","domcom","energyprod","indcom","indproc","nature","offshore","othertrans","roadtrans","solvents","totarea","total","waste"),
                       pollutant_data_name,last_two_digits_year,
                       ".asc")

      core <- tibble(Output=NamedList,Match=Nmdlst)


      for(Index in c(1:17)){
        #if the column name matches any of the renamable vectors then rename it
        if(colnames(data)[Index] %in% core$Match){
          track <- colnames(data)[Index]

          replace <- core %>% filter(Match==track)

          colnames(data)[Index] <- replace$Output
          #Break the loop if there's no match, adds efficiency
        }#ermoved to prevent bad triggers else{break}
        #Close the for loop
      }
      print("Renamed")
      #Output data
      data
      #Close the function

    }

  prawns <- renamer(
    data=output,
    last_two_digits_year=year-2000,
    pollutant_data_name = pollutant_data_name
  )
  prawns <- prawns %>% mutate("Point sources"=Total-Tot_area)

prawns
  }


