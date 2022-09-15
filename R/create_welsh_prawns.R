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

#' @keywords data
#' @export

#'
#' @examples
#' create_prawns()
#'

create_welsh_prawns <- function(raster_path=FALSE,
                          tif_path=FALSE,
                          csv_coordinates_path=FALSE,
                          shapefile_path,
                          output_path=FALSE,
                          pollutant_data_name,
                          year,
                          pollutant,
                          is_raw=FALSE){

# Calculate the average pollution for each area ----------------------------

#Three chained if statements, which trigger if there is a path to the appropriate file in the function call
  #If its'a raster
  if(raster_path!=FALSE){
  #Create a list of all the raster files present in the folder specified by raster_path
  filelist <- list.files(raster_path,
    pattern = ".asc",
    full.names = TRUE)

  #Store the rasters as a stack
  source_stack <- rast(filelist[1])
  for(index in 2:length(filelist)){
    source_stack <- c(source_stack,rast(filelist[index]))
  }
  }
  #If its a tif
  if(tif_path!=FALSE){
    source_stack <- read_stars(tif_path) %>% rast()

  }
  #If it's a csv
  if(csv_coordinates_path!=FALSE){
    csv_raster <- read.csv(file=csv_coordinates_path,skip = 5,
                           row.names=1,
                           check.names=FALSE) %>% tibble()

    base_raster <- rasterFromXYZ(csv_raster,crs="OSGB")

    source_stack <- rast(base_raster)
  }
  #Read the shapefile
  LSOA_shapefile <- vect(shapefile_path)

  #Calculate the average for each polygon in the shapefile
  index <-  c(1:length(LSOA_shapefile))
  transient <- sf::st_as_sf(LSOA_shapefile[index])
  pollution_mean <- exact_extract(source_stack,transient,'mean')
  #Output the results as a tibble containing the indexed position, the pollution mean and the LSOA code, a property from the shapefile that enables binding on LSOA statistics
  output <- tibble(poll_mean=pollution_mean,
                   Data_Zone=DZ_shapefile$DataZone
                   ) %>% unnest(poll_mean)
  #output a csv wit the minimum processing done if is.raw=true
  if (is_raw==TRUE){
    if(output_path!=FALSE){
    write.csv(file=output_path,x = output
              )}
    output
  }
  else{

# Combine the pollution means with the additional data --------------------

link <- read.csv("Data/wales_lookup.csv",skip = 2)
refined_chunk <- read.csv("Data/WIMD.csv") %>% tibble() %>%
  #make a column with the deprivation decile
  mutate(IMD=ntile(WIMD,10)) %>% inner_join(link,by=("LSOA.Name"))

  prawns <- inner_join(output,refined_chunk,by="LSOA.Name")


  renamer <- function(data,last_two_digits_year,pollutant_data_name){

# Rename the columns for readability --------------------------------------


    NamedList <- c("Agricultural","Domestic combustion","Energy production",
                   "Industrial combustion","Industrial production","Natural",
                   "Offshore","Other transport and mobile machinery","Road transport","Solvents","Total"
                   ,"Total_no_points","Waste treatment and disposal")

    Nmdlst <- paste0("mean.",c("agric","domcom","energyprod","indcom","indproc","nature","offshore","othertrans","roadtrans","solvents","total","totarea","waste","pntsrc"),pollutant_data_name,last_two_digits_year)

    tracer <- colnames(data) %in% Nmdlst

    #Finds the first position where matches start
    starter <- detect_index(tracer,is_true)-1

    for(index in 1:length(tracer)){
      if(tracer[index] == TRUE){
        colnames(data)[index] <- NamedList[index-starter]
      }}

    data
  }

  prawns <- renamer(
    data=prawns,
    last_two_digits_year=year-2000,
    pollutant_data_name = pollutant_data_name
  ) %>% mutate("Point sources"=Total-Total_no_points)
  #Return the resulting object

# Output the results ------------------------------------------------------
if (output_path!=FALSE){
  write.csv(prawns,
            file=output_path)
}


prawns
  }
}

