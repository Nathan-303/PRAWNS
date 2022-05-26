#' A function for creating the data source for PRAWNS functions``
#'
#' This function takes a raster and a shapefile, works out the average value for
#' each polygon in the shapefile then uses an identifier present in the
#' shapefile to link with other data sources. This the pollution for areas to be
#' worked out then linked to statistics associated with those areas.
#'
#' @param raster_path The filepath for the raster that is to be used. This is
#' compatible with folders containing multiple raster layers
#'
#' @param shapefile_path The filepath for the shapefile to be used
#'
#' @param data_path The filepath for all csv files which contain additional data
#' this data should have entries that correspond to polygons in the shapefile
#'
#' @param key_variable The variable that is common between the shapefile and csv
#' files specified in data_path
#'
#' @param output_path The filepath to output to. Defaults to FALSE
#'
#' @param key_variable_aliases Any alternate ways of namingf the key variable
#' that are used in the different inputted tables, not necessary if the data is
#' already nicely formatted so defaults to FALSE

#' @keywords data
#' @export

#'
#' @examples
#' create_prawns()
#'

create_prawns <- function(raster_path,
                          shapefile_path,
                          data_path,
                          key_variable,
                          key_variable_aliases=FALSE,
                          output_path=FALSE){

# Calculate the average pollution for each area ----------------------------


  #Create a list of all the raster files present in the folder specified by raster_path
  filelist <- list.files(raster_path,
    pattern = ".asc",
    full.names = TRUE)

  #Store the rasters as a stack
  source_stack <- rast(filelist[1])
  for(index in 2:length(filelist)){
    source_stack <- c(source_stack,rast(filelist[index]))
  }
  #Read the shapefile
  LSOA_shapefile <- vect(shapefile_path)

  #Calculate the average for each polygon in the shapefile
  index <-  c(1:length(LSOA_shapefile))
  transient <- sf::st_as_sf(LSOA_shapefile[index])
  pollution_mean <- exact_extract(source_stack,transient,'mean')
  #Output the results as a tibble containing the indexed position, the pollution mean and the LSOA code, a property from the shapefile that enables binding on LSOA statistics
  output <- tibble(FID=index,poll_mean=pollution_mean,LSOA11CD=LSOA_shapefile$LSOA11CD) %>% unnest(pollution_mean)
  #


# Read the additional data as a list of tibbles----------------------------------------------------------------

  additional_data <- list()
  for (count in c(1:length(data_path))){
    #Read the file as a tibble and hold it in a transition state whilst it's checked for errors
    ts <- read.csv(data_path[count],row.names=1) %>% tibble()
    #Check for a known formatting issue in the column names and correct it
    if (colnames(ts)[1]== "ï..LSOA11CD"||colnames(ts)[1]== "LSOA.code..2011."){
      colnames(ts)[1] <- "LSOA11CD"
    }
    #Store the transition state in the list of data
    additional_data[count] <- ts
  }

  #Create a tibble to collect all the additional data in
  additional_data_tibble <- additional_data[1]

  #If there's more than one table of additional data, combine them all
  if(length((additional_data)>1)){
    for (count in c(2:length(additional_data))){
      inner_join(additional_data_tibble,additional_data[count],by="LSOA11CD")
    }}

# Combine the pollution means with the additional data --------------------


  prawns <- inner_join(output,refined_chunk,by="LSOA11CD")
  #Link the averages for each polygon with the additional data
  #Save the results if a filepath was specified
  #Return the resulting object

# Output the results ------------------------------------------------------
if (output_path!=FALSE){
  write.csv(prawns,
            file=output_path)
}


prawns
}
