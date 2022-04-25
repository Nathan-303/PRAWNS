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

#' @keywords
#' @export
#' @examples
#' create_prawns()
#' 

create_prawns <- function(raster_path,
                          shapefile_path,
                          data_path,
                          key_variable,
                          output_path=FALSE){
  #Read the raster file
  #Read the shapefile
  #Calculate the average for each polygon in the shapefile
  #Read in the additional data
  #Link the averages for each polygon with the additional data
  #Save the results if a filepath was specified
  #Return the resulting object
  prawns
}