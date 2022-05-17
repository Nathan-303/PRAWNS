#' A function for creating a graphic summarising a city
#'
#' This function takes a prawns CSV and produces a summary of the geographic areas
#' matching an inputted parameter
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used. This is
#' compatible with folders containing multiple raster layers
#'
#' @param column_targeted The name of the column to be searched. It is reccomended
#' to use somehtign like city or county/UA that is geographically specific, but
#' any variable can be used
#'
#' @param target The name of the thing you're trying to isolate e.g. "Manchester"
#' can also be a vector of valid values 
#'
#' @param key_variable The variable that is common between the shapefile and csv
#' files specified in data_path
#'
#' @param output_path The filepath to output to. Defaults to FALSE

#' @keywords
#' @export
#' @examples
#' geographic summary()
#'