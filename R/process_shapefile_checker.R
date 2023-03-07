#' Outputs the shapefile as a graphic so it can be visually checked
#'
#' @param shapefile_path the path to the shapefiles that are being checked
#'@export
#

process_shapefile_checker <- function(shapefile_path){
  raw_shapefile <- st_read(shapefile_path)

  output <- ggplot(raw_shapefile)+geom_sf()

  output
}
