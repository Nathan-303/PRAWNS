#' Outputs the shapefile as a graphic so it can be visually checked
#'
#' @param shapefile_path
#'@export
#

shapefile_checker <- function(shapefile_path){
  raw_shapefile <- st_read(shapefile_path)

  output <- ggplot(raw_shapefile)+geom_sf()

  output
}
