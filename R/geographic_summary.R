#' A function for creating a graphic summarising a city
#'
#' This function takes a prawns CSV and produces a summary of the geographic areas
#' matching an inputted parameter
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param shape_path The filepath for the shapefile that is to be used
#'
#' @param column_targeted The name of the column to be searched. It is reccomended
#' to use somehtign like city or county/UA that is geographically specific, but
#' any variable can be used
#'
#' @param targets The name of the thing you're trying to isolate e.g. "Manchester"
#' can also be a vector of valid values
#'
#' @param key_variable The variable that is common between the shapefile and csv
#' files specified in data_path, defaults to LSOA19CD
#'
#' @param output_path The filepath to output to, a folder will be created at
#' this location which contains all the graphs produced by this code. Defaults
#' to FALSE

#' @keywords
#' @export
#' @examples
#' geographic_summary()
#'
geographic_summary <- function(prawn_path,
                               shape_path,
                               column_targeted,
                               targets,
                               key_variable="LSOA19CD",
                               output_path=FALSE){

  #Reads in the demographic and pollution data
  raw_data <- read.csv(file=prawn_path,
                             row.names=1,
                             check.names=FALSE) %>% tibble()

  #Reads in the shapefiles
  raw_shapefiles <- vect(file=shape_path)

  #Iterates separately for each entry in targets


  #Takes the subset of the data where the value in column_targeted matches target
  filtered_data <- subset(superstack, subset= superstack$column_targeted == target)

  filtered_shapefiles <- subset(raw_shapefiles,subset = raw_shapefiles$key_variable %in% filtered_data$key_variable )

  #Stitch the data you want onto the shapefile, this section has assumptions about the column names, the continous and factored deciles are for graphing capacity
  stitched_shapefile <- filtered_shapefiles
    stitched_shapefile$LSOA_Decile <- filtered_data$Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs. %>% factor()
    stitched_shapefile$NOx <- filtered_data$Total
    stitched_shapefile$Cont_decile <- filtered_data$Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.

# Graph creation ----------------------------------------------------------


if (output_path=TRUE){

    if (file.exists(output_path)) {

      ggsave(filename=paste0(output_path,"/Nox in ",targets[index]),
             plot=last_plot(),
             units = "mm",height = 160,width=160,
             device="png")

    } else {

      dir.create(output_path)

      ggsave(filename=paste0(output_path,"/Nox in ",targets[index]),
             plot=last_plot(),
             units = "mm",height = 160,width=160,
             device="png")
    }
  }
}


