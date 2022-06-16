#' A function that performs everything in the package and stores the results in
#' a folder that is created with an appropriate procedurally generated name
#'
#' This function takes a prawns CSV and produces a summary of the geographic areas
#' matching an inputted parameter
#'@param raster_path The filepath for the raster that is to be used. This is
#' compatible with folders containing multiple raster layers
#'
#' @param shapefile_path The filepath for the shapefile to be used
#'
#' @param data_path The filepath to a folder containing the additonal data
#'
#' @param pollutant the pollutant being investigated, this should be in quotes and correspond with its name in the data
#'
#' @param pollutant_data_name what the pollutant is referred to as in the raw data
#'
#' @param year the year the data is from, used to standardise column names, currently doesnt work for years before 2000

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The name of the pollutant that's being examined, this is used in the graph names
#'
#' @param version_number the version of the package used, aids in reproducibility
#'
#' @keywords faceted, sources
#' @export
#' @examples
#' bulk_processor()
#'

bulk_processor <- function(raster_path,
                            shapefile_path,
                            data_path,
                            key_variable,
                            key_variable_aliases=FALSE,
                            pollutant_data_name,
                            year,
                            pollutant){

#create the folder that everything goes in
dir.create(paste0(pollutant,"_emissions_in_",year,"_v",version))

#Create the prawns csv that the rest of the functions operate off
create_prawns(raster_path= raster_path,
              shapefile_path = shapefile_path,
              data_path= list.files(data_path),
              pollutant_data_name = pollutant_data_name,
              year=year,
              pollutant=pollutant)

#Make and save a graph showing a summary of the pollutants
long_chunk <- read.csv(file=prawn_path,
                       row.names=1,
                       check.names=FALSE) %>%
              tibble() %>%

              mutate(point_sources=Total-Total_no_points)%>%

              pivot_longer(
    cols=c("Agricultural","Domestic combustion","Energy production",
           "Industrial combustion","Industrial production","Natural",
           "Offshore","Other transport and mobile machinery","Road transport","Solvents","Total"
           ,"Waste treatment and disposal","point_sources"),
    names_to = "Emission_source",
    values_to = "emissions")
  long_chunk$Emission_source <- factor(long_chunk$Emission_source)

  long_chunk <- long_chunk %>% mutate(Emission_source=fct_reorder(Emission_source,emissions,mean,.desc=TRUE))

  city_sources <- Decile_vs_emission_by_variable(
    active_stack = long_chunk,
    chosen_decile = IMD,
    chosen_grouping = Emission_source,
    xaxis = "IMD Decile",
    yaxis = "NOx emissions",
    title = paste0("Source breakdown for ",targets),
    chosen_variable = emissions,
    Pollutant = "NOx"

)+
  geom_quantile(quantiles=0.5,linetype=2)
}
