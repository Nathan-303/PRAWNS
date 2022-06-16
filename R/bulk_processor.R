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
                           data_folder,
                           pollutant_data_name,
                           year,
                           pollutant,
                           iteration){

#store the procedural tag as a variable to save space and make the code clearer

proc_tag <- paste0(pollutant,"_emissions_in_",year,"_v",iteration)

prawn_path <- paste0(proc_tag,"/PRAWN.csv")
#create the folder that everything goes in
dir.create(path=paste0(pollutant,"_emissions_in_",year,"_v",iteration))

#Create the prawns csv that the rest of the functions operate off

create_prawns(
                raster_path= raster_path,
                shapefile_path = shapefile_path,
                data_path= list.files(data_folder),
                pollutant_data_name = pollutant_data_name,
                year=year,
                pollutant=pollutant,
                output_path = prawn_path)
#Make and save a graph showing a summary of the pollutants
source_breakdown <- source_summary(prawn_path=prawn_path,
                                   pollutant=pollutant,
                                   year=year)

  ggsave(filename= paste0(proc_tag,"/",pollutant," source summary.png"),
         plot=source_breakdown,
         device="png")

#Make and save a graph where the sources are all faceted
source_facets <- faceted_sources(prawn_path = prawn_path,
                                 pollutant=pollutant)

  ggsave(filename= paste0(proc_tag,"/",pollutant," faceted sources.png"),
         plot=source_facets,
         device="png")

#Make and save a graph showing IMD based inequality for each RUC code
RUC_breakdown <- RUC_IMD(prawn_path = prawn_path,
                         pollutant=pollutant)

  ggsave(filename= paste0(proc_tag,"/",pollutant," RUC breakdown.png"),
         plot=RUC_breakdown[[1]],
         device="png")

  ggsave(filename= paste0(proc_tag,"/",pollutant," RUC populationbreakdown.png"),
         plot=RUC_breakdown[[2]],
         device="png")
#Facet the mean and median pollutantlevels by city
city_facets <- faceted_plot(prawn_path = prawn_path,
                            group= "TCITY15NM",
                            pollutant = pollutant)

  ggsave(filename= paste0(proc_tag,"/",pollutant," faceted by city.png"),
        plot=city_facets,
        device="png")

#Facet the mean and median pollutant levels by county/unitary authority
area_facets <- faceted_plot(prawn_path = prawn_path,
                              group= "TCITY15NM",
                              pollutant = pollutant)
  ggsave(filename= paste0(proc_tag,"/",pollutant," faceted by area.png"),
         plot=area_facets,
         device="png")

#Plot the average pollutant vs average IMD grouped by county/ua
avg_imd_pol <- area_IMD_vs_pol(prawn_path=prawn_path,
                               pollutant = pollutant,
                               area_type = "Area")
  ggsave(filename= paste0(proc_tag,"/",pollutant," average vs average IMD by area.png"),
       plot=avg_imd_pol,
       device="png")

#calculate and record the difference between the mean and median points and regression lines at deciles 1 and 10
numbers <- stat_wrangler(prawn_path = prawn_path,
              deciles=c(1,10))

  write.csv(x=numbers,
            file=paste0(proc_tag,"/differnce between deciles.csv"))
}
