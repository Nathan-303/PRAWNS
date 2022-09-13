#' A function that performs everything in the package and stores the results in
#' a folder that is created with an appropriate procedurally generated name
#'
#' This function takes a prawns CSV and produces a summary of the geographic areas
#' matching an inputted parameter. Refer to the vignette setting_up_data sources
#' to obtain the correct input files and structure them for compatibility
#'
#'@param raster_path The filepath for the raster that is to be used. This is
#' compatible with folders containing multiple raster layers
#'
#' @param shapefile_path The filepath for the shapefile to be used
#'
#' @param pollutant the pollutant being investigated, this should be in quotes and correspond with its name in the data
#'
#' @param pollutant_data_name what the pollutant is referred to as in the raw data, should be quoted
#'
#' @param year the year the data is from, used to standardise column names, currently doesnt work for years before 2000

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The name of the pollutant that's being examined, this is used in the graph names
#'
#' @keywords faceted, sources
#' @export
#' @examples
#' bulk_processor()
#'

scottish_bulk_processor <- function(raster_path,
                           shapefile_path,
                           pollutant_data_name,
                           year,
                           pollutant,
                           iteration=as.character(packageVersion("PRAWNS"))
                           ){
  #work out the version pf the package for reproducibility, calculating it here makes the code more streamlined

#store the procedural tag as a variable to save space and make the code clearer

proc_tag <- paste0(pollutant,"_emissions_in_",year,"_v",iteration)

#This loop creates three different outputs using different data: the base data, the data without london and the data with na values replaced with 0
for ( index in c(1:3)){

  #Create the folder for the results using the raw data
  if (index==1){
    proc_tag <- paste0(pollutant,"_emissions_in_",year,"_v",iteration)
    prawn_path <- paste0(proc_tag,"/PRAWN.csv")
    #The base path remains unchanged and is used to fetch a raw copy of the prawn for processed versions
    raw_path <- paste0(proc_tag,"/PRAWN.csv")
    #The crude path is used to output the unrefined prawn
    crude_path <- paste0(proc_tag,"/crude_PRAWN.csv")
    #create the folder that everything goes in
    dir.create(path=paste0(proc_tag))

    #create the base prawn for this function, it will also be used by iterations 2 and 3 but filtered
    create_scottish_prawns(
      raster_path= raster_path,
      shapefile_path = shapefile_path,
      pollutant_data_name = pollutant_data_name,
      year=year,
      pollutant=pollutant,
      output_path = prawn_path)

    #output the raw form of the prawn as well so that it can be examined if needed
    create_scottish_prawns(
      raster_path= raster_path,
      shapefile_path = shapefile_path,
      pollutant_data_name = pollutant_data_name,
      year=year,
      pollutant=pollutant,
      output_path = crude_path,
      is_raw = TRUE)

    data_summary <- data_flagger(crude_path) %>% tibble()
    write.csv(x =  data_summary,
              file = paste0(proc_tag,"/data_summary.csv")
              )

    shape_test <- shapefile_checker(shapefile_path)

    ggsave(filename= paste0(proc_tag,"/shapefile_test.png"),
           plot=shape_test,
           device="png")

    print("Basic PRAWN creation and data tests successful")
  }


  if (index==2){
    proc_tag <- paste0(pollutant,"_emissions_in_",year,"_v",iteration,"/na is 0")
    prawn_path <- paste0(proc_tag,"/PRAWN.csv")
    #create the folder that everything goes in
    dir.create(path=paste0(proc_tag))

    #Create the filtered data without London and save it at prawn_path
    na_0_prawn <- read.csv(raw_path,
                           row.names=1,
                           check.names=FALSE) %>%
                  tibble() %>%
                  replace_na(list("Agricultural"=0,"Domestic combustion"=0,"Energy production"=0,
                              "Industrial combustion"=0,"Industrial production"=0,"Natural"=0,
                              "Offshore"=0,"Other transport and mobile machinery"=0,"Road transport"=0,"Solvents"=0,"Total"=0
                              ,"Total_no_points"=0,"Waste treatment and disposal"=0))
    write.csv(x = na_0_prawn,
              file=prawn_path)

    print("Creation of prawns where na values are set to 0 successful")
    }
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
              )

  write.csv(x=numbers,
            file=paste0(proc_tag,"/difference between deciles.csv"))


pie <- gradient_bar(pollutant = pollutant,
                    #The input path is the same as the output file for numbers
                    input_path=paste0(proc_tag,"/difference between deciles.csv"))

 ggsave(filename= paste0(proc_tag,"/pie chart of how ",pollutant," sources contribute to the inequality gradient.png"),
       plot=pie,
       device="png")

 print(paste0("Graphing pass ",index," of 3 successful"))
#close the for loop
}
#close the function
}
