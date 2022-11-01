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
#' @param dpi The dpi to save graphs with, defaults to 600, the minimum for figures in an RSC publication, higher values will take longer to process
#'
#' @param file_format The file format to save in, should be all lower case and in quotes, as it's piped straight into a ggsave function
#' @keywords faceted, sources
#' @export
#' @examples
#' bulk_processor()
#'

bulk_processor <- function(raster_path,
                           shapefile_path,
                           pollutant_data_name,
                           year,
                           pollutant,
                           iteration=as.character(packageVersion("PRAWNS")),
                           dpi=600,
                           file_format="png"){
  #work out the version pf the package for reproducibility, calculating it here makes the code more streamlined

#store the procedural tag as a variable to save space and make the code clearer

proc_tag <- paste0(pollutant,"_emissions_in_",year,"_v",iteration)

if(grepl("agg_",file_format)==TRUE){
  file_format1 <- gsub(pattern="agg_",x=file_format,replacement = "")}

#This loop creates three different outputs using different data: the base data, the data without london and the data with na values replaced with 0
for ( index in c(1:3)){

  #Create the folder for the results using the raw data
  if (index==1){
    proc_tag <- paste0(pollutant,"_emissions_in_",year,"_v",iteration)
    prawn_path <- paste0(proc_tag,"/PRAWN.csv")
    #The base path remains unchanged and is used to fetch a raw copy of the prawn for processed versions
    raw_path <- paste0(proc_tag,"/PRAWN.csv")

    #create the folder that everything goes in
    dir.create(path=paste0(proc_tag))

    #create the base prawn for this function, it will also be used by iterations 2 and 3 but filtered, it is created with NA values included so the data can be analysed
    create_prawns(
      raster_path= raster_path,
      shapefile_path = shapefile_path,
      pollutant_data_name = pollutant_data_name,
      year=year,
      pollutant=pollutant,
      output_path = paste0(proc_tag,"/PRAWN with NA.csv"))

    #set all na values to 0
    transformer <- read.csv(paste0(proc_tag,"/PRAWN with NA.csv"),
                    row.names=1,
                    check.names=FALSE) %>%

      replace_na(list("Agricultural"=0,"Domestic combustion"=0,"Energy production"=0,
                      "Industrial combustion"=0,"Industrial production"=0,"Natural"=0,
                      "Offshore"=0,"Other transport and mobile machinery"=0,"Road transport"=0,"Solvents"=0,"Total"=0
                      ,"Total_no_points"=0,"Waste treatment and disposal"=0))

    write.csv(x=transformer,
              file=prawn_path)

    shape_test <- shapefile_checker(shapefile_path)

    ggsave(filename= paste0(proc_tag,"/shapefile_test.png"),
           plot=shape_test,
           device="png")

    stat_facet <- side_by_side_stats(pollutant=pollutant,
                                    prawn_path=prawn_path,
                                    year=2020)

    graph_saver(filename= paste0(proc_tag,"/medmeancomp.",file_format1),
           plot=stat_facet,
           file_format = file_format,
           type=2,
           scaling=0.3)

  #clear up
    rm(shape_test)
    rm(stat_facet)

    print("Basic PRAWN creation and data tests successful")
  }

  #create the results without London,
  if (index==2){
    proc_tag <- paste0(pollutant,"_emissions_in_",year,"_v",iteration,"/Londonless")
    prawn_path <- paste0(proc_tag,"/PRAWN.csv")
    #create the folder that everything goes in
    dir.create(path=paste0(proc_tag))
    #Create the filtered data without London and save it at prawn_path
    londonless_prawn <- read.csv(raw_path,
                                 row.names=1,
                                 check.names=FALSE) %>%
                        tibble() %>%
                        filter(TCITY15NM!="London") %>%
                        mutate(IMD=ntile(Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.,10))
    #Write the filtered prawn
    write.csv(x = londonless_prawn,
              file=prawn_path)

    rm(londonless_prawn)

    print("Creation of PRAWNS without London successful")
  }

  if (index==3){
    proc_tag <- paste0(pollutant,"_emissions_in_",year,"_v",iteration,"/London_only")
    prawn_path <- paste0(proc_tag,"/PRAWN.csv")
    #create the folder that everything goes in
    dir.create(path=paste0(proc_tag))
    #Create the filtered data without London and save it at prawn_path
    londonless_prawn <- read.csv(raw_path,
                                 row.names=1,
                                 check.names=FALSE) %>%
      tibble() %>%
      filter(TCITY15NM=="London") %>%
      mutate(IMD=ntile(Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.,10))

    #Write the filtered prawn
    write.csv(x = londonless_prawn,
              file=prawn_path)
    rm(londonless_prawn)
    print("Creation of PRAWNS with only London succesful")
  }

  noxxogram <- avg_nox_histogram(prawn_path)

  graph_saver(filename= paste0(proc_tag,"/",pollutant," emission average per IMD histogram with ",noxxogram[[2]]," entries cropped to right of limit.",file_format1),
         plot=noxxogram[[1]],
         file_format = file_format,
         type=1,
         scaling=0.6)

  rm(noxxogram)
#Make and save a graph where the sources are all faceted
source_facets <- faceted_sources(prawn_path = prawn_path,
                                 pollutant=pollutant,
                                 year=year)

  graph_saver(filename= paste0(proc_tag,"/",pollutant," faceted sources.",file_format1),
         plot=source_facets,
         file_format = file_format,
         type=2,
         scaling=0.6)

  rm(source_facets)
#Make and save a graph showing IMD based inequality for each RUC code
RUC_breakdown <- RUC_IMD(prawn_path = prawn_path,
                         pollutant=pollutant,
                         year=year)

  graph_saver(filename= paste0(proc_tag,"/",pollutant," RUC breakdown.",file_format1),
         plot=RUC_breakdown[[1]],
         file_format = file_format,
         type=2,
         scaling=0.8)

  graph_saver(filename= paste0(proc_tag,"/",pollutant," RUC populationbreakdown.",file_format1),
         plot=RUC_breakdown[[2]],
         file_format = file_format,
         type=1,
         scaling=0.4)

  graph_saver(filename= paste0(proc_tag,"/RUC IMD histogram.",file_format1),
         plot=RUC_breakdown[[3]],
         file_format = file_format,
         type=1,
         scaling=0.4)

  graph_saver(filename= paste0(proc_tag,"/RUC IMD histogram2.",file_format1),
         plot=RUC_breakdown[[4]],
         file_format = file_format,
         type=1,
         scaling=0.4)

  write.csv(x=RUC_breakdown[[5]],file = paste0(proc_tag,"/analysis of RUC linear models.csv"))

    rm(RUC_breakdown)

#Facet the mean and median pollutantlevels by city
city_facets <- faceted_plot(prawn_path = prawn_path,
                            group= "TCITY15NM",
                            pollutant = pollutant)

  graph_saver(filename= paste0(proc_tag,"/",pollutant," faceted by city.",file_format1),
        plot=city_facets,
        file_format = file_format,
        type=3,
        scaling=0.5)
  rm(city_facets)
#Facet the mean and median pollutant levels by county/unitary authority
area_facets <- faceted_plot(prawn_path = prawn_path,
                              group= "TCITY15NM",
                              pollutant = pollutant)

  graph_saver(filename= paste0(proc_tag,"/",pollutant," faceted by area.",file_format1),
         plot=area_facets,
         file_format = file_format,
         type=3,
         scaling=0.5)

    rm(area_facets)
#Plot the average pollutant vs average IMD grouped by county/ua
avg_imd_pol <- area_IMD_vs_pol(prawn_path=prawn_path,
                               pollutant = pollutant,
                               area_type = "County/UA",
                               year=year)

  graph_saver(filename= paste0(proc_tag,"/",pollutant," average vs average IMD by county UA.",file_format1),
       plot=avg_imd_pol[[1]],
       file_format = file_format,
       type=1,
       scaling=0.5)

  write.csv(x=avg_imd_pol[[2]],
            file = paste0(proc_tag,"/model analysis for",pollutant," average vs average IMD by county UA .csv"))

  rm(avg_imd_pol)

area_histogram <- plot_area_gradients(prawn_path=prawn_path,area_type="County/UA")

graph_saver(filename= paste0(proc_tag,"/",pollutant," emission gradient for counties and UAs.",file_format1),
       plot=area_histogram,
       file_format = file_format,
       type=1,
       scaling=0.4)

  rm(area_histogram)
avg_imd_pol <- area_IMD_vs_pol(prawn_path=prawn_path,
                                 pollutant = pollutant,
                                 area_type = "City",
                                 year=year)

  graph_saver(filename= paste0(proc_tag,"/",pollutant," average vs average IMD by city.",file_format1),
         plot=avg_imd_pol[[1]],
         file_format = file_format,
         type=1,
         scaling=0.4)

  write.csv(x=avg_imd_pol[[2]],
            file = paste0(proc_tag,"/model analysis for",pollutant," average vs average IMD by city .csv"))

  area_histogram <- plot_area_gradients(prawn_path=prawn_path,area_type="City")

  graph_saver(filename= paste0(proc_tag,"/",pollutant," emission gradient for cities.",file_format1),
         plot=area_histogram,
         file_format = file_format,
         type=1,
         scaling=0.4)

  rm(avg_imd_pol)
# calculate and record the difference between the mean and median points and regression lines at deciles 1 and 10
numbers <- stat_wrangler(prawn_path = prawn_path)

  statnames <- c("Mean regression model","Median regression model","difference between deciles")
  for (position in c(1:3)){
  write.csv(x=numbers[position],
            file=paste0(proc_tag,"/",statnames[position],".csv"))
}
graph_saver(filename= paste0(proc_tag,"/residuals for linear fit.",file_format1),
            plot=numbers[4][[1]],
            file_format = file_format,
            type=2,
            scaling=0.7)
 rm(numbers)

p_plot <- p_values_for_chunks(prawn_path)

graph_saver(filename= paste0(proc_tag,"/p_values for random chunks.",file_format1),
            plot=p_plot,
            file_format = file_format,
            type=2,
            scaling=0.7)
  rm(p_plot)

sourceogram <- LSOA_pollutant_histo(prawn_path)

graph_saver(filename= paste0(proc_tag,"/histogram of ",pollutant,"emissions by source.",file_format1),
            plot=sourceogram,
            file_format = file_format,
            type=2,
            scaling=0.7)

rm(sourceogram)

pie <- gradient_bar(pollutant = pollutant,
                    #The input path is the same as the output file for numbers
                    input_path=paste0(proc_tag,"/difference between deciles.csv"))

 graph_saver(filename= paste0(proc_tag,"/pie chart of how ",pollutant," sources contribute to the inequality gradient.",file_format1),
       plot=pie,
       file_format = file_format,
       type=1,
       scaling=0.4)

 rm(pie)


 print(paste0("Graphing pass ",index," of 3 successful"))
#close the for loop
}
#close the function
}
