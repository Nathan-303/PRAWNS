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
#'
#' @param gotta_go_fast Allows the code to sacrifice things that take a long time to get it to run faster, defaults to "No"
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
                           file_format="png",
                           gotta_go_fast="No"){
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
    process_create_prawns(
      raster_path= raster_path,
      shapefile_path = shapefile_path,
      pollutant_data_name = pollutant_data_name,
      year=year,
      pollutant=pollutant,
      output_path = prawn_path)

    #set all na values to 0
    # transformer <- read.csv(paste0(proc_tag,"/PRAWN with NA.csv"),
    #                 row.names=1,
    #                 check.names=FALSE) %>%
    #
    #   replace_na(list("Agricultural"=0,"Domestic combustion"=0,"Energy production"=0,
    #                   "Industrial combustion"=0,"Industrial production"=0,"Natural"=0,
    #                   "Offshore"=0,"Other transport and mobile machinery"=0,"Road transport"=0,"Solvents"=0,"Total"=0
    #                   ,"Total_no_points"=0,"Waste treatment and disposal"=0))
    #
    # write.csv(x=transformer,
    #           file=prawn_path)

    shape_test <- shapefile_checker(shapefile_path)

    ggsave(filename= paste0(proc_tag,"/shapefile_test.png"),
           plot=shape_test,
           device="png")

    stat_facet <- facet_medmean_london_src(pollutant=pollutant,
                                    prawn_path=prawn_path,
                                    year=year)

    process_graph_saver(filename= paste0(proc_tag,"/medmeancomp.",file_format1),
           plot=stat_facet,
           file_format = file_format,
           type=2,
           scaling=0.3)

  #clear up
    rm(shape_test)
    rm(stat_facet)

    print("Basic PRAWN creation and data tests successful")
  }
#Begin a skip in the fastest mode
if(gotta_go_fast != "zooom"){
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

    london_focus <- WIP_focus_compare_trimmed_data_src(prawn_path,raw_path,pollutant,year)

    process_graph_saver(filename= paste0(proc_tag,"/Total",pollutant," emissions for London.",file_format1),
                plot=london_focus,
                file_format = file_format,
                type=1,
                scaling=0.5)

    rm(londonless_prawn)

    print("Creation of PRAWNS with only London succesful")

  }
#End a skip if in the fastest mode
}
  #do the bare minumum graph for abstracts
print("making a minimalistic graph")
minimal <- cartesian_minimalist_src(prawn_path=prawn_path,
                            pollutant=pollutant,
                            year=year)

process_graph_saver(filename= paste0(proc_tag,"/Minimalistic",pollutant," emissions.",file_format1),
            plot=minimal,
            file_format = file_format,
            type=4,
            scaling=0.5)

rm(londonless_prawn)

#Make a histogram showing the distribution of NOx averages
print("Making a histogram of pollutant averages")

  total_emission_histogram <- histogram_total_emissions_src(prawn_path=prawn_path,
                                 pollutant=pollutant,
                                 year=year)

  process_graph_saver(filename= paste0(proc_tag,"/",pollutant," emission average per IMD histogram with ",total_emission_histogram[[2]]," entries cropped to right of limit.",file_format1),
         plot=total_emission_histogram[[1]],
         file_format = file_format,
         type=1,
         scaling=0.6)

  rm(noxxogram)

print("Plotting inequality for expanse quintiles")
exquint <- cartesian_expanse_quintile_src(prawn_path=prawn_path,
                                       pollutant=pollutant,
                                       year=year)

process_graph_saver(filename= paste0(proc_tag,"/",pollutant," emission inequality for expanse quintiles.",file_format1),
            plot=exquint,
            file_format = file_format,
            type=1,
            scaling=0.6)

rm(exquint)

#Make and save a graph where the sources are all faceted
print("Making a faceted graph of sources")
source_facets <- facet_sources_src(prawn_path = prawn_path,
                                 pollutant=pollutant,
                                 year=year)

  process_graph_saver(filename= paste0(proc_tag,"/",pollutant," faceted sources.",file_format1),
         plot=source_facets,
         file_format = file_format,
         type=2,
         scaling=0.6)

  rm(source_facets)

#Make and save a graph showing IMD based inequality for each RUC code
print("Graphing IMD vs pollutant for each RUC")
RUC_breakdown <- facet_RUC_src(prawn_path = prawn_path,
                         pollutant=pollutant,
                         year=year)

  process_graph_saver(filename= paste0(proc_tag,"/",pollutant," RUC breakdown.",file_format1),
         plot=RUC_breakdown[[1]],
         file_format = file_format,
         type=2,
         scaling=0.8)

  process_graph_saver(filename= paste0(proc_tag,"/",pollutant," RUC populationbreakdown.",file_format1),
         plot=RUC_breakdown[[2]],
         file_format = file_format,
         type=1,
         scaling=0.4)

  process_graph_saver(filename= paste0(proc_tag,"/RUC IMD histogram.",file_format1),
         plot=RUC_breakdown[[3]],
         file_format = file_format,
         type=1,
         scaling=0.4)

  process_graph_saver(filename= paste0(proc_tag,"/RUC IMD histogram2.",file_format1),
         plot=RUC_breakdown[[4]],
         file_format = file_format,
         type=2,
         scaling=1)

  write.csv(x=RUC_breakdown[[5]],file = paste0(proc_tag,"/analysis of RUC linear models.csv"))

    rm(RUC_breakdown)
#Begin a skip if in anything but the fastest mode

if(gotta_go_fast == "No"){
#Facet the mean and median pollutantlevels by city
print("Faceting by city, this may take a while")
city_facets <- facet_all_areas_src(prawn_path = prawn_path,
                            group= "TCITY15NM",
                            pollutant = pollutant,
                            facet_name="city")

  process_graph_saver(filename= paste0(proc_tag,"/",pollutant,"_faceted_by_city.",file_format1),
        plot=city_facets,
        file_format = file_format,
        type=3,
        scaling=0.5)
  rm(city_facets)


#Facet the mean and median pollutant levels by county/unitary authority
print("Faceting by county/UA, this may take a while")
area_facets <- facet_all_areas_src(prawn_path = prawn_path,
                              group= "counties",
                              pollutant = pollutant,
                              facet_name="county/unitary authority")

  process_graph_saver(filename= paste0(proc_tag,"/",pollutant,"_faceted_by_area.",file_format1),
         plot=area_facets,
         file_format = file_format,
         type=3,
         scaling=0.5)

    rm(area_facets)
#End a skip if going in anything but the slowest mode
}
#Plot the average pollutant vs average IMD grouped by county/ua
print("Plotting county/UA as a scatter where the axes are average IMD and average pollutant")
avg_imd_pol <- cartesian_area_avg_deprivation_src(prawn_path=prawn_path,
                               pollutant = pollutant,
                               area_type = "County/UA",
                               year=year)

  process_graph_saver(filename= paste0(proc_tag,"/",pollutant," average vs average IMD by county UA.",file_format1),
       plot=avg_imd_pol[[1]],
       file_format = file_format,
       type=1,
       scaling=0.6)

  write.csv(x=avg_imd_pol[[2]],
            file = paste0(proc_tag,"/model analysis for",pollutant," average vs average IMD by county UA .csv"))

  rm(avg_imd_pol)

print("Calculating the individual gradients for each county/UA and putting them on a histogram")
area_histogram <- cartesian_area_gradients_src(prawn_path=prawn_path,area_type="County/UA")

process_graph_saver(filename= paste0(proc_tag,"/",pollutant," emission gradient for counties and UAs.",file_format1),
       plot=area_histogram,
       file_format = file_format,
       type=1,
       scaling=0.7)

  rm(area_histogram)

print("Repeating what was done for county/UA but for cities instead")
avg_imd_pol <- cartesian_area_avg_deprivation_src(prawn_path=prawn_path,
                                 pollutant = pollutant,
                                 area_type = "City",
                                 year=year)

  process_graph_saver(filename= paste0(proc_tag,"/",pollutant," average vs average IMD by city.",file_format1),
         plot=avg_imd_pol[[1]],
         file_format = file_format,
         type=2,
         scaling=1.2)

  write.csv(x=avg_imd_pol[[2]],
            file = paste0(proc_tag,"/model analysis for",
                          pollutant," average vs average IMD by city .csv"))

  area_histogram <- cartesian_area_gradients_src(prawn_path=prawn_path,area_type="City")

  process_graph_saver(filename= paste0(proc_tag,"/",pollutant," emission gradient for cities.",file_format1),
         plot=area_histogram,
         file_format = file_format,
         type=2,
         scaling=1.2)

  rm(avg_imd_pol)

#Begin skip if going in fastest mode
if(!(gotta_go_fast%in%c("zooom"))){
print("Doing lots of maths")
# calculate and record the difference between the mean and median points and regression lines at deciles 1 and 10
numbers <- stats_reg_RMSE_reg_src(prawn_path = prawn_path,pollutant=pollutant)

  statnames <- c("Mean regression model","Median regression model","difference between deciles")
  for (position in c(1:3)){
  write.csv(x=numbers[position],
            file=paste0(proc_tag,"/",statnames[position],".csv"))
  }
  print("Putting the change in concentration from most to least deprived on a bar graph")

#end skip if going in fastest mode
}
#Begin skip if going in anything but slowest mode
if(!(gotta_go_fast%in%c("zooom","yes"))){
process_graph_saver(filename= paste0(proc_tag,"/residuals for linear fit.",file_format1),
            plot=numbers[4][[1]],
            file_format = file_format,
            type=2,
            scaling=0.7)

  process_graph_saver(filename= paste0(proc_tag,"/histogram of linear fit residuals.",file_format1),
              plot=numbers[5][[1]],
              file_format = file_format,
              type=2,
              scaling=0.7)
 rm(numbers)

print("Calculating p values for randomly sliced and diced data")
p_plot <- stats_pvalues_src(prawn_path)

process_graph_saver(filename= paste0(proc_tag,"/p_values for random chunks.",file_format1),
            plot=p_plot,
            file_format = file_format,
            type=2,
            scaling=0.7)
  rm(p_plot)

#End skip if going in anything but slowest mode, numbers was created outside the skip and is removed inside, so this tidies it up if the skil happened
}else{rm(numbers)}

print("Plotting the change in road transport as a contribution to the total for the most deprived 20%")

expanse_scale <- cartesian_area_for_most_deprived_src(prawn_path,pollutant,year)

process_graph_saver(filename= paste0(proc_tag,"/how ",pollutant," sources differ with LSOA area for most deprived.",file_format1),
            plot=expanse_scale,
            file_format = file_format,
            type=1,
            scaling=0.4)
rm(expanse_scale)

print("Plotting a histogram of all the sources")
sourceogram <- histogram_sources_src(prawn_path)

process_graph_saver(filename= paste0(proc_tag,"/histogram of ",pollutant,"emissions by source.",file_format1),
            plot=sourceogram,
            file_format = file_format,
            type=2,
            scaling=0.7)

rm(sourceogram)

print("plotting emissions vs LSOA expanse faceted by source sector")
emexpanse <- cartesian_expanse_src(prawn_path,
                                  pollutant,
                                  year)

process_graph_saver(filename= paste0(proc_tag,"/",pollutant,"emissions by LSOA expanse faceted by source.",file_format1),
            plot=emexpanse,
            file_format = file_format,
            type=2,
            scaling=0.7)

rm(emexpanse)

if(gotta_go_fast=="No"){
print("Plotting inequalities for size slices")
  areas <- facet_expanse_src(prawn_path = prawn_path,
                         pollutant=pollutant,
                         year=year)

  process_graph_saver(filename= paste0(proc_tag,"/emissions_by_sector_in_12_size_percentiles.",file_format1),
              plot=areas[[1]],
              file_format = file_format,
              type=3,
              scaling=0.7)

  process_graph_saver(filename= paste0(proc_tag,"/histogram of LSOA areas faceted by IMD.",file_format1),
              plot=areas[[2]],
              file_format = file_format,
              type=2,
              scaling=0.7)

  rm(areas)
}


 print(paste0("Graphing pass ",index," of 3 successful"))
#close the for loop
}
#close the function
}
