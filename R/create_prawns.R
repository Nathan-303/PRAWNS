#' A function for creating the data source for PRAWNS functions``
#'
#' This function takes a raster and a shapefile, works out the average value for
#' each polygon in the shapefile then uses an identifier present in the
#' shapefile to link with other data sources. This the pollution for areas to be
#' worked out then linked to statistics associated with those areas. Refer to +
#' the vignette setting_up_data sources to obtain the correct input files and
#' structure them for compatibility
#'
#' @param raster_path The filepath for the raster that is to be used. This is
#' compatible with folders containing multiple raster layers, mutually exclusive with tif path
#'
#' @param tif_path The filepath for the tif file used to input modelled data, mutually sxclusive with raster_path
#'
#' @param shapefile_path The filepath for the shapefile to be used
#'
#' @param output_path The filepath to output to. Defaults to FALSE
#'
#' @param pollutant the pollutant being investigated, this should be in quotes and correspond with its name in the data
#'
#' @param pollutant_data_name what the pollutant is referred to as in the tables
#'
#' @param year the year the data is from, used to standardise column names, currently doesnt work for years before 2000
#'
#' @param is_raw whether or not to output a csv only containing the LSOAs mapped to the emissions

#' @keywords data
#' @export

#'
#' @examples
#' create_prawns()
#'

create_prawns <- function(raster_path=FALSE,
                          tif_path=FALSE,
                          shapefile_path,
                          output_path=FALSE,
                          pollutant_data_name,
                          year,
                          pollutant,
                          is_raw=FALSE){

# Calculate the average pollution for each area ----------------------------

#run an ifelse to read either the rasters or a tif, the outputs (source_stack) are the same class of object
  if(raster_path!=FALSE){
  #Create a list of all the raster files present in the folder specified by raster_path
  filelist <- list.files(raster_path,
    pattern = ".asc",
    full.names = TRUE)

  #Store the rasters as a stack
  source_stack <- rast(filelist[1])
  for(index in 2:length(filelist)){
    source_stack <- c(source_stack,rast(filelist[index]))
  }
  }else{
    source_stack <- read_stars("Data/2019_modelled_NOx.tif") %>% rast()

  }
  #Read the shapefile
  LSOA_shapefile <- vect(shapefile_path)

  #Calculate the average for each polygon in the shapefile
  index <-  c(1:length(LSOA_shapefile))
  transient <- sf::st_as_sf(LSOA_shapefile[index])
  pollution_mean <- exact_extract(source_stack,transient,'mean')
  #Output the results as a tibble containing the indexed position, the pollution mean and the LSOA code, a property from the shapefile that enables binding on LSOA statistics
  output <- tibble(FID=index,
                   poll_mean=pollution_mean,
                   LSOA11CD=LSOA_shapefile$LSOA11CD
                   ) %>% unnest(poll_mean)
  #output a csv wit the minimum processiong done if is.raw=true
  if (is_raw==TRUE){
    write.csv(file=output_path,x = output
              )
    output
  }else{

# Read the additional data as a list of tibbles----------------------------------------------------------------

  rural_urban <- read.csv("Data/LSOA_statistics/LSOA_urban_rural.csv") %>% tibble %>%  dplyr::select(-FID)

  city_data <- read.csv("Data/LSOA_statistics/city lookup 2011.csv") %>%
    tibble()
  #Renames a column to avoid a special character that makes things go wrong
  colnames(city_data)[1] <- "LSOA11CD"

  #Reads the demographic information about the LSOAs, binds them by LSOA code so the FID is incorporated
  LSOA_demographics <- read.csv("Data/LSOA_statistics/2019_LSOA_Stats.csv") %>%
    tibble() %>%
    rename(LSOA11CD=LSOA.code..2011.) %>%
    inner_join(city_data,by="LSOA11CD")%>%
    inner_join(rural_urban,by=c("LSOA11NM"="LSOA11NM"))

  #Links the demographic data to the references to the shapefile
  demo_linked_reference <- inner_join(city_data,LSOA_demographics,by=c("LSOA11CD"="LSOA11CD","FID"="FID",
                                                                       "LSOA11NM"="LSOA11NM","TCITY15CD"="TCITY15CD",
                                                                       "TCITY15NM"="TCITY15NM"))

  #Reads the county lookup data
  county_lookup <- read.csv("Data/LSOA_statistics/county lookup 2019.csv",row.names = 1)

  #Makes a chunk for the LSOAs that are in counties, then mutates in a column saying it's a couty not UA
  county_chunk <- inner_join(county_lookup,demo_linked_reference,by=c("LAD19NM"="Local.Authority.District.name..2019.")) %>%
    mutate(Area_Type="County") %>%
    rename("Area"=CTY19NM)

  unitary_list <- !(LSOA_demographics$Local.Authority.District.name..2019. %in% county_lookup$LAD19NM)

  unitary_chunk <- LSOA_demographics[unitary_list,] %>%
    mutate(Area_Type="Unitary Authority",Area=Local.Authority.District.name..2019.)# %>%
  #inner_join(demo_linked_reference,by="LSOA11CD")

  refined_chunk <- bind_rows(unitary_chunk,county_chunk) %>% tibble()

# Combine the pollution means with the additional data --------------------


  prawns <- inner_join(output,refined_chunk,by="LSOA11CD")


  renamer <- function(data,last_two_digits_year,pollutant_data_name){

# Rename the columns for readability --------------------------------------


    NamedList <- c("Agricultural","Domestic combustion","Energy production",
                   "Industrial combustion","Industrial production","Natural",
                   "Offshore","Other transport and mobile machinery","Road transport","Solvents","Total"
                   ,"Total_no_points","Waste treatment and disposal")

    Nmdlst <- paste0("mean.",c("agric","domcom","energyprod","indcom","indproc","nature","offshore","othertrans","roadtrans","solvents","total","totarea","waste","pntsrc"),pollutant_data_name,last_two_digits_year)

    tracer <- colnames(data) %in% Nmdlst

    #Finds the first position where matches start
    starter <- detect_index(tracer,is_true)-1

    for(index in 1:length(tracer)){
      if(tracer[index] == TRUE){
        colnames(data)[index] <- NamedList[index-starter]
      }}

    data
  }

  prawns <- renamer(
    data=prawns,
    last_two_digits_year=year-2000,
    pollutant_data_name = pollutant_data_name
  ) %>% mutate("Point sources"=Total-Total_no_points) %>%
    rename(IMD=Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.)
  #Return the resulting object

# Output the results ------------------------------------------------------
if (output_path!=FALSE){
  write.csv(prawns,
            file=output_path)
}


prawns
  }
}

