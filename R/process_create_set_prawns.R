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
#'
#' @keywords data
#'
#' @export
#'
#' @examples
#' process_create_prawns(
#'   raster_path="The folder containing the rasters",
#'   shapefile_path="Where the shapefiles are",
#'   pollutant_data_name="nox",
#'   year="2019",
#'   pollutant="NOx")
shapefile_path <- "Data/Historic_stats/2011_LSOA"

process_create_set_prawns <- function(raster_path="undefined",
                          tif_path="undefined",
                          csv_coordinates_path="undefined",
                          shapefile_path,
                          output_path="undefined",
                          pollutant_data_name,
                          year,
                          pollutant,
                          is_raw=FALSE){

# Calculate the average pollution for each area ----------------------------
  #names that the LSOA column can have
  potentials <- c("LSOA01CD","LSOA11CD","LSOA21CD")

#Three chained if statements, which trigger if there is a path to the appropriate file in the function call
  #If its'a raster
  if(raster_path!="undefined"){
  #Create a list of all the raster files present in the folder specified by raster_path
  filelist <- list.files(raster_path,
    pattern = ".asc",
    full.names = TRUE)

  #Store the rasters as a stack
  source_stack <- rast(filelist[1]) %>% terra::subst(NA,0)
  for(index in 2:length(filelist)){
    source_stack <- c(source_stack,rast(filelist[index])%>% terra::subst(NA,0))
  }
  }
  #If its a tif
  if(tif_path!="undefined"){
    source_stack <- read_stars("Data/2019_modelled_NOx.tif") %>% rast()

  }
  #If it's a csv
  if(csv_coordinates_path!="undefined"){
    print(csv_coordinates_path)
    csv_raster <- read.csv(file=csv_coordinates_path,
                           skip = 5,
                           row.names=1,
                           check.names=FALSE) %>% tibble()
  print("csv made")
    base_raster <- rasterFromXYZ(csv_raster)
  print("raster next")
    source_stack <- rast(base_raster)
  }
print("vect call")
  #Read the shapefile
  LSOA_shapefile <- vect(shapefile_path)
  LSOA_field <- intersect(potentials,as.character(names(LSOA_shapefile)))
    #potentials%in%names(LSOA_shapefile)
  LSOA_shapefile <- LSOA_shapefile %>% tidyterra::rename("LSOA"=LSOA_field)
print("index call")
  #Calculate the average for each polygon in the shapefile
  index <-  c(1:length(LSOA_shapefile))
  print("stftft call")
  transient <- sf::st_as_sf(LSOA_shapefile[index])
  print("exactextract call")
  pollution_mean <- exact_extract(source_stack,transient,'mean')
  #Output the results as a tibble containing the indexed position, the pollution mean and the LSOA code, a property from the shapefile that enables binding on LSOA statistics
  print("output call")
  output <- tibble(poll_mean=pollution_mean,
                   LSOA=LSOA_shapefile[LSOA],
                   expanse=expanse(LSOA_shapefile)
                   ) %>% unnest(poll_mean)


  if(csv_coordinates_path!="undefined"){
    return(output)
  }
  #output a csv with minimum processing
  if(output_path!="undefined"){
    print("outputting?")
    if (is_raw==TRUE){
      print("writing file")
    write.csv(file=output_path,
              x = output)

      return(output)
    }else{

        write.csv(file=output_path,
                    x = output)}}


# Read the additional data as a list of tibbles----------------------------------------------------------------

  rural_urban <- read.csv("Data/LSOA_statistics/LSOA_urban_rural.csv") %>%
    tibble %>%  dplyr::select(-FID)

  city_data <- read.csv("Data/LSOA_statistics/city lookup 2011.csv") %>%
    tibble()
  #Renames a column to avoid a special character that makes things go wrong
  colnames(city_data)[1] <- "LSOA21CD"

  #Reads the demographic information about the LSOAs, binds them by LSOA code so the FID is incorporated
  LSOA_demographics <- read.csv("Data/LSOA_statistics/2019_LSOA_Stats.csv") %>%
    tibble() %>%
    rename(LSOA11CD=LSOA.code..2011.) %>%
    inner_join(city_data,by="LSOA11CD")%>%
    inner_join(rural_urban,by=c("LSOA11CD"="LSOA11CD"))

  #Links the demographic data to the references to the shapefile
  demo_linked_reference <- inner_join(city_data,LSOA_demographics,by=c("LSOA11CD"="LSOA11CD","FID"="FID","TCITY15CD"="TCITY15CD",
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

  rm(unitary_list)
  refined_chunk <- bind_rows(unitary_chunk,county_chunk) %>% tibble()
  rm(unitary_chunk)
  rm(county_chunk)

#add a conversion column for 2011 LSOA codes
  keys <- read.csv("Data/LSOA_statistics/LSOA_porter.csv",
                   check.names=FALSE)

  conversion <- inner_join(refined_chunk,keys,by="LSOA11CD")

  tidy <- conversion %>% group_by(LSOA21CD) %>%
    summarise(IMDrank=mean(Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.),
                           RUC11=RUC11,
              LSOA11CD=LSOA11CD) %>% ungroup() %>%
    mutate(IMD=ntile(x=IMDrank,
                     n=10))

  removebroken <- tibble(broken=c(rep(0,times=20),1,0,0,1))

  takeout <- tidy %>%group_by(LSOA21CD) %>%
    filter(n()>1) %>%
    group_by(LSOA21CD,RUC11) %>%
    filter(n()==1) %>%
    mutate(RUC11="mismatch") %>%
    cbind(removebroken)
           #remove the weird dupe column where it assigns part of gateshead to northumberland, and vice
           #versa in addition to where they should actually be. This needs to be done by inserting a killswitch


  superrefined <- dplyr::rows_update(#usew the killswitch to remove the broken bits
    tidy %>% mutate(broken=0) %>% anti_join(takeout %>% filter(broken==1),by=c("LSOA21CD","LSOA11CD"))


                                     ,takeout %>% filter(broken==0), by="LSOA11CD")
# Combine the pollution means with the additional data --------------------


  prawns <- inner_join(output,superrefined,by="LSOA21CD")

  if(raster_path!="undefined"){

    renamer <- function(data,last_two_digits_year,pollutant_data_name){

      NamedList <- c("Agricultural","Domestic combustion","Energy production",
                     "Industrial combustion","Industrial production","Natural",
                     "Offshore","Other transport and mobile machinery","Road transport","Solvents","Tot_area","Total",
                     "Waste treatment and disposal")

      Nmdlst <- paste0("mean.",c("agric","domcom","energyprod","indcom","indproc","nature","offshore","othertrans","roadtrans","solvents","totarea","total","waste"),pollutant_data_name,last_two_digits_year)

      core <- tibble(Output=NamedList,Match=Nmdlst)


      for(Index in c(1:17)){
        #if the column name matches any of the renamable vectors then rename it
        if(colnames(data)[Index] %in% core$Match){
          track <- colnames(data)[Index]

          replace <- core %>% filter(Match==track)

          colnames(data)[Index] <- replace$Output
          #Break the loop if there's no match, adds efficiency
        }#ermoved to prevent bad triggers else{break}
        #Close the for loop
      }
      print("Renamed")
      #Output data
      data
      #Close the function

    }

  prawns <- renamer(
    data=prawns,
    last_two_digits_year=year-2000,
    pollutant_data_name = pollutant_data_name
  )
print("Does it get here")
  prawns <- prawns %>% mutate("Point sources"=Total-Tot_area)
  }


# Output the results ------------------------------------------------------
print("Down to the save")
  if (output_path!="undefined"){
    print("Down to the save")
  write.csv(prawns,
            file=output_path)
}

output
  }


