library(PRAWNSdata)
pollutant <- "NOx"


#read the table used for assigning years and shapefiles
setlinks <- read.csv(file="Data/Historic_stats/YearIMDCensus.csv")
#loop for the years
for(year in c(2010:2020)){
#select the correct protion of the lookup table
  chosenset <- setlinks %>% dplyr::filter(Year==year)
  filename <- paste0("Data/Historic_PRAWNS/",
                     year,
                     "_",
                     pollutant,
                     "_PRAWN.csv")
  #
  selected_key <- pollutant_key %>% filter(Pollutant==pollutant)


  #check if the final file has already been made
  if(file.exists(filename)==FALSE){
    ziploc <- paste0(
      "Data/Autodownloads/",
      year,
      "_",
      pollutant,".zip")
    #check if the emission maps are downloaded
    if(file.exists(ziploc)==FALSE){
    #Download the pollutant emission maps
    file_url=paste0(
      "https://naei.beis.gov.uk/mapping/mapping_",
      year,
      "/",
      selected_key$ID.number,
      ".zip")

    #Download the zip to a temporary location
    download.file(url=file_url,
                  destfile=(ziploc))
    }


    input_prawn <- process_create_set_prawns(
      raster_path = ziploc,
      pollutant_data_name = selected_key$ID.string,
      year = year,
      pollutant = pollutant,
      output_path = paste0(pollutant," emissions in ",year,"PRAWN.csv"),
      shapefile_path = paste0("Data/Historic_stats/",
                              as.character(chosenset$LSOAs),
                              "_LSOA")
    )

  write.csv(x=input_prawn,
            file=filename)



  }
}

