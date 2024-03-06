library(PRAWNSdata)
pollutant <- "NOx"
#loop for the years
for(year in c(2010:2020)){

  filename <- paste0(year,
                     "_",
                     pollutant,
                     "_PRAWN.csv")
  #
  selected_key <- pollutant_key %>% filter(Pollutant==pollutant)
  #check if the final file has already been made
  if(file.exists(filename)==FALSE){
    #download the file
    #Download the pollutant emission maps
    file_url=paste0(
      "https://naei.beis.gov.uk/mapping/mapping_",
      year,
      "/",
      selected_key$ID.number,
      ".zip")

    ziploc <- paste0(
      "Data/Autodownloads/",
      year,
      "_",
      pollutant,".zip")
    #Download the zip to a temporary location
    download.file(url=file_url,
                  destfile=here::here(ziploc))
                  )



    selected_key <- pollutant_key %>% filter(Pollutant==input$Pollutant)

    incProgress(amount=0.1,detail="Downloading emissions maps")

    input_prawn <- process_tempura_prawns(
      raster_path = here::here(ziploc),
      pollutant_data_name = selected_key$ID.string,
      year = as.numeric(input$year),
      pollutant = input$Pollutant,
      output_path = here::here(paste0(input$Pollutant," emissions in ",input$year,"PRAWN.csv"))
    )

    #create prawns
    process_create_set_prawns(
      year=year,
      raster_path = ,


    )

  }
}
