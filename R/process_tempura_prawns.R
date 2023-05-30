#' A variant of create_prawns which uses the PRAWNSdata package to access
#' static data in a preprocessed form, enabling use of the tempura gui on
#' shinyapps`
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
#' process_tempura_prawns(
#'   raster_path="The folder containing the rasters",
#'   shapefile_path="Where the shapefiles are",
#'   pollutant_data_name="nox",
#'   year="2019",
#'   pollutant="NOx")

process_tempura_prawns <- function(raster_path="undefined",
                          tif_path="undefined",
                          csv_coordinates_path="undefined",
                          shapefile_path,
                          output_path="undefined",
                          pollutant_data_name,
                          year,
                          pollutant,
                          is_raw=FALSE){

# Calculate the average pollution for each area ----------------------------


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
    base_raster <- rasterFromXYZ(csv_raster,crs="OSGB")
  print("raster next")
    source_stack <- rast(base_raster)
  }
print("vect call")
  #Beware recursion
  vectorised_shapefile <- vect(LSOA_shapefile)
print("index call")
  #Calculate the average for each polygon in the shapefile
  index <-  c(1:length(vectorised))
  print("stftft call")
  transient <- sf::st_as_sf(vectorised[index])
  print("exactextract call")
  pollution_mean <- exact_extract(source_stack,transient,'mean')
  #Output the results as a tibble containing the indexed position, the pollution mean and the LSOA code, a property from the shapefile that enables binding on LSOA statistics
  print("output call")
  output <- tibble(poll_mean=pollution_mean,
                   LSOA11CD=LSOA_shapefile$LSOA11CD,
                   expanse=expanse(LSOA_shapefile)
                   ) %>% unnest(poll_mean)

  #output a csv with minimum processing
  if(output_path!="undefined"){
    print("outputting?")
    if (is_raw==TRUE){
      print("writing file")
    write.csv(file=output_path,
              x = output)

      output
    }else{

        write.csv(file=output_path,
                    x = output)}}


# Refined_chunk is obtained from PRAWNSdata----------------------------------------------------------------

  prawns <- inner_join(output,refined_chunk,by="LSOA11CD")%>%
    rename(IMD=`Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.`)

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
        }else{break}
        #Close the for loop
      }
      #Output data
      data
      #Close the function
    }

  prawns <- renamer(
    data=prawns,
    last_two_digits_year=year-2000,
    pollutant_data_name = pollutant_data_name
  )

  prawns <- prawns %>% mutate("Point sources"=Total-Tot_area)
  }


# Output the results ------------------------------------------------------
print("Down to the save")
  if (output_path!="undefined"){
    print("Down to the save")
  write.csv(prawns,
            file=output_path)
}

TRUE
  }


