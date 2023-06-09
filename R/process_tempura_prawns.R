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
                          shapefile_path="Data/2011_LSOA_shapefile_20m_generalised",
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
  filelist <- grep('\\.asc$', unzip((raster_path), list=TRUE)$Name,
                   ignore.case=TRUE, value=TRUE)
  }

print("vect call")
  #Beware recursion

  vectorised_shapefile <- vect(LSOA_shapefile)

  output <- tibble(LSOA11CD=vectorised_shapefile$LSOA11CD)

print("still alive")

index <- c(1:length(vectorised_shapefile))

transient <- sf::st_as_sf(vectorised_shapefile[index])
  for(source_number in 1:length(filelist)){
    incProgress(1/length(filelist), detail = paste("Processing rasters this will take a while, currently on source ", source_number, " of ",length(filelist) ))
    #unzip only the layer being extracted to minimise memory use, skip offshore
    transient_raster <- unzip(zipfile = raster_path,
                              files=filelist[source_number],
                              exdir=here::here(tempdir())) %>%
      rast()%>%
      terra::subst(NA,0)


  pollution_mean <- exact_extract(transient_raster,transient,'mean') %>%
    tibble() %>% signif(digits=4)

  unlist(here::here("tempdir"))
  #rename the column for smoother binding, dplyr rename not used becaus eit was being awkwa
  colnames(pollution_mean)[1]=filelist[source_number]

  output <- pollution_mean %>% bind_cols(output)
  }
  rm(vectorised_shapefile)
  rm(index)
  rm(transient)

# Refined_chunk is obtained from PRAWNSdata----------------------------------------------------------------

  prawns <- inner_join(output,refined_chunk,by="LSOA11CD")%>%
    rename(IMD=`Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.`)

  rm(output)

  if(raster_path!="undefined"){

    renamer <- function(data,last_two_digits_year,pollutant_data_name){

      NamedList <- c("Agricultural","Domestic combustion","Energy production",
                     "Industrial combustion","Industrial production","Natural",
                     "Offshore","Other transport and mobile machinery","Road transport","Solvents","Tot_area","Total",
                     "Waste treatment and disposal")

      Nmdlst <- paste0(c("agric","domcom","energyprod","indcom","indproc","nature","offshore","othertrans","roadtrans","solvents","totarea","total","waste"),
                       pollutant_data_name,last_two_digits_year,
                       ".asc")

      core <- tibble(Output=NamedList,Match=Nmdlst)


      for(Index in c(1:17)){
        #if the column name matches any of the renamable vectors then rename it
        if(colnames(data)[Index] %in% core$Match){
          track <- colnames(data)[Index]

          replace <- core %>% filter(Match==track)

          colnames(data)[Index] <- replace$Output

        }
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
  ) %>% dplyr::select(-any_of(c("Tot_area","Offshore")))

  prawns <- prawns %>% mutate("Point sources"=Total-Tot_area)
  }


# Output the results ------------------------------------------------------

prawns
  }


