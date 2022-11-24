#' Create a graph showing how size affects emissions

#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @keywords faceted, sources
#' @export
#' @examples
#' size_poker()
#' 

size_poker <- function(prawn_path,pollutant,year){
  data <- read.csv(prawn_path,
                   row.names=1,
                   check.names=FALSE)
  
  reformed_data <- data %>% 
    #aggregate all non
    mutate(Other_sources=Solvents+Natural+`Energy production`+`Waste treatment and disposal`+Agricultural+
             "Point sources"+"Domestic combustion"+"Industrial combustion"+"Industrial production"+
           "Other transport and mobile machinery") %>%
    
    pivot_longer(
      cols=c("Road transport","Total","Other_sources"),
      names_to = "Emission_source",
      values_to = "emissions") %>% 
    #sort into thirds based on IMD rank  
  mutate(third=ntile(Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.,3))
  #round the data to the decimal places you want
  
  
  
  
}