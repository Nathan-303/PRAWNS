#' summarises the source sectors and their relationship with IMD in the data specified
#'
#'@param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The name of the pollutant that's being examined, this is used in the graph names
#'
#'@param year The year used, used in graph names
#'
#'@export
#'source_summary

source_summary <- function(prawn_path,pollutant,year){

long_chunk <- read.csv(file=prawn_path,
         row.names=1,
         check.names=FALSE) %>%
  tibble() %>%

  mutate(point_sources=Total-Total_no_points)%>%

  pivot_longer(
    cols=c("Agricultural","Domestic combustion","Energy production",
           "Industrial combustion","Industrial production","Natural",
           "Offshore","Other transport and mobile machinery","Road transport","Solvents","Total"
           ,"Waste treatment and disposal","point_sources"),
    names_to = "Emission_source",
    values_to = "emissions")
long_chunk$Emission_source <- factor(long_chunk$Emission_source)

long_chunk <- long_chunk %>% mutate(Emission_source=fct_reorder(Emission_source,emissions,mean,.desc=TRUE))

output <- Decile_vs_emission_by_variable(
  active_stack = long_chunk,
  chosen_decile = IMD,
  chosen_grouping = Emission_source,
  xaxis = "IMD Decile",
  yaxis = paste0(pollutant," emissions"),
  title = paste0("Source breakdown for ",pollutant, " emissions in ", year),
  chosen_variable = emissions,
  Pollutant = pollutant)+

  labs(linetype= "Metric used",
       colour= "Emission source")



output
}
