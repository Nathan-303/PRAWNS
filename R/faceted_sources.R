#' A function for faceting the sources of a pollutant
#'
#' This function takes a prawns CSV and produces a summary of the geographic areas
#' matching an inputted parameter
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The name of the pollutant that's being examined, this is used in the graph names
#'
#' @keywords faceted, sources
#' @export
#' @examples
#' faceted_sources()
#'

faceted_sources <- function(prawn_path,pollutant){

long_chunk <- read.csv(file=prawn_path,row.names=1,check.names=FALSE) %>% tibble() %>% mutate(point_sources=Total-Total_no_points)%>%
  pivot_longer(
    cols=c("Agricultural","Domestic combustion","Energy production",
           "Industrial combustion","Industrial production","Natural",
           "Offshore","Other transport and mobile machinery","Road transport","Solvents","Total"
           ,"Waste treatment and disposal","point_sources"),
    names_to = "Emission_source",
    values_to = "emissions")
long_chunk$Emission_source <- as.factor(long_chunk$Emission_source)

long_chunk <- long_chunk %>% mutate(Emission_source=fct_reorder(Emission_source,emissions,mean,.desc=TRUE))

output <- ggplot(data=long_chunk
                          )+

  aes(x=IMD,
      y=emissions
      )+

  facet_wrap(~Emission_source,
             scale="free_y"
             )+

  geom_line(stat="summary",aes(linetype="Mean")
  )+


  geom_smooth(method="lm",formula=y~x,se=FALSE,show.legend=FALSE,aes(linetype="Mean"))+
  #Plot the line of best fit for the median
  geom_quantile(quantiles=0.5,
                aes(linetype="Median"),
                size =1)+

  #Plot a line through the medians for each decile
  geom_line(stat="summary",fun=median,aes(linetype="Median"))+

  labs(x=paste0("IMD decile where 10 is least deprived"),
       y=paste0(pollutant," emissions"),
       title=paste0(pollutant," emissions faceted by source")
  )
 output
}
