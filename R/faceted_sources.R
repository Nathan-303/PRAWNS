#' A function for faceting the sources of a pollutant
#'
#' This function takes a prawns CSV and produces a summary of the geographic areas
#' matching an inputted parameter
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used, should be quoted
#'
#' @param pollutant The name of the pollutant that's being examined, this is used in the graph names, should be a string
#'
#' @keywords faceted, sources
#' @export
#' @examples
#' faceted_sources()
#'

faceted_sources <- function(prawn_path,pollutant,year){

long_chunk <- read.csv(file=prawn_path,row.names=1,check.names=FALSE) %>% tibble() %>%

  mutate(point_sources=Total-Total_no_points)%>%

  rename(`Other transport and \nmobile machinery`=`Other transport and mobile machinery`,
         `Waste treatment \nand disposal`=`Waste treatment and disposal`,
         ) %>%

  pivot_longer(
    cols=c("Agricultural","Domestic combustion","Energy production",
           "Industrial combustion","Industrial production","Natural",
           `Other transport and \nmobile machinery`,"Road transport","Solvents","Total"
           ,`Waste treatment \nand disposal`,"Point sources"),
    names_to = "Emission_source",
    values_to = "emissions")

long_chunk$Emission_source <- as.factor(long_chunk$Emission_source)

long_chunk <- long_chunk %>% group_by(Emission_source) %>%   mutate(Emission_source=fct_reorder(Emission_source,emissions,median,.desc=TRUE))

output <- ggplot(data=long_chunk
                          )+

  aes(x=IMD,
      y=emissions)+

  facet_wrap(~fct_reorder(Emission_source,emissions,mean,na.rm=TRUE,.desc=TRUE),
             scale="free_y"
             )+

  geom_line(stat="summary",
            aes(colour="Mean",linetype="Average points"),
            fun=mean,
            na.rm=TRUE
  )+


  geom_smooth(method="lm",
              formula=y~x,
              se=FALSE,
              aes(colour="Mean",linetype="Linear regression"),
              na.rm = TRUE)+

  #Plot the line of best fit for the median
  geom_quantile(quantiles=0.5,
                aes(colour="Median",linetype="Linear regression"),
                formula=y~x,
                na.rm=TRUE)+



  #Plot a line through the medians for each decile
  geom_line(stat="summary",
            fun=median,
            aes(colour="Median",linetype="Average points"),
            na.rm=TRUE)+

  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+

  scale_colour_manual(values = c("Median"="blue","Mean"="orange"),
                      name= "Average used")+


  scale_linetype_identity(name= "Line plotted",
                          breaks=c(1,2)
                        labels =c("Linear regression","Average points")
                      # breaks=c(1,2),
                      # labels=c("Average points","Linear regression"),
                      # guide="legend"
                      )+

  labs(x=paste0("IMD decile where 10 is least deprived"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^2),
                title=NULL
  )+
  theme(legend.position = "bottom")

 output
}
