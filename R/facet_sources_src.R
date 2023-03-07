#' A function for faceting the sources of a pollutant
#'
#' This function takes a prawns CSV and produces a summary of the geographic areas
#' matching an inputted parameter
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used, should be quoted
#'
#' @param pollutant The name of the pollutant that's being examined, this is used in the graph names, should be a string
#'
#' @param year The year of the data, used in graph labelling
#' @keywords faceted, sources
#' @export
#' @examples
#' faceted_sources()
#'

facet_sources_src <- function(prawn_path,pollutant,year){

long_chunk <- read.csv(file=prawn_path,row.names=1,check.names=FALSE) %>% tibble() %>%

  rename(`Other transport and \nmobile machinery`=`Other transport and mobile machinery`,
         `Waste treatment \nand disposal`=`Waste treatment and disposal`,
         ) %>%

  pivot_longer(
    cols=c("Agricultural","Domestic combustion","Energy production",
           "Industrial combustion","Industrial production","Natural",
           `Other transport and \nmobile machinery`,"Road transport","Solvents","Total"
           ,`Waste treatment \nand disposal`,"Point sources"),
    names_to = "Emission_source",
    values_to = "emissions") %>% group_by(Emission_source)

boxxy <- long_chunk %>% group_by(IMD,Emission_source) %>% summarise(q90=quantile(emissions,c(0.90)),
                                                                      q10=quantile(emissions,c(0.10)),
                                                                      q1=quantile(emissions,c(0.25)),
                                                                      q3=quantile(emissions,c(0.75)),
                                                                      med=quantile(emissions,c(0.5))) %>%
  pivot_longer(cols=c(q90,q10,q1,q3,med),values_to = "emissions")



long_chunk$Emission_source <- as.factor(long_chunk$Emission_source)

long_chunk <- long_chunk %>% group_by(Emission_source) %>%   mutate(Emission_source=fct_reorder(Emission_source,emissions,median,.desc=TRUE))

output <- ggplot(data=long_chunk
                          )+

  aes(x=IMD,
      y=emissions)+

  facet_wrap(~fct_reorder(Emission_source,emissions,mean,na.rm=TRUE,.desc=TRUE),
             scale="free_y"
             )+

  geom_boxplot(data=boxxy,
               inherit.aes=FALSE,
               aes(x=IMD,
                   y=emissions,
                   group=IMD),
               coef=10000000000000000000000000000000000000000000000000000000000000)+

geom_line(stat="summary",
          aes(linetype="Average points",colour="Mean"),

          fun=mean,
          na.rm=TRUE
)+


  geom_smooth(method="lm",
              formula=y~x,
              se=FALSE,
              aes(linetype="Linear regression",colour="Mean"),

              size=1,
              na.rm = TRUE)+

#Plot the line of best fit for the median
geom_quantile(quantiles=0.5,
              aes(linetype="Linear regression",colour="Median"),

              formula=y~x,
              size=1,
              na.rm=TRUE)+



  #Plot a line through the medians for each decile
  geom_line(stat="summary",
            fun=median,
            aes(linetype="Average points",colour="Median"),

            na.rm=TRUE)+

  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+

  scale_colour_manual(values = c("Median"="royalblue","Mean"="#FB8022FF"),
                      name= "Average used")+


  scale_linetype_manual(name= "Line plotted:",
                          values = c("Linear regression"="solid","Average points"="dashed"),
                          guide=guide_legend(override.aes = list(linetype=c("solid","dashed"),colour="black",shape=c(NA,NA),size=c(1,1)))
                      )+

  labs(x=paste0("IMD decile where 10 is least deprived"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
                title=NULL
  )+
  theme(legend.position = "bottom",legend.key.width = unit(1.5,"cm"))




 output
}