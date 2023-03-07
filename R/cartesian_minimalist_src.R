#' Make a minimalist graph suitable for a graphical abstract
#'
#' @param prawn_path the filepath to load data from
#' @param pollutant the pollutant investigated, used to name axis
#' @param year the year the data is from, used to name axis
#' @export
#' @examples
#' minimalist_graph()

cartesian_minimalist_src <- function(prawn_path,pollutant,year){

  data <- read.csv(prawn_path,
                   row.names=1,
                   check.names=FALSE)

  reformed_data <- data %>%
    #aggregate some sources so it can be small
    mutate(`Industrial sources`=Solvents+
             `Energy production`+
             `Waste treatment and disposal`+
             `Industrial combustion`+
             `Industrial production`+
             `Point sources`,

           `Other sources`=Natural+
             `Agricultural`+
             `Other transport and mobile machinery`+
             `Domestic combustion`) %>%

    pivot_longer(
      cols=c("Road transport","Total","Other sources","Industrial sources"),
      names_to = "Emission_source",
      values_to = "emissions")



    source_summary <- ggplot(reformed_data)+
    geom_smooth(aes(IMD,
                    emissions,
                    colour = fct_reorder(Emission_source,emissions,mean,.desc=TRUE),
                    linetype ="Linear regression"),
    method = "lm",
    se=FALSE)+

  geom_line(data=reformed_data %>% group_by(IMD,Emission_source)%>%
              summarise(emissions=mean(emissions)),
              aes(x=IMD,
                y=emissions,
                colour = Emission_source,
                linetype="Average points"))+

    labs(x="IMD decile where 10 is least deprived",
         y=bquote(.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
         colour="Emission source")+

    scale_x_continuous(
      breaks=c(1:10),
      expand = expansion(mult=0,add=0),
      minor_breaks = FALSE)+

    scale_y_continuous(expand=c(0,0))+

    scale_linetype_manual(values=c("Average points"="dashed","Linear regression"="solid"),
                          name="Values plotted")+

#
#     scale_linewidth(name= "Line plotted",
#                         labels=c("Average points","Linear regression"),
#                         guide="legend")+

    scale_colour_manual(values=c("black","royalblue","olivedrab1","#FB8022FF"))+

      guides(colour=guide_legend(override.aes=list(size=3)),

             linetype=guide_legend(override.aes =list(colour="black",
                                                      shape=c(NA,NA),
                                                      linewidth=c(1,1)))
      )+

    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"))

  source_summary


}
