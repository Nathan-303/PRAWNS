#' Plot modelled data
#'
#' @param model_prawn_path the filepath to the prawn csv file that contains only rhe target region
#' @param pollutant the pollutant analysed, used for naming axis
#' @export
#' @examples
#' model_plot()
model_plot <- function(model_prawn_path,pollutant,year){

active_stack <- read.csv(model_prawn_path,
                                 check.names = FALSE,
                                 row.names = 1)

part_boxxy <- active_stack %>% group_by(IMD) %>% summarise(q90=quantile(poll_mean,c(0.90),na.rm=TRUE),
                                                          q10=quantile(poll_mean,c(0.10),na.rm=TRUE),
                                                          q1=quantile(poll_mean,c(0.25),na.rm=TRUE),
                                                          q3=quantile(poll_mean,c(0.75),na.rm=TRUE),
                                                          med=quantile(poll_mean,c(0.5),na.rm=TRUE))

boxxy <- part_boxxy%>%
  pivot_longer(cols=c(q90,q10,q1,q3,med),values_to = "Emissions")


focused_long_prawn <- active_stack %>% pivot_longer(cols=c("poll_mean"),
                                                           values_to = "Emissions",
                                                           names_to="Source")

focused_window <- ggplot(data = focused_long_prawn)+
  aes(x=IMD,
      y=Emissions)+
  geom_boxplot(data=boxxy,
               aes(x=IMD,group=IMD,y=Emissions),
               coef=3,
               show.legend = FALSE) +

  geom_quantile(quantiles=0.5,
                linewidth =1,
                formula=y~x,
                aes(linetype="Median",
                    colour="median"))+

  geom_smooth(data=focused_long_prawn,
              method="lm",
              formula=y~x,
              se=FALSE,
              show.legend=FALSE,
              aes(x=IMD,
                  y=Emissions,
                  linetype="Mean",
                  colour="mean"),
              )+

  geom_line(data=focused_long_prawn,
            stat="summary",
            aes(x=IMD,
                y=Emissions,
                linetype="Mean points",
                colour="mean"),
            linewidth=1)+

  scale_linetype_manual(values=c("Median"="solid","Mean"="dashed","Mean points"="dotted"),name = "Line plotted")+


  labs(x=paste0("IMD decile where 10 is least deprived"),
       y=bquote("Modelled background"~.(pollutant)~"concentration"~.(year)~"/ ug "~m^3),
       title=NULL)+

  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0.1),
    minor_breaks = FALSE) +

  scale_colour_manual(breaks = c("mean","median"),
                                               values=c("#FB8022FF","royalblue"),
                                               name="Average used")
focused_window

}
