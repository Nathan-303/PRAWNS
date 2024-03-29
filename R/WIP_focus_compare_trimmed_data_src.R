#' Compare a trimmed data set with the whole, this function doesn't cover the
#' trimming, provide it with two prawns and it'll do the work
#'
#' @param focused_prawn_path the filepath to the prawn csv file that contains
#' only the target region
#'
#' @param base_prawn_path the filepath to the prawn csv with the whole data set,
#'  used to plot an average line for comparison
#'
#' @param year The year the data is from, used to generate axis titles
#'
#' @param pollutant The pollutant investigated, used to generate axis titles
#' @export
#' @examples
#' focused_plot()
WIP_focus_compare_trimmed_data_src <- function(focused_prawn_path,base_prawn_path,pollutant,year){

active_stack <- read.csv(focused_prawn_path,
                                 check.names = FALSE,
                                 row.names = 1)

allthings <- read.csv(base_prawn_path,
                                       check.names = FALSE,
                                       row.names = 1)
part_boxxy <- active_stack %>% group_by(IMD) %>% summarise(q90=quantile(Total,c(0.90)),
                                                          q10=quantile(Total,c(0.10)),
                                                          q1=quantile(Total,c(0.25)),
                                                          q3=quantile(Total,c(0.75)),
                                                          med=quantile(Total,c(0.5)))

boxxy <- part_boxxy%>%
  pivot_longer(cols=c(q90,q10,q1,q3,med),values_to = "Emissions")


focused_long_prawn <- active_stack %>% pivot_longer(cols=c("Total"),
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
                aes(colour="London",
                    linetype="Median"))+

  geom_quantile(data=allthings,
                quantiles=0.5,
                linewidth =1,
                formula=y~x,
                aes(x=IMD,
                    y=Total,
                    colour="Outside\nLondon",
                    linetype="Median"))+

  geom_smooth(data=focused_long_prawn %>% filter(Source=="Total"),
              method="lm",
              formula=y~x,
              se=FALSE,
              show.legend=FALSE,
              aes(x=IMD,
                  y=Emissions,
                  linetype="Mean",
                  colour="London"),
              )+

  geom_smooth(data=allthings,
              method="lm",
              formula=y~x,
              se=FALSE,
              show.legend=FALSE,
              aes(x=IMD,
                  y=Total,
                  linetype="Mean",
                  colour="Outside\nLondon"),
              )+

  geom_point(data=focused_long_prawn %>% filter(Source=="Total"),
            stat="summary",
            aes(x=IMD,
                y=Emissions,
                colour = "London"),
            show.legend = FALSE,
            shape="cross",
            size=1.8,
            stroke=1.3)+

  scale_linetype_manual(values=c("Median"="dashed","Mean"="solid"),name = "Line plotted")+

  scale_colour_manual(breaks = c("London","Outside\nLondon"),
                      values=c("#FB8022FF","royalblue"),
                      name="LSOA\nlocation")+

  labs(x=paste0("IMD decile where 10 is least deprived"),
       y=bquote(.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       title=NULL)+

  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0.1),
    minor_breaks = FALSE)
focused_window

}
