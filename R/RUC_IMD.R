#' A function for looking at the relationship between IMD and emissions for
#' rural urban classifications. The output is a list with two entries output1 is
#' a graph of the emissions by decile for each classification, output 2 is their
#'  populations
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The name of the pollutant that's being examined, this is used in the graph names
#'
#' @keywords RUC
#' @export
#' @examples
#' RUC_IMD()
#'

RUC_IMD <- function(prawn_path,
                    pollutant,
                    year){
#Read in the data with NA valuse changed to unclassified, raising red flags if necessary
active_stack <-read.csv(prawn_path) %>% mutate(across(RUC11,str_replace_all,c(
  "Rural town and fringe in a sparse setting"="Rural town and fringe \nin a sparse setting",
  "Rural village and dispersed in a sparse setting"= "Rural village and dispersed \nin a sparse setting",
  "Urban city and town in a sparse setting"="Urban city and town \nin a sparse setting")))%>%
  replace_na(list(RUC11="Unclassified"))

#Make the data long to enable grouping by source
temp <- active_stack %>%
  tibble() %>%

  dplyr::select(decile = IMD,
         Emissions=Total,
         Classification=RUC11) %>%
        #group by the classification for graphing
         group_by(Classification)

RUC_linear_model <- temp %>%
  #get the rsquared
  do(glance(lm(Emissions~decile, data=.)))


RUC_summary <- ggplot(temp)+
  aes(x=decile,
      y=Emissions,
      colour=fct_reorder(Classification,Emissions,.desc=TRUE))+

  geom_line(stat="summary",
            aes(linetype="Mean"),
            fun=mean)+


  geom_smooth(method="lm",
              formula=y~x,
              se=FALSE,
              show.legend=FALSE,
              aes(linetype="Mean"))+

  #Plot the line of best fit for the median
  geom_quantile(quantiles=0.5,
                aes(linetype="Median"),
                size =1,
                formula=y~x)+

  #Plot a line through the medians for each decile
  geom_line(stat="summary",
            fun=median,
            aes(linetype="Median"),
            )+

labs(x="IMD decile where 10 is least deprived",
     y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^2),
     linetype="Average used",
     colour= "RUC classification",
     title=NULL)+

guides(linetype=guide_legend(override.aes =list(linetype=c("solid","dashed"),
                                                colour="black",
                                                shape=c(NA,NA),
                                                size=c(1,1)),
       keywidth = 3),
       colour=guide_legend(byrow=TRUE))+

  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+

  scale_colour_viridis_d(option = "turbo")

Area_population <- ggplot(data=active_stack)+


  aes(x=RUC11,fill=RUC11 )+
  geom_bar()+
  theme(
         axis.text.x = element_blank(),

         axis.ticks = element_blank(),

         legend.spacing.y = unit(1,"cm"))

dec_histo <- ggplot(data=active_stack)+aes(x=IMD)+facet_wrap(~RUC11)+geom_bar()

dec_histo2 <- ggplot(data=active_stack)+aes(x=IMD)+facet_wrap(~RUC11,scale="free_y")+geom_bar()

output <- list(RUC_summary,Area_population,dec_histo,dec_histo2,RUC_linear_model)

output
}
