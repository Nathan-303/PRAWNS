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

RUC_IMD_targeted <- function(prawn_path,
                    pollutant){
#Read in the data with NA valuse changed to unclassified, raising red flags if necessary
active_stack <-read.csv(prawn_path) %>%
  replace_na(list(RUC11="Unclassified"))

#Make the data long to enable grouping by source
temp <- active_stack %>%
  tibble() %>%

  dplyr::select(decile = IMD,
         Emissions=Total,
         Classification=RUC11) %>%
        #group by the classification for graphing
         group_by(Classification)


RUC_summary <- ggplot(temp)+
  aes(x=decile,
      y=Emissions,
      colour=fct_reorder(Classification,Emissions,.desc=TRUE))+

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
     y=paste0("Average ",pollutant," emissions/tonnes km^2"),
     title=paste0(pollutant," emissions by RUC classification 2020"),
     linetype="Metric used",
     colour= "RUC classification")+

  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+


  scale_y_continuous(breaks=c(0,3,6,9,12,15),expand = expansion(mult=0,add=0))
  coord_cartesian(ylim=c(0,15),expand = FALSE,)


Area_population <- ggplot(data=active_stack)+

  aes(x=RUC11,fill=RUC11 )+
  geom_bar()+
  labs(
       title="population distribution")+
  theme(
         axis.text.x = element_blank(),

         axis.ticks = element_blank())+

  scale_colour_viridis_d("turbo")

output <- list(RUC_summary,Area_population)

output
}
