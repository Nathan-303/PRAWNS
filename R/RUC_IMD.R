#' A function for looking at the relationship between IMD and emissions for
#' rural urban classifications
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param targets The name of the thing you're trying to isolate e.g. "Manchester"
#' can also be a vector of valid values
#'
#' @param pollutant The name of the pollutant that's being examined, this is used in the graph names
#'
#' @param output_path The filepath to output to, a folder will be created at
#' this location which contains all the graphs produced by this code. Defaults
#' to FALSE

#' @keywords RUC
#' @export
#' @examples
#' RUC_IMD()
#'
if(!exists("Started")){
  source("C:/Users/Nathan Gray/Documents/GitHub/Pollutant-processing-hub/Scripts/Startup.R")
  Started <- TRUE
}

#Read in the data with NA valuse changed to unclassified, raising red flags if necessary
active_stack <-read.csv(prawn_path) %>%
  replace_na(list(RUC11="Unclassified"))

#create a summary of the data at the first decile to sort the legend using
first_decile <- filter(active_stack,IMD==4) %>%
  group_by(RUC11) %>%
  summarise(mean=mean(Total)) %>%
  mutate(mean=as.factor(mean))

#Make the data long to enable grouping by source
temp <- active_stack %>%
  tibble() %>%

  select(decile = IMD,
         Emissions=Total,
         Classification=RUC11) %>%
        #group by the classification for graphing
         group_by(Classification)


RUC_summary <- ggplot(temp)+
  aes(x=decile,
      y=Emissions,
      colour=fct_reorder(Classification,Emissions,.desc=TRUE))+
  geom_line(stat="summary",aes(linetype="Mean")
  )+


  geom_smooth(method="lm",formula=y~x,se=FALSE,show.legend=FALSE,aes(linetype="Mean"))+
  #Plot the line of best fit for the median
  geom_quantile(quantiles=0.5,
                aes(linetype="Median"),
                size =1)+

  #Plot a line through the medians for each decile
  geom_line(stat="summary",fun=median,aes(linetype="Median"))+

labs(x="IMD decile where 10 is least deprived",
     y=paste0("Average ",pollutant," emissions/tonnes km^2"),
     title="NOx emissions by RUC classification 2019")+

  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)

ggsave(filename="Outputs/Plots/NOx/RUC_summary_2019.png",
       plot=RUC_summary,
       units = "mm",height = 113,width=160,
       device="png")
Area_population <- ggplot(data=active_stack)+

  aes(x=RUC11,fill=RUC11 )+
  geom_bar()+
  labs(
       title="population distribution")+
  theme(
         axis.text.x = element_blank(),

         axis.ticks = element_blank())


ggsave(filename="Outputs/Plots/NOx/NOX_source_summary_2019.png",
       plot=Area_population,
       units = "mm",height = 113,width=160,
       device="png")
