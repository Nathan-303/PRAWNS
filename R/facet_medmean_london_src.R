#' Plot the mean and median emissions with and without London next to each other
#'
#' @param year the year the data was taken, used for axis titles
#'
#' @param pollutant the pollutant being investigated, this should be in quotes
#' and correspond with its name in the data
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @keywords faceted, sources
#'
#' @export
#'
#' @examples
#' side_by_sidestats(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019
#'   )

facet_medmean_london_src <- function(pollutant,year,prawn_path){

active_stack <- read.csv(file=prawn_path,
                         check.names = FALSE,
                          row.names = 1)%>%


  #mutate in the columns you want (removing natural NOx)
  mutate(
         "Other sources"=Solvents+Natural+Agricultural+`Waste treatment and disposal`+`Energy production`+`Industrial combustion`+`Industrial production`+`Point sources`) %>%
  tibble()

#Make the data long to eneble grouping by source
long_stack <- active_stack %>% rename(`Other transport and \nmobile machinery`=`Other transport and mobile machinery`) %>%
  pivot_longer(
    cols=c("Domestic combustion",
           "Other transport and \nmobile machinery","Road transport","Total","Other sources"
           ),
    names_to = "Emission_source",
    values_to = "NOx_emissions"

  ) %>%
  tibble()

long_stack <- long_stack %>% mutate(NOx_emissions=replace_na(NOx_emissions,0))

long_stack$Emission_source <- factor(long_stack$Emission_source)

long_stack <- long_stack %>% mutate(Emission_source=fct_reorder(Emission_source,NOx_emissions,mean,.desc=TRUE))

london_free <- filter(long_stack,!TCITY15NM=="London") %>%
  mutate(IMD=ntile(Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.,10))

temp = bind_rows(
  long_stack %>%
    mutate(justLondon = "With London"),
  london_free %>%
    mutate(justLondon = "Without London")
) %>%
  dplyr::select(decile = IMD,
         NOx_emissions,
         justLondon,
         Emission_source) %>%
  group_by(justLondon,
           Emission_source,
           decile) %>%
  summarise(Mean = mean(NOx_emissions, na.rm = T),
            Median = median(NOx_emissions, na.rm =T )) %>%
  pivot_longer(c(Mean, Median), names_to = "stat")



source_summary <- ggplot(temp)+
  geom_smooth(aes(decile,
                  value,
                  colour = Emission_source,
                  linetype = justLondon,
                  size=2
                  ),
              method = "lm",
              se=FALSE,
              )+
  geom_line(aes(decile,
                 value,
                 colour = Emission_source,
                 linetype = justLondon,
                size=1
                ))+

  facet_wrap(~stat)+

  labs(x="IMD decile where 10 is least deprived",
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       linetype="Dataset",
       colour="Emission source")+

  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+

  scale_y_continuous(expand=c(0,0))+

  theme(text=element_text(size=30),
        panel.spacing.x = unit(3,"lines"),
        legend.key.size = unit(2,"lines"),
        legend.key.height = unit(4,"lines")
        )+

  scale_size_identity(name= "Line plotted",
                      breaks=c(1,2),
                      labels=c("Average points","Linear regression"),
                      guide="legend")+

  scale_colour_manual(values=c("black","royalblue","olivedrab1","#FB8022FF","deeppink2"))+

  guides(colour=guide_legend(override.aes=list(size=3)),

         linetype=guide_legend(override.aes =list(linetype=c("solid","dashed"),
                                                  colour="black",
                                                  shape=c(NA,NA),
                                                  linewidth=c(1,1)))
  )+

  theme(panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

source_summary

}
