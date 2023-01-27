#' Make a nice graph
#'
#' @param year the year the data was taken, used for axis titles
#' @param pollutant the pollutant being investigated, this should be in quotes and correspond with its name in the data
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#` @keywords faceted, sources
#' @export
#' @examples
#' side_by_sidestats()
#'
streamlined_stats <- function(pollutant,year,prawn_path=1,input=1){

  active_stack <- input %>%


  #mutate in the columns you want (removing natural NOx)
  mutate("Industry"=`Energy production`+`Industrial combustion`+`Industrial production`+`Point sources`,
         "Other sources"=Solvents+Natural+Agricultural+`Waste treatment and disposal`) %>%
  tibble()

#Make the data long to eneble grouping by source
long_stack <- active_stack %>% rename(`Other transport and \nmobile machinery`=`Other transport and mobile machinery`) %>%
  pivot_longer(
    cols=c("Domestic combustion",
           "Other transport and \nmobile machinery","Road transport","Total","Other sources","Industry"
           ),
    names_to = "Emission_source",
    values_to = "NOx_emissions"

  ) %>%
  tibble()

long_stack <- long_stack %>% mutate(NOx_emissions=replace_na(NOx_emissions,0))

long_stack$Emission_source <- factor(long_stack$Emission_source)

long_stack <- long_stack %>% mutate(Emission_source=fct_reorder(Emission_source,NOx_emissions,mean,.desc=TRUE))

temp = long_stack  %>%
  dplyr::select(decile = IMD,
         NOx_emissions,
         Emission_source) %>%
  group_by(Emission_source,
           decile) %>%
  summarise(Mean = mean(NOx_emissions, na.rm = T),
            Median = median(NOx_emissions, na.rm =T )) %>%
  pivot_longer(c(Mean, Median), names_to = "stat")



source_summary <- ggplot(temp)+
  geom_smooth(aes(decile,
                  value,
                  colour = Emission_source,
                  ),
              linewidth=1.2,
              method = "lm",
              se=FALSE,
              )+
  geom_line(aes(decile,
                 value,
                 colour = Emission_source,
                ),
            linewidth=0.6)+

facet_wrap(~stat)+

  labs(x="IMD decile where 10 is least deprived",
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^2),
       linetype="Dataset",
       colour="Emission source")+

  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+

  scale_y_continuous(expand=c(0,0))+

  scale_colour_manual(values=c("black","royalblue","olivedrab1","#FB8022FF","deeppink2","grey"))+

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
