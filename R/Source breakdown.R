side_by_side_stats() <- function(pollutant,year,prawn_path){

active_stack <- read.csv(prawn_path)

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

london_free <- filter(long_stack,!TCITY15NM=="London")

temp = bind_rows(
  long_stack %>% 
    mutate(justLondon = "With London"),
  london_free %>% 
    mutate(justLondon = "Without London")
) %>% 
  select(decile = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.,
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
       y="Average NOx emissions/tonnes km^2",
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
  
  scale_colour_viridis_d(option="turbo")

source_summary

ggsave(filename="Outputs/Plots/NOx/NOX_source_summary_2019_turbopalette.png",
       plot=source_summary,
       units = "mm",height = 339,width=480,
       device="png")

}
             