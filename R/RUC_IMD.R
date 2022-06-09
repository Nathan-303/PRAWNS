#Runs the startup file to load in all the required libraries, a simple if statement 
#stops it being run if the code has already been used
if(!exists("Started")){
  source("C:/Users/Nathan Gray/Documents/GitHub/Pollutant-processing-hub/Scripts/Startup.R")
  Started <- TRUE
}

active_stack <- Stack_seeker("NOx",2019) %>%   
  
  #mutate in the columns you want (removing natural NOx)
  mutate("Point sources" =Total-Total_no_points) %>% 
  tibble() %>% 

replace_na(list(RUC11="Unclassified"))

#Make the data long to eneble grouping by source
temp <- active_stack %>% 
  tibble() %>% 

  select(decile = Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs.,
         NOx_emissions=Total,
         Classification=RUC11) %>% 
  group_by(Classification,
           decile) %>% 
  summarise(Mean = mean(NOx_emissions, na.rm = T), 
            Median = median(NOx_emissions, na.rm =T )) %>% 
  pivot_longer(c(Mean, Median), names_to = "stat")



RUC_summary <- ggplot(temp)+
  geom_smooth(aes(decile,
                  value,
                  colour = Classification
  ),
  method = "lm",
  se=FALSE,
  size=1)+
  geom_line(aes(decile,
                value,
                colour = Classification
  ),
  size=0.5)+
  
  facet_wrap(~stat)+
  
labs(x="IMD decile where 10 is least deprived",
     y="Average NOx emissions/tonnes km^2",
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