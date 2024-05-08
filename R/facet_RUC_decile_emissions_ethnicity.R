#' Bind ethnicity data to the prawn and create graphs based on it
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The pollutant being investigated, used in graph titles
#'
#' @param year The year being investigated, used in graph titles
#'
#' @keywords faceted, sources
#'
#' @export


facet_RUC_decile_emissions_ethnicity <- function(prawn_path,pollutant,year){
data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  mutate(`Minoritised white`=
           `Ethnic group: White: Irish`+
           `Ethnic group: White: Gypsy or Irish Traveller`+
           `Ethnic group: White: Roma`+
           `Ethnic group: White: Other White`
         ) %>% 


  dplyr::select(c(geography,
                  `geography code`,
                  `Ethnic group: Asian, Asian British or Asian Welsh`,
                  `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
                  `Ethnic group: Mixed or Multiple ethnic groups`,
                  `Ethnic group: White`,
                  `Ethnic group: Other ethnic group`,
                  `Minoritised white`,
                  `Ethnic group: Total: All usual residents`)) %>%
  rename(`Asian, Asian British or\nAsian Welsh`=
           `Ethnic group: Asian, Asian British or Asian Welsh`,
         `Black, Black British, \nBlack Welsh, Caribbean\nor African` =
         `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
         `Mixed or Multiple \nethnic groups`=
         `Ethnic group: Mixed or Multiple ethnic groups`,
         `White`=
         `Ethnic group: White`,
         `Other ethnic\ngroup`=
         `Ethnic group: Other ethnic group`
         ) %>%
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=c(
      `Asian, Asian British or\nAsian Welsh`,
      `Black, Black British, \nBlack Welsh, Caribbean\nor African`,
      `Mixed or Multiple \nethnic groups`,
      `White`,
      `Other ethnic\ngroup`,
      `Minoritised white`),
    names_to = "Ethnic group",
    values_to = "flat population"
  ) %>%
  #convert flat population into percentage
  mutate("Percentage"=`flat population`/`Ethnic group: Total: All usual residents`) %>%
    group_by(`Ethnic group`)

quantile_bounds <- edata %>% summarise(Decile=rep(c(1:10),by=5),
                                       `Lower bound`=quantile(Percentage,probs=seq(from=0,to=0.9, by=0.1)),
                                       `Upper bound`=quantile(Percentage,probs=seq(from=0.1,to=1, by=0.1))
                                       )

foray <- edata %>%
  mutate(Diversity_quintile = ntile(x=Percentage,
                                    n=10)) %>%
  group_by(`Ethnic group`,Diversity_quintile) 


plottable <- foray %>% inner_join(
  x=data %>% dplyr::select(RUC11,Total,LSOA21CD),
  y=foray,
  by=c("LSOA21CD"="geography code")
) %>% 
  dplyr::filter(RUC11!="mismatch") %>% 
  mutate("Rural urban classification"=case_when(
    RUC11=="Rural town and fringe in a sparse setting"~"Rural town and fringe",
    RUC11=="Rural village and dispersed in a sparse setting"~ "Rural village and dispersed",
    RUC11=="Urban city and town in a sparse setting"~"Urban city and town",
    .default = RUC11)) %>%
  group_by(`Ethnic group`,Diversity_quintile,`Rural urban classification`) %>%
  summarise(Mean=mean(Total))%>% 
  mutate(linetype=case_when(
    `Ethnic group`=="Minoritised white"~"dashed",
    .default="straight"
  ))

RUC_frequency <- foray %>% inner_join(
  x=data %>% dplyr::select(RUC11,Total,LSOA11CD),
  y=foray,
  by=c("LSOA11CD"="geography code")
) %>%
  mutate("Rural urban classification"=case_when(RUC11%in%c("Rural village and dispersed in a sparse setting",
                                                           "Rural town and fringe in a sparse setting",
                                                           "Urban city and town in a sparse setting")~"RUCs in a sparse setting",
                                                !RUC11%in%c("Rural village and dispersed in a sparse setting",
                                                            "Rural town and fringe in a sparse setting",
                                                            "Urban city and town in a sparse setting")~RUC11,
  )) %>%

  group_by(`Ethnic group`,`Rural urban classification`) %>%

  summarise(Total=sum(`flat population`))

ruc_freq_plot <- ggplot(data=RUC_frequency)+
  aes(y=Total,x=`Ethnic group`)+
  geom_bar(stat="identity")+
  facet_wrap(~`Rural urban classification`,scale="free_y")

output <- ggplot(data=plottable
)+

  aes(x=Diversity_quintile,
      y=Mean,
      colour=`Ethnic group`,
      linetype=linetype)+

  geom_line(stat="summary",
            linewidth=0.5,
            fun=mean,
            na.rm=TRUE
  )+


  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+

  labs(x=paste0("Decile where 10 contains the most people within the group"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       title=NULL
  )+
  theme(legend.position = "right",
        legend.key.width = unit(0.5,"cm"),
        legend.key.height = unit(1.3,"cm"))+

  guides(fill = guide_legend(byrow = TRUE))+

  facet_wrap(~fct_reorder(`Rural urban classification`,Mean,mean,na.rm=TRUE,.desc=TRUE))+
  scale_colour_manual(values=c("black","royalblue","deeppink2","olivedrab1","#FB8022FF","deeppink2"))+
  theme_classic()+
  scale_linetype_manual(values=c(5,1),)+
  guides(linetype="none",
         colour=guide_legend(override.aes = 
                               list(linetype=c(1,1,5,1,1,1)))
  )+
  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0.1),
    minor_breaks = FALSE)+
  scale_y_continuous(expand=expansion(mult=c(0,0.06),add=0), limits = c(0, NA))+
  labs(y=bquote("Average "~.(pollutant)~" emissions/ tonnes "~km^"-2"),
       x="Population decile where 10 contains the highest percentage population")

output

}
