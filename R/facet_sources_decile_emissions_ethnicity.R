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
#'
#' @examples
#' cartesian_ethnicity_groups_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019)

facet_sources_decile_emissions_ethnicity <- function(prawn_path,pollutant,year){
data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  rename(`Asian, Asian British or\nAsian Welsh`=
           `Ethnic group: Asian, Asian British or Asian Welsh`,
         `Black, Black British, \nBlack Welsh, Caribbean\nor African` =
         `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
         `Mixed or Multiple \nethnic groups`=
         `Ethnic group: Mixed or Multiple ethnic groups`,
         `White`=
           `Ethnic group: White: English, Welsh, Scottish, Northern Irish or British`,
         `Other ethnic\ngroup`=
         `Ethnic group: Other ethnic group`
         ) %>% 
  mutate(`Minoritised white`=
                    `Ethnic group: White: Irish`+
                    `Ethnic group: White: Gypsy or Irish Traveller`+
                    `Ethnic group: White: Roma`+
                    `Ethnic group: White: Other White`
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

end_of_sources <- which(colnames(data)=="LSOA11CD")-1

source_list <- colnames(data)[c(1:end_of_sources)]

source_list_trimmer <- !(source_list %in% c("Offshore","Tot_area"))

source_list <- source_list[source_list_trimmer]

long_chunk <- data %>%
  mutate(
    "Industry and\npoint sources"=`Energy production`+`Industrial combustion`+`Industrial production`+`Point sources`,
    "Other sources"=Solvents+Natural+Agricultural+`Waste treatment and disposal`) %>%
  rename("Other transport and \nmobile machinery"="Other transport and mobile machinery") %>% 
  dplyr::select(!c(`Energy production`,
             `Industrial combustion`,
             `Industrial production`,
             `Point sources`,
             Solvents,
             Natural,
             Agricultural,
             `Waste treatment and disposal`)) %>% 
  pivot_longer(
    cols=c("Domestic combustion",
                "Other transport and \nmobile machinery",
                "Road transport",
                "Total",
                "Other sources",
                "Industry and\npoint sources"
    ),
    names_to = "Emission_source",
    values_to = "emissions")
#Plot a faceted graph

foray <- edata %>%
  mutate(Diversity_quintile = ntile(x=Percentage,
                                    n=10)) %>%
  group_by(`Ethnic group`,Diversity_quintile)


plottable <- foray %>% inner_join(
  x=long_chunk,
  y=foray,
  by=c("LSOA21CD"="geography code")
) %>%
  filter(!Emission_source%in%c("Waste treatment and disposal",
                               "Energy production",
                               "Natural",
                               "Solvents",
                               "Agricultural")) %>%
  group_by(`Ethnic group`,Diversity_quintile,Emission_source) %>%
  summarise(Mean=mean(emissions))%>% 
  mutate(linetype=case_when(
    `Ethnic group`=="Minoritised white"~"dashed",
    .default="straight"
  ))




output <- ggplot(data=plottable
)+

  aes(x=Diversity_quintile,
      y=Mean,
      colour=`Ethnic group`,linetype=linetype)+

  geom_line(stat="summary",
            linewidth=0.75,
            fun=mean,
            na.rm=TRUE
  )+


  # geom_smooth(method="lm",
  #             formula=y~x,
  #             se=FALSE,
  #             linewidth=1,
  #             na.rm = TRUE)+


  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+
  
  scale_y_continuous(expand=expansion(mult=c(0,0.06),add=0), limits = c(0, NA))+

  labs(x=paste0("Decile where 10 contains the most people within the group"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       title=NULL
  )+
  theme(legend.position = "right",
        legend.key.width = unit(0.5,"cm"),
        legend.key.height = unit(1.3,"cm"))+
  scale_linetype_manual(values=c(5,1),)+

  scale_colour_manual(values =c("black","royalblue","deeppink2","olivedrab1","#FB8022FF","deeppink2"))+

  guides(fill = guide_legend(byrow = TRUE,
                             ),
         linetype="none",
         colour=guide_legend(override.aes = 
                               list(linetype=c(1,1,5,1,1,1))))+

  facet_wrap(~fct_reorder(Emission_source,Mean,mean,na.rm=TRUE,.desc=TRUE),
             scale="free_y")+
  theme_classic()

output

}
