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

facet_ethnicity_population_emissions_source <- function(prawn_path,pollutant,year){
active_stack <- read.csv(file=prawn_path,
                         check.names = FALSE,
                         row.names = 1)%>%


  #mutate in the columns you want (removing natural NOx)
  mutate(
    "Industry and\npoint sources"=`Energy production`+`Industrial combustion`+`Industrial production`+`Point sources`,
    "Other sources"=Solvents+Natural+Agricultural+`Waste treatment and disposal`) %>%
  tibble()

#Make the data long to eneble grouping by source
long_stack <- active_stack %>% rename(`Other transport and \nmobile machinery`=`Other transport and mobile machinery`) %>%
  pivot_longer(
    cols=c("Domestic combustion",
           "Other transport and \nmobile machinery",
           "Road transport",
           "Total",
           "Other sources",
           "Industry and\npoint sources"
    ),
    names_to = "Emission_source",
    values_to = "Emissions"

  ) %>%
  mutate(Emissions=replace_na(Emissions,0))

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  mutate(`Ethnic group: Minoritised white`=
                                       `Ethnic group: White: Irish`+
                                       `Ethnic group: White: Gypsy or Irish Traveller`+
                                       `Ethnic group: White: Roma`+
                                       `Ethnic group: White: Other White`
                  ) %>% 
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=-c(date,geography,`geography code`,`Ethnic group: Total: All usual residents`),
    names_to = "Ethnic group",
    values_to = "flat_population"
  ) %>%

  group_by(`Ethnic group`) %>%

  mutate(percentage=`flat_population`/`Ethnic group: Total: All usual residents`) %>%

  mutate(`Ethnic group`=str_sub(`Ethnic group`,start=14L))%>% mutate(`Ethnic group`=`Ethnic group` 
                                                                     %>% str_replace_all(
    c("Asian, Asian British or Asian Welsh: "="",
      "Black, Black British, Black Welsh, Caribbean or African: "="",
      "Mixed or Multiple ethnic groups: "="",
      "Other ethnic group:"="")
  )
  #close mutate
  ) %>%
  mutate(`Ethnic group`=str_trim(`Ethnic group`,"left")) %>%

  mutate(`Ethnic group`=case_when(
    `Ethnic group`=="Black, Black British, Black Welsh, Caribbean or African"~"Black, Black British, Black\nWelsh, Caribbean or African",
    `Ethnic group`=="White: English, Welsh, Scottish, Northern Irish or British"~"White: English, Welsh, Scottish,\nNorthern Irish or British",
    `Ethnic group`=="Asian, Asian British or Asian Welsh"~"Asian, Asian British\nor Asian Welsh",
    `Ethnic group`=="Mixed or Multiple ethnic groups"~"Mixed or Multiple\nethnic groups",
    `Ethnic group`=="Other Mixed or Multiple ethnic groups"~"Other Mixed or\nMultiple ethnic groups",
    `Ethnic group`=="Minoritised white"~"Minoritised white",
    !`Ethnic group`%in%c("Black, Black British, Black Welsh, Caribbean or African",
                         "White: English, Welsh, Scottish, Northern Irish or British",
                         "Asian, Asian British or Asian Welsh",
                         "Mixed or Multiple ethnic groups",
                         "Minoritised white",
                         "Other Mixed or Multiple ethnic groups"
    )~`Ethnic group`
  ))%>%
  group_by(`Ethnic group`) %>% 
  mutate(decile=ntile(x=percentage,
                      n=10))


weightchunk <- inner_join(long_stack,edata,by=c("LSOA21CD"="geography code")) %>%
  dplyr::select(LSOA11CD,`Ethnic group`,Emission_source,IMD,Emissions,flat_population,decile) %>%
  group_by(`Ethnic group`,decile,Emission_source) %>%
  mutate(weighted=Emissions*flat_population)

plottable <- weightchunk %>%
  summarise(emsum=sum(weighted),popsum=sum(flat_population),decile=decile) %>%
  mutate(avgems=emsum/popsum)%>% 
  dplyr::filter(`Ethnic group`%in%c("Black, Black British, Black\nWelsh, Caribbean or African",
                                    "White: English, Welsh, Scottish,\nNorthern Irish or British",
                                    "Asian, Asian British\nor Asian Welsh",
                                    "Mixed or Multiple\nethnic groups",
                                    "Other Mixed or\nMultiple ethnic groups",
                                    "Minoritised white"))
output <- ggplot(data=plottable)+
  aes(x=decile,y=avgems,colour=`Emission_source`)+
  geom_line()+
  facet_wrap(~fct_reorder(`Ethnic group`,avgems,.desc=TRUE),scale="free_y")+
  scale_colour_manual(values=c("black","royalblue","olivedrab1","#FB8022FF","deeppink2","goldenrod"))+
  theme_classic()+
  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+
  scale_y_continuous(expand=expansion(mult=c(0,0.06),add=0), limits = c(0, NA))+
  labs(y=bquote("Average "~.(pollutant)~" emissions/ tonnes "~km^"-2"),
            x="Population decile")

output
}
