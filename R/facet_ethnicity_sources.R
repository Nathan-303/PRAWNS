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
    values_to = "Emissions"
    
  ) %>%
  tibble()

long_stack <- long_stack %>% mutate(NOx_emissions=replace_na(NOx_emissions,0))

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=-c(date,geography,`geography code`),
    names_to = "Ethnic group",
    values_to = "flat_population"
  ) %>%
  
  group_by(`Ethnic group`) %>% 
  
  mutate(groupid=cur_group_id()) %>% 
  
  mutate(`Ethnic group`=str_sub(`Ethnic group`,start=14L))%>% mutate(`Ethnic group`=`Ethnic group` %>% str_replace_all(
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
    !`Ethnic group`%in%c("Black, Black British, Black Welsh, Caribbean or African",
                         "White: English, Welsh, Scottish, Northern Irish or British",
                         "Asian, Asian British or Asian Welsh",
                         "Mixed or Multiple ethnic groups",
                         "Other Mixed or Multiple ethnic groups"
    )~`Ethnic group`
  ))


plottable <- inner_join(long_stack,edata,by=c("LSOA11CD"="geography code")) %>%
  dplyr::select(LSOA11CD,`Ethnic group`,Emission_source,IMD,)
  group_by(`Ethnic group`,IMD,Emission_source,Emissions)
