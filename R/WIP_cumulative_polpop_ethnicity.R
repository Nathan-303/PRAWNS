

data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE) %>% 
  dplyr::select(Total,IMD,LSOA11CD)

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  
  dplyr::select(`geography code`,
         `Ethnic group: Asian, Asian British or Asian Welsh`,
         `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
         `Ethnic group: Mixed or Multiple ethnic groups`,
         `Ethnic group: White`,
         `Ethnic group: Other ethnic group`) %>% 

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
      `Other ethnic\ngroup`),
    names_to = "Ethnic group",
    values_to = "flat population"
  ) %>%
  #convert flat population into percentage

  group_by(`Ethnic group`)


plottable <- edata %>% inner_join(
  x=data,
  y=edata,
  by=c("LSOA11CD"="geography code")
) %>%
  mutate("pollution_population"=`flat population`*Total) %>%
  group_by(`Ethnic group`,IMD)

poptot <- plottable %>% group_by(`Ethnic group`) %>% summarise(totpop=sum(`flat population`))

penultimate <- plottable  %>%
  group_by(`Ethnic group`,IMD) %>% 
  mutate(token=pollution_population)

final <- penultimate %>% summarise(normalised_number=sum(token))%>% 
  inner_join(poptot,by="Ethnic group") %>% 
  mutate(point=normalised_number/totpop) %>%
  group_by(`Ethnic group`) %>% 
  mutate(cumulative=cumsum(point))

output <- ggplot(data=final)+
  
  aes(x=IMD,
      y=cumulative,
      colour=`Ethnic group`)+
  
  geom_line()

output
