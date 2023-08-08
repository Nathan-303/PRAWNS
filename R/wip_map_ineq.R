shapefile_path <-  "Data/2011_LSOA_shapefile_20m_generalised"
prawn_path <- "NOx_emissions_in_2019_v0.13.3/PRAWN.csv"
library(nngeo)
library(RColorBrewer)

data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  rename(`Asian, Asian British or Asian Welsh`=
           `Ethnic group: Asian, Asian British or Asian Welsh`,
         `Black, Black British, Black Welsh, Caribbean or African` =
           `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
         `Mixed or Multiple ethnic groups`=
           `Ethnic group: Mixed or Multiple ethnic groups`,
         `White`=
           `Ethnic group: White`,
         `Other ethnic group`=
           `Ethnic group: Other ethnic group`
  ) %>%
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=c(
      `Asian, Asian British or Asian Welsh`,
      `Black, Black British, Black Welsh, Caribbean or African`,
      `Mixed or Multiple ethnic groups`,
      `White`,
      `Other ethnic group`),
    names_to = "Ethnic group",
    values_to = "flat_population"
  )


intermediate <- inner_join(data,edata,by=c("LSOA11CD"="geography code"))%>%

  mutate(`Weighted emissions`= Total*flat_population,
         `Weighted deprivation`=IMD*flat_population)


weighted_data <- intermediate %>%

  group_by(LAD19NM,`Ethnic group`) %>%

  summarise(popsum=sum(flat_population),
            emissions_sum=sum(`Weighted emissions`),
            IMD_sum=sum(`Weighted deprivation`)) %>%

  mutate(`Weighted emissions`=emissions_sum/popsum,
         `Weighted deprivation`=IMD_sum/popsum) %>%

  group_by(`Ethnic group`) %>%

  mutate(tile =dplyr::ntile(x=`Weighted deprivation`,
                            n=10))

wide_table <- weighted_data %>% pivot_wider(
  id_cols = LAD19NM,
  names_from = `Ethnic group`,
  values_from = c(`Weighted emissions`,
                  `Weighted deprivation`)
)


#the error is here
delta_table <- wide_table %>% mutate(
  #calculate the differnce in deprivation
  as_dep=`Weighted deprivation_Asian, Asian British or Asian Welsh`-
    `Weighted deprivation_White`,
  bl_dep=`Weighted deprivation_Black, Black British, Black Welsh, Caribbean or African`-
    `Weighted deprivation_White`,
  mx_dep=`Weighted deprivation_Mixed or Multiple ethnic groups`-
    `Weighted deprivation_White`,
  other_dep=`Weighted deprivation_Other ethnic group`-
    `Weighted deprivation_White`,

  #calculate the differnce in emissions
  as_ems=`Weighted emissions_Asian, Asian British or Asian Welsh`-
    `Weighted emissions_White`,
  bl_ems=`Weighted emissions_Black, Black British, Black Welsh, Caribbean or African`-
    `Weighted emissions_White`,
  mx_ems=`Weighted emissions_Mixed or Multiple ethnic groups`-
    `Weighted emissions_White`,
  other_ems=`Weighted emissions_Other ethnic group`-
    `Weighted emissions_White`
)

penultimate_pivot <- delta_table %>% pivot_longer(
  cols = c(as_dep,bl_dep,mx_dep,other_dep,
           as_ems,bl_ems,mx_ems,other_ems),
  names_to=c("Ethnicity","Measure"),
  names_sep = "_",
  names_repair = "unique"
)

final_pivot <- penultimate_pivot %>%
  pivot_wider(names_from=Measure,
              values_from = value)

converged_map <- inner_join(final_pivot,
                            data %>% dplyr::select(LAD19NM,LSOA11CD),
                            by="LAD19NM")

raw_shapefile <- st_read(shapefile_path)

stitched_shapefile <- inner_join(converged_map,raw_shapefile, by="LSOA11CD") %>%
  mutate(ems2=case_when(ems>15~15,
                        -5>ems~-5,
                        15>ems & ems>-5~ems
                        )) %>% 
  group_by(LAD19NM,Ethnicity) %>% 
  summarise(geometry = sf::st_union(geometry),ems=mean(ems),ems2=mean(ems2))%>% 
mutate(geometry=geometry %>% st_remove_holes())


#tidy up to free ram, the next step hungers
rm(data,
     edata,
     intermediate,
     weighted_data,
     wide_table,
     delta_table,
     penultimate_pivot,
     final_pivot,
     converged_map,
     raw_shapefile)
#set the values where the gradient should be
colours <- brewer.pal(n=4,name="RdBu")
colour_gradient <- rescale(c(-5,0,5,14.5,15),to=c(0,1))
testing <- ggplot(data=stitched_shapefile)+

  geom_sf(aes(fill = ems2,
              geometry=geometry),
          color = "black",
          size=0.1) +

  #scale_fill_distiller(palette="RdBu",values = colour_gradient)+
  scale_fill_gradientn(colours = colours,values = colour_gradient) +

  facet_wrap(~Ethnicity)

testing
"#CA0020" "#F4A582" "white" "#92C5DE" "#0571B0"
