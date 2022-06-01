# 1.0.1 Functionality: county/UA distinction ------------------------------


#merge by the common variable, which in this case is the local authority district code
county_stat_core <- inner_join(stat_core,county_data,by=c("Local.Authority.District.code..2019."="LAD19CD")) %>%
  #Add a column that tracks the fact that these are counties
  mutate(area_type="County")

test <- bind_rows()

#Flag everything that isn't a county as a unitary authority
UA_stat_core <- filter(stat_core,)

amalgamation <- bind_rows(county_stat_core,UA_stat_core)

write.csv(amalgamation,"Data/LSOA11CD_compatible_amalgamation")
}
#Bind data that doesn't have inherent links to LSOA11CD to data that does. This data is the link between district area and county

