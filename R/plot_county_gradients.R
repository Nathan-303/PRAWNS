raw <- read.csv(prawn_path,
                row.names=1,
                check.names=FALSE)

refined <- raw %>% dplyr::select(LSOA11CD,IMD,Total,Area) %>% group_by(Area)


coeffs <- refined %>%
  #get the rsquared
  do(tidy(lm(Total~IMD, data=.)))

graphable <- inner_join(models,coeffs %>% filter(term=="IMD"),by="Area")

curiosity <- graphable %>% filter(p.value)