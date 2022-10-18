#'Plots the average IMD vs the average NOx for geographical areas (cities or
#'county/UA)
#'
#'@param prawn_path The filepath for the csv to work from
#'
#'@param pollutant The pollutant name, used in graph annotations
#'
#'@param area_type can be "City" or "County/UA"
#'
#'@export

area_IMD_vs_pol <- function(prawn_path,pollutant,area_type,year){
  if(area_type=="County/UA"){
active_stack <- read.csv(prawn_path) %>% group_by(Area)
  }else{
    active_stack <- read.csv(prawn_path) %>% group_by(TCITY15NM)
}

area_rank <- active_stack %>% summarise(Mean =mean(Total),
                                          Median=median(Total),
                                          dep=mean(IMD)) %>%
  pivot_longer(c(Mean, Median), names_to = "Average used for NOx",values_to = "token") %>% group_by(`Average used for NOx`)


test <- ggplot(data=area_rank)+

  aes(x=dep,
      y=token,
      shape=`Average used for NOx`,
      linetype=`Average used for NOx`)+

  geom_point()+

  geom_smooth(method="lm",
              formula=y~x,
              se=FALSE,)+

  scale_linetype_manual(name="Linear regression",values=c(1,2))+

  scale_shape_manual(name="Average used for NOx",
                     values =c(16,4))+


  theme(legend.key.size = unit(2,"lines"))+

  labs(x="Mean deprivation decile",
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^2),
       title=paste0("Deprivation vs ",pollutant, " emission, grouped by ",area_type))


test

model <- area_rank %>%  do(tidy(lm(token~dep, data=.)))

output <- list(test,model)

output

}
