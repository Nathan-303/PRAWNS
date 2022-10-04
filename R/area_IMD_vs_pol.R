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

area_IMD_vs_pol <- function(prawn_path,pollutant,area_type){
  if(area_type=="County/UA"){
active_stack <- read.csv(prawn_path) %>% group_by(Area)
  }else{
    active_stack <- read.csv(prawn_path) %>% group_by(TCITY15NM)
}

area_rank <- active_stack %>% summarise(mean =mean(Total),
                                          median=median(Total),
                                          dep=mean(IMD)) %>%
  pivot_longer(c(mean, median), names_to = "stat",values_to = "token") %>% group_by(stat)


test <- ggplot(data=area_rank)+

  aes(x=dep,
      y=token,
      colour=stat)+

  geom_point()+

  geom_smooth(method="lm",
              formula=y~x,
              se=FALSE,
              show.legend=FALSE)+

  labs(x="Average deprivation rank",
       y=paste0(pollutant," levels"),
       title=paste0("Deprivation vs ",pollutant, "emission, grouped by ",area_type))


test


}
