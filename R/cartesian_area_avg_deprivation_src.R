#'Plots the average IMD vs the average NOx for geographical areas (cities or
#'counties/unitary authorities)
#'
#' @param prawn_path The filepath for the csv to work from
#'
#' @param pollutant The pollutant name, used in graph annotations
#'
#' @param year The year of the data, used in graph annotations
#'
#' @param area_type The type of area investigated, can be "City" or "County/UA"
#'
#' @export
#'
#' @examples
#'   cartesian_area_avg_deprivation_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   area_type="City",
#'   year=2019)

cartesian_area_avg_deprivation_src <- function(prawn_path,pollutant,area_type,year){
  if(area_type=="County/UA"){
active_stack <- read.csv(prawn_path) %>% group_by(Area)
  }else{
    active_stack <- read.csv(prawn_path) %>% group_by(TCITY15NM)
}

area_rank <- active_stack %>% summarise(Mean =mean(Total),
                                          Median=median(Total),
                                          dep=mean(IMD)) %>%
  pivot_longer(c(Mean, Median), names_to = paste0(pollutant,"_average"),values_to = "token") %>%
  group_by(NOx_average) %>%
  mutate(tile=ntile(dep,8))


test <- ggplot(data=area_rank)+

  aes(x=dep,
      y=token)+

geom_boxplot(data=area_rank %>% filter(NOx_average=="Median"),
             aes(x=dep,
                 group=tile,
                 y=token),
             show.legend = FALSE,
             outlier.shape = NA)+

  geom_boxplot(data=area_rank %>% filter(NOx_average=="Mean"),
               aes(x=dep,
                   group=tile,
                   y=token),
               show.legend = FALSE,
               outlier.shape = NA)+


  geom_point(data=area_rank,
             colour="#FB8022FF",
             shape="cross",
             size=1.2,
             stroke=1)+
  #reimpose the lines on top of the points, its janky but necessary
  geom_boxplot(data=area_rank %>% filter(NOx_average=="Mean"),
               aes(x=dep,
                   group=tile,
                   y=token),
               show.legend = FALSE,
               alpha=0,
               outlier.shape = NA)+
  geom_boxplot(data=area_rank %>% filter(NOx_average=="Median"),
               aes(x=dep,
                   group=tile,
                   y=token),
               show.legend = FALSE,
               alpha=0,
               outlier.shape = NA)+
  geom_smooth(
    method="lm",
    formula=y~x,
    se=FALSE)+

  theme(legend.key.size = unit(2,"lines"))+

  labs(x="Mean deprivation decile",
       y=bquote(.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       title=NULL)+
  #tim the axis and add in the outlier cropped
  coord_cartesian(ylim=c(0,35),xlim=c(2.3,9.3),expand=FALSE)+

  geom_point(data=area_rank %>%
               dplyr::filter(token>35) %>%
               mutate(token=34),
             colour="deeppink2",
             shape=1,
             size=1.1,
             stroke=0.9)+

  facet_wrap(~NOx_average)


test

model <- area_rank %>%  do(glance(lm(token~dep, data=.)))

output <- list(test,model)

output

}
