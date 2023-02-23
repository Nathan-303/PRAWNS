#' Make a nice graph
#'
#' @param prawn_path the filepath to the prawn csv file used for the graph
#' @param pollutant the pollutant looked at, used in graph titles
#' @param year the year of the measurement, used in axis labels
#' @export
#' @examples please no error message
emissions_by_expanse <- function(prawn_path,pollutant,year){
#read data

long_chunk <- read.csv(file=prawn_path,row.names=1,check.names=FALSE) %>% tibble() %>%

  rename(`Other transport and \nmobile machinery`=`Other transport and mobile machinery`,
         `Waste treatment \nand disposal`=`Waste treatment and disposal`,
  ) %>%
  #convert expanse to km2
  mutate(expanse=expanse/10^6,
         #split expanse into tiles
         extile=ntile(expanse,20)) %>%

  pivot_longer(
    cols=c("Agricultural","Domestic combustion","Energy production",
           "Industrial combustion","Industrial production","Natural",
           `Other transport and \nmobile machinery`,"Road transport","Solvents","Total"
           ,`Waste treatment \nand disposal`,"Point sources"),
    names_to = "Emission_source",
    values_to = "emissions")


quants <- tibble(exnum=quantile(long_chunk$expanse,probs=seq(0.05,1,0.05)),
                                 extile=c(1:20))

grouped_chunk <- long_chunk %>%  group_by(extile,Emission_source) %>% summarise(Emissions=mean(emissions)) %>% inner_join(quants,by="extile")

text_points <- grouped_chunk %>% ungroup() %>% filter(exnum>2) %>%
  #set the dummy values to be plotted in geom text
  mutate(dummy=base::rep(x=seq(2.2,3,0.2),each=12),
         #round the true x values to be labelled in geom text
         label=signif(exnum,2))

output <- ggplot()+

  aes(x=exnum,
      y=Emissions)+

  facet_wrap(~fct_reorder(Emission_source,Emissions,mean,.desc=TRUE),scale="free_y"
  )+
  #plot the normal data
  geom_point(data=grouped_chunk %>% filter(exnum<2))+
  #plot the data outside a nice x range
  geom_point(data=text_points,aes(x=dummy,y=Emissions),
             colour="#FB8022FF",
             shape=4,
             stroke=1.5
             )+
  # geom_text_repel(data=text_points,
  #           aes(x=dummy,
  #               y=Emissions,
  #               label=label),
  #           size=3,
  #           force_pull=2,
  #           force=2,
  #           max.time = 3,
  #           point.padding = 3)+

  scale_x_continuous(limits=c(0,3),
                     expand=expansion(c(0,0.05)),
                     breaks=c(0:3),
                     labels = c(0,1,2,"n.a."))+

  labs(x=bquote("LSOA area / "~km^2),
       y=bquote("Average "~.(pollutant)~"emissions/ tonnes "~km^"-2"))

output

}
