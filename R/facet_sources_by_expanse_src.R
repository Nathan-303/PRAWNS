#' Plot the relationship between emissions and LSOA expanse for each emissions
#' sector
#'
#' @param prawn_path the filepath to the prawn csv file used for the graph
#'
#' @param pollutant the pollutant looked at, used in graph titles
#'
#' @param year the year of the measurement, used in axis labels
#'
#' @export
#'
#' @examples
#' facet_sources_by_expanse_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019
#'   )

facet_sources_by_expanse_src <- function(prawn_path,pollutant,year){
#read data

long_chunk <- read.csv(file=prawn_path,row.names=1,check.names=FALSE) %>% tibble()

if("Other transport and mobile machinery" %in% colnames(long_chunk)){
  long_chunk <- long_chunk %>% rename(`Other transport and \nmobile machinery`=`Other transport and mobile machinery`)
}

if("Waste treatment and disposal" %in% colnames(long_chunk)){
  long_chunk <- long_chunk %>% rename(`Waste treatment \nand disposal`=`Waste treatment and disposal`)
}

end_of_sources <- which(colnames(long_chunk)=="LSOA11CD")-1

source_list <- colnames(long_chunk)[c(1:end_of_sources)]

  #convert expanse to km2
 long_chunk <- long_chunk %>%  mutate(expanse=expanse/10^6,
         #split expanse into tiles
         extile=ntile(expanse,20)) %>%

   pivot_longer(
     cols=all_of(c(source_list,"Point sources")),
     names_to = "Emission_source",
     values_to = "emissions") %>%
   group_by(Emission_source)


quants <- tibble(exnum=quantile(long_chunk$expanse,probs=seq(0.05,1,0.05)),
                                 extile=c(1:20))

grouped_chunk <- long_chunk %>%  group_by(extile,Emission_source) %>% summarise(Emissions=mean(emissions)) %>% inner_join(quants,by="extile")



output <- ggplot()+

  aes(x=exnum,
      y=Emissions)+

  facet_wrap(~fct_reorder(Emission_source,Emissions,mean,.desc=TRUE),scale="free_y"
  )+
  #plot the normal data
  geom_point(data=grouped_chunk %>% filter(exnum<2))+
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

#execute this code only if the number of rows is divisible by 5
if(ncol(grouped_chunk)%%5==0){

text_points <- grouped_chunk %>% ungroup() %>% filter(exnum>2)

text_points <- text_points %>%
  #set the dummy values to be plotted in geom text
  mutate(dummy=base::rep(x=seq(2.2,3,0.2),each=(nrow(text_points)/5)),
         #round the true x values to be labelled in geom text
         label=signif(exnum,2))
output <- ouptut+
  #plot the data outside a nice x range
  geom_point(data=text_points,aes(x=dummy,y=Emissions),
             colour="#FB8022FF",
             shape=4,
             stroke=1.5
  )
}

output

}
