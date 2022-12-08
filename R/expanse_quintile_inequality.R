#' A function for looking at the relationship between IMD and emissions for
#' different LSOA sizes
#'  populations
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The name of the pollutant that's being examined, this is used in the graph names
#'
#' @param year The year the dat is from, used for axis labels
#'
#' @keywords expanse
#' @export
#' @examples
#' expanse_probe()
#'

expanse_quintile_inequality <- function(prawn_path,pollutant,year){

active_stack <- read.csv(file=prawn_path,row.names=1,check.names=FALSE) %>% tibble() %>%

  rename("Other transport and \nmobile machinery"=`Other transport and mobile machinery`,
         `Waste treatment \nand disposal`=`Waste treatment and disposal`) %>%mutate(`Other sources`=Solvents+Natural+`Energy production`+`Waste treatment \nand disposal`+Agricultural) %>%

  pivot_longer(
    cols=c("Domestic combustion",
           "Industrial combustion","Industrial production",
           "Other transport and \nmobile machinery","Road transport","Total"
           ,"Point sources","Other sources"),
    names_to = "Emission_source",
    values_to = "emissions") %>% group_by(Emission_source)%>%
  #convert to km2
  mutate(expanse=expanse/10^6,face=ntile(expanse,5))

axticks <- tibble(hi=quantile(active_stack$expanse,probs=seq(0.2,1,0.2)) %>% signif(2) %>% as.numeric())
axticks <- axticks %>% mutate(lo=c(0,hi) %>%  head(-1),
                              key=c(1:5))
converted_stack <- inner_join(active_stack,axticks,by=c("face"="key")) %>%
  mutate(name=paste0(lo," to ",hi)) %>%
  group_by(name,IMD,Emission_source) %>%
  summarise(emissions=mean(emissions),face=face)

faces <- ggplot(data=converted_stack %>% filter(Emission_source=="Total"))+
  aes(x=IMD,y=emissions,colour=name)+
  geom_line(stat="summary")+
  scale_colour_viridis_d(option = "turbo",
                         name=bquote("LSOA area / "~km^2))+
  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0.1),
    minor_breaks = FALSE)+

  scale_y_continuous(expand=expansion(mult=c(0,0.05),add=c(0.5,0)))+

  labs(x=paste0("IMD decile where 10 is least deprived"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^2),
       title=NULL
  )+

  geom_point(aes(x=IMD,
                 y=emissions,
                 colour=name,
                 shape=as.factor(face)),
             stroke=1.5,
             size=1.5)+

  scale_shape_manual(values=c("1"=21,"2"=22,"3"=23,"4"=24,"5"=25),
                     name=bquote("LSOA area / "~km^2),
                     labels=unique(converted_stack$name))

  guides(colour=guide_legend(override.aes = list(size=2)))


faces
#sort the sizes into
}
