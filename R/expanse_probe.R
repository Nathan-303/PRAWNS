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

expanse_probe <- function(prawn_path,pollutant,year){

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
  mutate(expanse=expanse/10^6,face=ntile(expanse,12))

axticks <- tibble(hi=quantile(active_stack$expanse,probs=seq(0.08333,1,0.08333)) %>% signif(2) %>% as.numeric())
axticks <- axticks %>% mutate(lo=c(0,hi) %>%  head(-1),
                              key=c(1:12))
converted_stack <- inner_join(active_stack,axticks,by=c("face"="key")) %>%
  mutate(name=paste0("LSOAs ranging from ",lo," to ",hi," km^2")) %>%
  group_by(name,IMD,Emission_source) %>%
  summarise(emissions=mean(emissions),face=face)
faces <- ggplot(data=converted_stack)+
  aes(x=IMD,y=emissions,colour=fct_reorder(Emission_source,emissions,mean,na.rm=TRUE,.desc=TRUE))+
  geom_line(stat="summary")+
  facet_wrap(~fct_reorder(name,face))+
  scale_colour_viridis_d(option = "turbo",name="Emission source")+
  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+
  scale_y_continuous(expand=expansion(mult=c(0,0.05),add=c(0.5,0)))+
  labs(x=paste0("IMD decile where 10 is least deprived"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^2),
       title=NULL
  )+
  guides(colour=guide_legend(override.aes = list(size=2)))


trimmed_stack <- active_stack %>% group_by(IMD) %>% mutate(bound=quantile(expanse,probs=0.9)) %>%
  filter(expanse<=bound&Emission_source=="Total") %>% mutate(IMDtext=paste0("IMD decile: ", IMD))
LSOA_sizes <- ggplot(data=trimmed_stack)+
  aes(x=expanse)+
  geom_histogram(bins=120,boundary=0)+
  facet_wrap(~fct_reorder(IMDtext,IMD),scale="free")+
  scale_x_continuous(expand=expansion(0,0))+

  labs(x=bquote("LSOA area / "~km^2),
       y="Frequency",
       title=NULL
  )

output <- list(faces,LSOA_sizes)

output
#sort the sizes into
}
