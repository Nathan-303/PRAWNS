#' Plot the relationship between IMD and emissions, grouped by LSOA size quintiles
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The name of the pollutant that's being examined, this is
#' used in the graph names
#'
#' @param year The year the dat is from, used for axis labels
#'
#' @keywords expanse
#' @export
#' @examples
#' expanse_probe()
#'

cartesian_expanse_quintile_src <- function(prawn_path,pollutant,year){

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
  geom_line(stat="summary",linewidth=1)+

  scale_colour_manual(values=c("black","royalblue","olivedrab1","#FB8022FF","deeppink2"),
                         name=bquote("LSOA area / "~km^2))+
  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0.1),
    minor_breaks = FALSE)+

  scale_y_continuous(expand=expansion(mult=c(0,0.05),add=c(0.5,0)))+

  labs(x=paste0("IMD decile where 10 is least deprived"),
       y=bquote(.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       title=NULL
  )+

  guides(colour=guide_legend(override.aes = list(linewidth=2)))+

  theme_classic()


faces
#sort the sizes into
}
