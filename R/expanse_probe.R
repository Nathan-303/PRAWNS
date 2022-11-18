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
  mutate(point_sources=Total-Total_no_points)%>%
  
  rename(`Other transport and \nmobile machinery`=`Other transport and mobile machinery`,
         `Waste treatment \nand disposal`=`Waste treatment and disposal`) %>%
  
  pivot_longer(
    cols=c("Agricultural","Domestic combustion","Energy production",
           "Industrial combustion","Industrial production","Natural",
           `Other transport and \nmobile machinery`,"Road transport","Solvents","Total"
           ,`Waste treatment \nand disposal`,"Point sources"),
    names_to = "Emission_source",
    values_to = "emissions") %>% group_by(Emission_source)%>% 
  #convert to km2
  mutate(expanse=expanse/10^6,face=ntile(expanse,12)) 

faces <- ggplot(data=active_stack)+
  aes(x=IMD,y=emissions,group=Emission_source)+
  geom_line(stat="summary",aes(linetype="Average points",colour="Mean"))+
  facet_wrap(~face,scale="free")


#sort the sizes into 
} 
