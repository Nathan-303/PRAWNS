#' Bind ethnicity data to the prawn and create graphs based on it
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#' @param pollutant
#' @keywords faceted, sources
#' @export
#' @examples

data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") #%>% 
  # #Pivot the broadest subdivisions out
  # pivot_longer(
  #   cols=c(
  #     `Ethnic group: Asian, Asian British or Asian Welsh`,
  #     `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
  #     `Ethnic group: Mixed or Multiple ethnic groups`,
  #     `Ethnic group: White`,
  #     `Ethnic group: Other ethnic group`),
  #   names_to = "Broad group",
  #   values_to = "flat population"
  # ) %>% 
  # #convert flat population into percentage
  # mutate("Percentage"=`flat population`/`Ethnic group: Total: All usual residents`)

foray <- edata %>% 
  mutate(homogeneity=
           `Ethnic group: White`/
           `Ethnic group: Total: All usual residents`) %>% 
  mutate(Diversity_quintile = ntile(x=homogeneity,
                                    n=5))

plottable <- inner_join(
  x=data,
  y=foray,
  by=c("LSOA11CD"="geography code")
)

#Plot a faceted graph

City_profile <- ggplot(data=plottable)+
  aes(x=IMD,
      y=Total)+
  
  scale_x_continuous(
    breaks=c(1:10),
    minor_breaks = FALSE,
    expand = expansion(mult=0,add=0))+
  
  #Plot the line of best fit for the mean
  geom_smooth(method="lm",
              formula=y~x,
              se=FALSE,
              show.legend=FALSE,
              aes(color='Mean'))+
  
  #Plot a line passing through the mean at each decile
  geom_line(stat="summary",
            aes(color='Mean'),
            fun=mean)+
  
  #Plot the line of best fit for the median
  geom_quantile(quantiles=0.5,
                aes(color='Median'),
                size =1,
                formula=y~x,)+
  
  #Plot a line through the medians for each decile
  geom_line(stat="summary",
            fun=median,
            aes(color='Median'))+
  
  #Plot a regression line for the whole UK for comparison
  geom_smooth(data=read.csv(prawn_path) %>% tibble %>% dplyr::select(-c(TCITY15NM,Area))
              , aes(
                x=IMD,
                y=Total,
                color='UK mean'),
              method="lm",
              formula=y~x,
              se=FALSE)+
  
  scale_colour_manual(name="Average used",
                      values = c("Median"="royalblue","Mean"="#FB8022FF",'UK mean'='black'))+
  
  labs(x="IMD decile where 10 is least deprived",
       y=bquote("Average "~.(pollutant)~"emissions/ tonnes "~km^"-2"))+
  
  theme(legend.position = "bottom")+
  
  facet_wrap(~Diversity_quintile)

