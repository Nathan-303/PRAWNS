#' Bind ethnicity data to the prawn and create graphs based on it
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#' @param pollutant The pollutant being investigated, used in graph titles
#' @param year The year being investigated, used in graph titles
#' @keywords faceted, sources
#' @export
#' @examples
facet_ethnicity_groups_src <- function(prawn_path,pollutant,year){
data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=c(
      `Ethnic group: Asian, Asian British or Asian Welsh`,
      `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
      `Ethnic group: Mixed or Multiple ethnic groups`,
      `Ethnic group: White`,
      `Ethnic group: Other ethnic group`),
    names_to = "Broad group",
    values_to = "flat population"
  ) %>%
  #convert flat population into percentage
  mutate("Percentage"=`flat population`/`Ethnic group: Total: All usual residents`) %>%
    group_by(`Broad group`)

foray <- edata %>%
  mutate(Diversity_quintile = ntile(x=Percentage,
                                    n=5))

#Create a column for naming the facets
Diversity_bounds <- quantile(foray$Percentage,probs=seq(0,1,0.2)) %>% signif(2)*100
Diversity_brackets <- vector()
for(index in c(1:5)){
  Diversity_brackets[index] <- paste0(Diversity_bounds[index],
                                  "% to ",Diversity_bounds[index+1],"%")
}

Diversity_tibble <- tibble(Diversity_quintile=c(1,2,3,4,5),caption=Diversity_brackets)
plottable <- inner_join(
  x=data,
  y=foray,
  by=c("LSOA11CD"="geography code")
) %>%
#Bind on the captions
  inner_join(
    y=Diversity_tibble,
    by="Diversity_quintile"
  )

#Plot a faceted graph

output <- ggplot(data=plottable
)+

  aes(x=IMD,
      y=Total,
      colour=`Diversity_quintile`)+

  facet_wrap(~`Broad group`
  )+

  geom_line(stat="summary",
            aes(linetype="Mean"),
            linewidth=0.5,
            fun=mean,
            na.rm=TRUE
  )+


  geom_smooth(method="lm",
              formula=y~x,
              se=FALSE,
              aes(linetype="Mean"),
              linewidth=1,
              na.rm = TRUE)+

  #Plot the line of best fit for the median
  geom_quantile(quantiles=0.5,
                aes(linetype="Median"),
                formula=y~x,
                linewidth=1,
                na.rm=TRUE)+



  #Plot a line through the medians for each decile
  geom_line(stat="summary",
            fun=median,
            aes(linetype="Median"),
            linewidth=0.5,
            na.rm=TRUE)+

  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+


  scale_linetype_manual(name= "Line plotted:",
                        values = c("Mean"="solid","Median"="dashed"),
                        guide=guide_legend(override.aes = list(linetype=c("solid","dashed"),
                                                               colour="black",shape=c(NA,NA),
                                                               linewidth=c(1,1)))
  )+

  labs(x=paste0("IMD decile where 10 is least deprived"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       title=NULL
  )+
  theme(legend.position = "bottom",legend.key.width = unit(1.5,"cm"))+

  scale_colour_viridis_d()

output

}
