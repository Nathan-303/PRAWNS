#' Bind ethnicity data to the prawn and create graphs based on it
#'
#' @param prawn_path The filepath for the prawn CSV that is to be used.
#'
#' @param pollutant The pollutant being investigated, used in graph titles
#'
#' @param year The year being investigated, used in graph titles
#'
#' @keywords faceted, sources
#'
#' @export
#'
#' @examples
#' cartesian_ethnicity_groups_src(
#'   prawn_path="PRAWN.csv",
#'   pollutant="NOx",
#'   year=2019)

cartesian_ethnicity_continuous_src <- function(prawn_path,pollutant,year){
data <- read.csv(prawn_path,
                 row.names=1,
                 check.names=FALSE)

edata <- read.csv("Data/LSOA_statistics/census2021-ts021-lsoa.csv",
                  check.names=FALSE,
                  sep="|") %>%
  rename(`Asian, Asian British or\nAsian Welsh`=
           `Ethnic group: Asian, Asian British or Asian Welsh`,
         `Black, Black British, \nBlack Welsh, Caribbean\nor African` =
         `Ethnic group: Black, Black British, Black Welsh, Caribbean or African`,
         `Mixed or Multiple \nethnic groups`=
         `Ethnic group: Mixed or Multiple ethnic groups`,
         `White`=
         `Ethnic group: White`,
         `Other ethnic\ngroup`=
         `Ethnic group: Other ethnic group`
         ) %>%
  #Pivot the broadest subdivisions out
  pivot_longer(
    cols=c(
      `Asian, Asian British or\nAsian Welsh`,
      `Black, Black British, \nBlack Welsh, Caribbean\nor African`,
      `Mixed or Multiple \nethnic groups`,
      `White`,
      `Other ethnic\ngroup`),
    names_to = "Ethnic group",
    values_to = "flat population"
  ) %>%
  #convert flat population into percentage
  mutate("Percentage"=`flat population`/`Ethnic group: Total: All usual residents`) %>%
    group_by(`Ethnic group`)


plottable <- edata %>% inner_join(
  x=data,
  y=edata,
  by=c("LSOA11CD"="geography code")
) %>%
  group_by(`Ethnic group`)

pop_distribution <-   plottable %>%
  summarise(q90=quantile(Percentage,c(0.90)),
                   q10=quantile(Percentage,c(0.10)),
                   q1=quantile(Percentage,c(0.25)),
                   q3=quantile(Percentage,c(0.75)),
                   med=quantile(Percentage,c(0.5))) %>%
  pivot_longer(cols=c(q90,q10,q1,q3,med),values_to = "Percentagequantile")

#Plot a faceted graph

output <- ggplot(data=plottable
)+

  aes(x=Percentage,
      y=Total,
      colour=`Ethnic group`)+

  geom_smooth()+
  # geom_boxplot(data=boxxy,
  #              aes(x=Percentage,
  #                  group=decile,
  #                  y=Total),
  #              show.legend = FALSE,
  #              outlier.shape = NA)+

  geom_smooth(method="lm",
              formula=y~x,
              se=FALSE,
              linewidth=1,
              na.rm = TRUE)+


  scale_x_continuous(
    breaks=c(1:10),
    expand = expansion(mult=0,add=0),
    minor_breaks = FALSE)+

  labs(x=paste0("Percentage population"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       title=NULL
  )+
  theme(legend.position = "right",
        legend.key.width = unit(0.5,"cm"),
        legend.key.height = unit(1.3,"cm"))+

  geom_point(data=pop_distribution,
             aes(x=Percentagequantile),
             y=25)+

  scale_colour_viridis_d()+

  facet_wrap(~`Ethnic group`,scale="free_x")+

  guides(fill = guide_legend(byrow = TRUE))

output

}
