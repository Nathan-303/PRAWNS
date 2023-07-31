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

cartesian_ethnicity_groups_src <- function(prawn_path,pollutant,year){
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
  mutate("Percentage"=`flat population`/`Ethnic group: Total: All usual residents`*100) %>%
    group_by(`Ethnic group`)

foray <- edata %>%
  mutate(Diversity_quintile = ntile(x=Percentage,
                                    n=10)) %>%
  group_by(`Ethnic group`,Diversity_quintile)


plottable <- edata %>% inner_join(
  x=data,
  y=foray,
  by=c("LSOA11CD"="geography code")
) %>%group_by(`Ethnic group`) %>%
mutate(tile=ntile(Percentage,8))

boxrigger <- plottable %>% group_by(`Ethnic group`,tile) %>% summarise()
boxxy <- plottable %>% group_by(`Ethnic group`,tile) %>% summarise(high=quantile(Total,c(0.95)),
                                                                   low=quantile(Total,c(0.05)),
                                                                   maxper=max(Percentage),
                                                                   minper=min(Percentage)) %>%
  mutate(center=(maxper+minper)/2) %>% filter(!tile%in%c(7,8))

rogue_boxxy <- boxxy %>% mutate
#select the data that falls in range for a sensible scale
main_data <- plottable %>% dplyr::filter(!tile%in%c(7,8),!`Ethnic group`=="White") %>%
  rbind(plottable %>% dplyr::filter(!tile%in%c(1,2),`Ethnic group`=="White"))

#create a tibble with the dummy x values
xpositions <- tibble(
  #set up the frame ready for entries
  `Ethnic group`=rep(unique(transforming_data$`Ethnic group`),times=c(2,2,2,2,2)),
  tile=c(1,2,rep(c(7,8),times=4)),
  #manually enter the new breaks
  dummy=c(75,80,#white
          3.5,4,#mixed or multiple
          2.5,3,#black
          1.75,2,#other
          7,8#Asian
          )
  )
hmm2 <- transforming_data %>% inner_join(xpositions,join_by(`Ethnic group`))

output <- ggplot(data=plottable)+
  geom_errorbar(data=boxxy,
                aes(x=center,
                    ymin=low,
                    ymax=high))+
#plot the data that works nicely on a scale
  geom_boxplot(data=main_data,
               inherit.aes=FALSE,
               aes(x=Percentage,
                   y=Total,
                   group=tile),
               outlier.shape = NA,
               coef=0)+

  geom_point(data=xpositions,
             aes(x=dummy,
                 y=20))+

  labs(x=paste0("Percentage of the LSOA population\nidentifying within that ethnic group"),
       y=bquote("Average "~.(pollutant)~"emissions in "~.(year)~"/ tonnes "~km^"-2"),
       title=NULL
  )+
  scale_colour_viridis_d()+

  guides(fill = guide_legend(byrow = TRUE))+

  facet_wrap(~`Ethnic group`,scale="free_x")+

  coord_cartesian(ylim=c(0,40))

output

}
